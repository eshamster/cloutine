(defpackage cloutine/sync/channel
  (:use :cl)
  (:export :make-channel
           :close-channel
           :<-chan
           :chan<-)
  (:import-from :cloutine/queue
                :init-queue
                :queue
                :dequeue
                :queue-length)
  (:import-from :bordeaux-threads
                :make-lock
                :with-lock-held
                :make-condition-variable
                :condition-wait
                :condition-notify))
(in-package :cloutine/sync/channel)

;; LIMITATION:
;; The channel correctly works only when condition-notify notifies to only one waiter.
;; (In some implementations, it notifies to all waiter.)

(defclass channel ()
  ((queue :initform (init-queue) :reader ch-queue)
   (wait-cond :initform (make-condition-variable :name "WAIT COND") :reader ch-wait-cond)
   (notify-cond :initform (make-condition-variable :name "SIG COND") :reader ch-notify-cond)
   (lock :initform (make-lock) :accessor ch-lock)
   (wait-count :initform 0 :accessor ch-wait-count)
   (notify-count :initform 0 :accessor ch-notify-count)
   (max-length :initarg :max-resource :reader ch-max-resource) ; param
   (closed-p :initform nil :accessor ch-closed-p)))

(defun make-channel (&optional max-resource)
  "Make channel.
If max-resource is nil, there is no queue limit."
  (make-instance 'channel :max-resource max-resource))

(defmethod close-channel ((ch channel))
  "Close channel and broadcast signal to all waiting readers and writers."
  (let ((lock (ch-lock ch)))
    (with-lock-held (lock)
      (setf (ch-closed-p ch) t)
      (dotimes (i (ch-wait-count ch))
        (condition-notify (ch-wait-cond ch)))
      (dotimes (i (ch-notify-count ch))
        (condition-notify (ch-notify-count ch))))))

(defmethod <-chan ((ch channel))
  "Read from channel."
  (let ((lock (ch-lock ch))
        (q (ch-queue ch)))
    (with-lock-held (lock)
      (flet ((return-if-closed ()
               (when (ch-closed-p ch)
                 (return-from <-chan (values nil nil)))))
        (return-if-closed)
        (unless (> (queue-length q) 0)
          (incf (ch-wait-count ch))
          ;; wait until some value is queued
          (condition-wait (ch-wait-cond ch) lock)
          (decf (ch-wait-count ch))
          (return-if-closed))
        (assert (> (queue-length q) 0))
        (let ((res (dequeue q)))
          (when (> (ch-notify-count ch) 0)
            (condition-notify (ch-notify-cond ch)))
          (values res t))))))

(defmethod chan<- ((ch channel) value)
  "Write to channel"
  (let ((lock (ch-lock ch))
        (q (ch-queue ch))
        (max-length (ch-max-resource ch)))
    (with-lock-held (lock)
      (flet ((check-closed-p ()
               (when (ch-closed-p ch)
                 (error "Error: Insert a value into a closed channel")))
             (enable-queue-p ()
               (or (null max-length)
                   (< (queue-length q) max-length))))
        (check-closed-p)
        (unless (enable-queue-p)
          (incf (ch-notify-count ch))
          ;; wait until some value is dequeued
          (condition-wait (ch-notify-cond ch) lock)
          (decf (ch-notify-count ch))
          (check-closed-p))
        (assert (enable-queue-p))
        (queue q value)
        (when (> (ch-wait-count ch) 0)
          (condition-notify (ch-wait-cond ch)))))))



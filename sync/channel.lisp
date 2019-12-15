(defpackage cloutine/sync/channel
  (:use :cl)
  (:export :make-channel
           :close-channel
           :<-chan
           :chan<-)
  (:import-from :cloutine/cloutine
                :clt
                :init-cloutine
                :destroy-cloutine)
  (:import-from :cloutine/queue
                :init-queue
                :queue
                :dequeue
                :queue-length)
  (:import-from :blackbird
                :attach
                :with-promise)
  (:import-from :bordeaux-threads
                :make-lock
                :acquire-lock
                :release-lock
                :with-lock-held)
  (:import-from :cl-cont
                :defun/cc
                :let/cc))
(in-package :cloutine/sync/channel)

(defclass channel ()
  ((queue :initform (init-queue) :reader ch-queue)
   (queue-resolver-queue :initform (init-queue) :reader ch-queue-resolvers)
   (deqeueue-resolver-queue :initform (init-queue) :reader ch-dequeue-resolvers)
   (lock :initform (make-lock) :accessor ch-lock)
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
      (dotimes (i (queue-length (ch-queue-resolvers ch)))
        (funcall (dequeue (ch-queue-resolvers ch)) nil))
      (dotimes (i (queue-length (ch-dequeue-resolvers ch)))
        (funcall (dequeue (ch-dequeue-resolvers ch)) nil nil)))))

(defmacro with-release-lock ((lock) &body body)
  `(unwind-protect
        (progn ,@body)
     (release-lock ,lock)))

(defun/cc <-chan (ch)
  "Read from channel."
  (let ((lock (ch-lock ch))
        (q (ch-queue ch)))
    (acquire-lock lock)
    (cond ((ch-closed-p ch)
           (with-release-lock (lock)
             (release-lock lock)))
          ((> (queue-length q) 0)
           (with-release-lock (lock)
             (let ((res (dequeue q)))
               (when (> (queue-length (ch-queue-resolvers ch)) 0)
                 (funcall (dequeue (ch-queue-resolvers ch)) t))
               (values res t))))
          (t (let ((promise (with-promise (resolve reject :resolve-fn resolver)
                              (queue (ch-dequeue-resolvers ch) resolver))))
               (release-lock lock)
               (let/cc k
                 ;; wait until some value is queued
                 (attach promise
                         (lambda (val closed-p)
                           (print closed-p)
                           (funcall k val)))))))))

(defun/cc chan<- (ch value)
  "Write to channel"
  (let ((lock (ch-lock ch))
        (q (ch-queue ch))
        (max-length (ch-max-resource ch)))
    (acquire-lock lock)
    (cond ((ch-closed-p ch)
           (with-release-lock (lock)
             (error "Error: Insert a value into a closed channel")))
          ((> (queue-length (ch-dequeue-resolvers ch)) 0)
           (with-release-lock (lock)
             ;; Directly path value to a waiting reader.
             (funcall (dequeue (ch-dequeue-resolvers ch)) value t)))
          ((or (null max-length)
               (< (queue-length q) max-length))
           (with-release-lock (lock)
             (queue q value)))
          (t (let ((promise (with-promise (resolve reject :resolve-fn resolver)
                              (queue (ch-dequeue-resolvers ch) resolver))))
               (release-lock lock)
               (let/cc k
                 ;; wait until some value is dequeued
                 (attach promise
                         (lambda (closed-p)
                           (when closed-p
                             (error "Error: Channel is closed when waiting to insert a value"))
                           (queue q value)
                           (funcall k)))))))))


;; --- TEMP: easy-test --- ;;

(defun test ()
  (init-cloutine 3)
  (unwind-protect
       (let ((ch (make-channel)))
         (clt (chan<- ch 100))
         (clt (format t "read after write: ~D" (<-chan ch)))
         ;;
         (clt (format t "read before write: ~D" (<-chan ch)))
         (clt (chan<- ch 200))
         ;;
         (clt (format t "concat 2 channel: ~D" (+ (<-chan ch) (<-chan ch))))
         (clt (chan<- ch 300))
         (clt (sleep 0.5)
              (chan<- ch 400))
         (clt (let ((a (<-chan ch)))
                (format t "bind to variable ~D" a)))
         (clt (chan<- ch 500)))
    (progn (sleep 1)
           (destroy-cloutine)))))

;; (test)


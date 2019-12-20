(defpackage cloutine/real-threads
  (:use :cl)
  (:export :start-real-threads
           :destroy-real-threads
           :queue-pp
           ;; --- debug --- ;;
           :*debug-print-p*
           :debug-print
           :debug-format)
  (:import-from :cloutine/multi-queue
                :multi-queue
                :init-multi-queue
                :queue-into
                :dequeue-from)
  (:import-from :bordeaux-threads
                :make-thread
                :make-lock
                :with-lock-held
                :destroy-thread
                :make-semaphore
                :wait-on-semaphore
                :signal-semaphore)
  (:import-from :cl-async
                :with-event-loop))
(in-package :cloutine/real-threads)

(defvar *real-thread-index* nil)

(defclass real-thread ()
  ((thread :initarg :instance :accessor thread-instance)
   (index :initarg :index :accessor thread-index)))

(defclass real-threads ()
  ((threads :initarg :rt-array :accessor threads-array)
   (multi-queue :initarg :mq :accessor threads-mq)
   (destroied-p :initform nil :accessor threads-destroied-p)))

(defun start-real-threads (n)
  (let* ((mq (init-multi-queue n))
         (rt-arr (make-array n))
         (rts (make-instance 'real-threads :mq mq))
         (sem-to-wait-start (make-semaphore)))
    (dotimes (i n)
      (let ((rt (make-instance 'real-thread :index i)))
        (setf (thread-instance rt)
              (make-thread (lambda () (process-thread rt rts sem-to-wait-start))))
        (setf (aref rt-arr i) rt)))
    (setf (threads-array rts) rt-arr)
    (signal-semaphore sem-to-wait-start :count n)
    rts))

(defmethod destroy-real-threads ((rts real-threads))
  (unless (threads-destroied-p rts)
    (setf (threads-destroied-p rts) t)
    (let ((rt-arr (threads-array rts)))
      (dotimes (i (length rt-arr))
        (let ((rt (aref rt-arr i)))
          (destroy-thread (thread-instance rt))))
      (debug-print :destroyed))))

(defmethod process-thread ((rt real-thread) (rts real-threads) sem-to-wait-start)
  ;; wait until real-threads is initialized
  (wait-on-semaphore sem-to-wait-start)
  (with-event-loop ()
    (loop
       (let ((index (thread-index rt)))
         (debug-format t "~&Thread ~D tryies to dequeue." index)
         (let ((process (dequeue-from (threads-mq rts) index)))
           ;; Note: dequeue-from is kept on wait until some queue has an element.
           (assert (functionp process))
           (debug-format t "~&Thread ~D starts to process." index)
           (let ((*real-thread-index* index))
             (funcall process)))))))

(defmethod queue-pp ((rts real-threads) process)
  (debug-format t "~&Queue process to thread indexed as ~D" *real-thread-index*)
  (when (threads-destroied-p rts)
    (error "The thread has been destroied."))
  (queue-into (threads-mq rts)
              (if *real-thread-index*
                  *real-thread-index*
                  0)
              process))

;; ----- debug ----- ;;

(defvar *debug-format-lock* (make-lock))
(defparameter *debug-print-p* nil)

(defun debug-print (object)
  (when *debug-print-p*
    (with-lock-held (*debug-format-lock*)
      (print object))))

(defun debug-format (stream control-string &rest args)
  (when *debug-print-p*
    (with-lock-held (*debug-format-lock*)
      (apply #'format (list* stream control-string args)))))

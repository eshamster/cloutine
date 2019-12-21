(defpackage cloutine/multi-queue
  (:use :cl)
  (:export :multi-queue
           :init-multi-queue
           :queue-into
           :dequeue-from)
  (:import-from :cloutine/queue
                :init-queue
                :queue
                :dequeue)
  (:import-from :bordeaux-threads
                :make-lock
                :make-semaphore
                :with-lock-held
                :wait-on-semaphore
                :signal-semaphore))
(in-package :cloutine/multi-queue)

(defclass multi-queue ()
  ((queues :initarg :queues :accessor mq-queues)
   (locks :initarg :locks :accessor mq-locks)
   (semaphore :initform (make-semaphore) :accessor mq-semaphore)))

(defun init-multi-queue (n)
  (let ((queues (make-array n))
        (locks (make-array n)))
    (dotimes (i n)
      (setf (aref queues i) (init-queue)
            (aref locks  i) (make-lock (format nil "Multi-Queue lock: ~D" i))))
    (make-instance 'multi-queue
                   :queues queues
                   :locks locks)))

(defmethod queue-count ((mq multi-queue))
  (length (mq-queues mq)))

(defmethod queue-into ((mq multi-queue) queue-index value)
  (assert (< queue-index (queue-count mq)))
  (with-lock-held ((aref (mq-locks mq) queue-index))
    (queue (aref (mq-queues mq) queue-index)
           value))
  (signal-semaphore (mq-semaphore mq)))

(defmethod dequeue-from ((mq multi-queue) prior-queue-index)
  "Dequeue from multi-queue.
At first, try to dequeue from prior queue specified by prior-queue-index.
After that, try to dequeue from other queues.
If there is no value to dequeue, it keeps a thread to wait until some value is queued."
  (assert (< prior-queue-index (queue-count mq)))
  (wait-on-semaphore (mq-semaphore mq))
  (macrolet ((try-dequeue (index)
               `(with-lock-held ((aref (mq-locks mq) ,index))
                  (let ((value (dequeue (aref (mq-queues mq) ,index))))
                    (when value
                      (return-from dequeue-from value))))))
    (try-dequeue prior-queue-index)
    (dotimes (i (queue-count mq))
      (unless (= i prior-queue-index)
        (try-dequeue i)))
    (error "No value is there.")))

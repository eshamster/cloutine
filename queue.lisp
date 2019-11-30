(defpackage cloutine/queue
  (:use :cl)
  (:export :init-queue
           :queue
           :dequeue
           :queue-length))
(in-package :cloutine/queue)

(defclass queue-item ()
  ((prev :initform nil :initarg :prev :accessor queue-item-prev)
   (next :initform nil :initarg :next :accessor queue-item-next)
   (value :initarg :value :accessor queue-item-value)))

(defclass queue ()
  ((head :initform nil :initarg :head :accessor queue-head)
   (tail :initform nil :initarg :tail :accessor queue-tail)
   (length :initform 0 :accessor queue-length)))

(defun init-queue ()
  (make-instance 'queue))

(defmethod queue ((q queue) value)
  (incf (queue-length q))
  (let ((item (make-instance 'queue-item :value value)))
    (with-slots (head tail) q
      (cond ((not head)
             (assert (not tail))
             (setf head item
                   tail item))
            (t (assert (and head tail))
               (let ((next-item tail))
                 (setf tail item
                       (queue-item-next item) next-item
                       (queue-item-prev next-item) item)))))))

(defmethod dequeue ((q queue))
  (setf (queue-length q)
        (max 0 (1- (queue-length q))))
  (with-slots (head tail) q
    (cond ((not head)
           (assert (not tail))
           nil)
          ((eq head tail)
           (let ((item head))
             (setf head nil
                   tail nil)
             (queue-item-value item)))
          (t (assert (and head tail))
             (let* ((item head)
                    (prev-item (queue-item-prev item)))
               (setf head prev-item
                     (queue-item-next prev-item) nil)
               (queue-item-value item))))))

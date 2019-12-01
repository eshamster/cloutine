(defpackage cloutine/sync/wait-group
  (:use :cl)
  (:export :wait-group
           :add-wait
           :done-wait
           :wait-all)
  (:import-from :bordeaux-threads
                :make-lock
                :with-lock-held
                :make-semaphore
                :wait-on-semaphore
                :signal-semaphore))
(in-package :cloutine/sync/wait-group)

(defclass wait-group ()
  ((sem-list :initform nil :accessor wg-sem-list)
   (lock :initform (make-lock "Wait-Group lock") :accessor wg-lock)))

(defmacro with-wg-lock (wg &body body)
  `(with-lock-held ((wg-lock ,wg))
     ,@body))

(defmethod add-wait ((wg wait-group) n)
  (with-wg-lock wg
    (dotimes (i n)
      (push (make-semaphore) (wg-sem-list wg))
      t)))

(defmethod done-wait ((wg wait-group))
  (with-wg-lock wg
    (let ((sem (pop (wg-sem-list wg))))
      (unless sem
        (error "There is no resource to done in wait-group."))
      (signal-semaphore sem)
      t)))

(defmethod wait-all ((wg wait-group))
  (dolist (sem (wg-sem-list wg))
    (wait-on-semaphore sem)))

(defpackage cloutine/pred-process
  (:use :cl)
  (:export :pred-process
           :resolved-p
           :call-pred-process))
(in-package :cloutine/pred-process)

(defclass pred-process ()
  ((process :initarg :process
            :accessor pred-process-process
            :documentation "A function without argument")
   (pred :initarg :pred
         :accessor pred-process-pred
         :documentation "A function without argument that returns generalized boolian")
   (resolved :initform nil
             :accessor pred-process-resolved)))

(defmethod resolved-p ((pp pred-process))
  (with-slots (resolved pred) pp
    (when resolved
      (return-from resolved-p t))
    (setf resolved (funcall pred))))

(defmethod call-pred-process ((pp pred-process))
  (unless (resolved-p pp)
    (error "The pred-process has not been resolved"))
  (funcall (pred-process-process pp)))

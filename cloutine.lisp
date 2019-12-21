(defpackage :cloutine/cloutine
  (:use :cl)
  (:export :init-cloutine
           :destroy-cloutine
           :cloutine
           :clt)
  (:import-from :cloutine/real-threads
                :start-real-threads
                :destroy-real-threads
                :queue-process)
  (:import-from :cl-cont
                :with-call/cc
                :without-call/cc))
(in-package :cloutine/cloutine)

(defvar *real-threads* nil)

(defun init-cloutine (n)
  (setf *real-threads* (start-real-threads n)))

(defun destroy-cloutine ()
  (when *real-threads*
    (destroy-real-threads *real-threads*)
    (setf *real-threads* nil)))

(defmacro cloutine (&body body)
  `(queue-process *real-threads*
                  (without-call/cc
                    (lambda ()
                      (with-call/cc
                        ,@body)))))

(defmacro clt (&body body)
  `(cloutine ,@body))

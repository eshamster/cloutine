(defpackage :cloutine/cloutine
  (:use :cl)
  (:export :init-cloutine
           :destroy-cloutine
           :cloutine
           :clt)
  (:import-from :cloutine/real-threads
                :start-real-threads
                :destroy-real-threads
                :queue-pp
                :*debug-print-p*
                :debug-print
                :debug-format)
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
  `(queue-pp *real-threads*
             (without-call/cc
               (lambda ()
                 (with-call/cc
                   ,@body)))))

(defmacro clt (&body body)
  `(cloutine ,@body))

;; ----- easy test ----- ;;

(defun test ()
  (let ((*debug-print-p* t))
    (init-cloutine 3)
    (macrolet ((clt-debug (&body body)
                 `(clt (let ((*debug-print-p* t))
                         ,@body))))
      (unwind-protect
           (progn (debug-print :start)
                  (dotimes (i 3)
                    (let ((i i))
                      (clt-debug
                       (debug-format t "~&TEST: ~D" i)
                       (dotimes (j 2)
                         (let ((j j))
                           (clt-debug (debug-format t "~&TEST: ~D-~D" i j)
                                      (sleep 0.1))))
                       (sleep 0.1))))
                  (debug-print :end)
                  (sleep 1)
                  (debug-print :sleep-end))
        (progn (debug-print :destroy)
               (destroy-cloutine))))))

(test)

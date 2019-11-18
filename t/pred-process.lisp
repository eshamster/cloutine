(defpackage cloutine/t/pred-process
  (:use :cl
        :rove
        :cloutine/pred-process))
(in-package cloutine/t/pred-process)

(defun same-bool-p (a b)
  (or (and a b)
      (and (not a) (not b))))

(deftest for-pred-process
  (let* ((count 0)
         (pp (make-instance
              'pred-process
              :pred (lambda () (> count 1))
              :process (lambda () 999)))
         (test-table
          `((:exp-pred nil
             :exp-error t
             :exp-process nil)
            (:exp-pred nil
             :exp-error t
             :exp-process nil)
            (:exp-pred t
             :exp-error nil
             :exp-process 999))))
    (dolist (tt test-table)
      (destructuring-bind (&key exp-pred exp-error exp-process) tt
        (ok (same-bool-p (resolved-p pp) exp-pred))
        (if exp-error
            (ok (signals (call-pred-process pp)))
            (ok (= (call-pred-process pp) exp-process)))
        (incf count)))))

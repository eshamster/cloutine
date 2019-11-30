(defpackage cloutine/t/queue
  (:use :cl
        :rove
        :cloutine/queue))
(in-package cloutine/t/queue)

(deftest for-queue
  (let ((q (init-queue))
        (test-seq
         ;; (:q <queue-value> <expected-queue-count>) OR
         ;; (:d <expected-dequeued-value> <expected-queue-count>)
         '((:d nil 0)
           (:q :a 1) ; a
           (:q :b 2) ; a b
           (:d :a 1) ; b
           (:q :c 2) ; b c
           (:d :b 1) ; c
           (:d :c 0)
           (:d nil 0)
           (:q :a 1) ; a
           (:d :a 0))))
    (dolist (ts test-seq)
      (destructuring-bind (op val count) ts
        (ecase op
          (:q (queue q val))
          (:d (ok (eq (dequeue q) val))))
        (ok (= (queue-length q) count))))))

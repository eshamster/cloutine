(defpackage cloutine/t/queue
  (:use :cl
        :rove
        :cloutine/queue))
(in-package cloutine/t/queue)

(deftest for-queue
  (let ((q (init-queue))
        (test-seq
         ;; (:q <queue-value>) OR
         ;; (:d <expected-dequeued-value>)
         '((:d nil)
           (:q :a) ; a
           (:q :b) ; a b
           (:d :a) ; b
           (:q :c) ; b c
           (:d :b) ; c
           (:d :c)
           (:d nil)
           (:q :a) ; a
           (:d :a))))
    (dolist (op test-seq)
      (ecase (car op)
        (:q (queue q (cadr op)))
        (:d (ok (eq (dequeue q) (cadr op))))))))

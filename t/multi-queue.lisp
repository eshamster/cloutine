(defpackage cloutine/t/multi-queue
  (:use :cl
        :rove
        :cloutine/multi-queue))
(in-package cloutine/t/multi-queue)

(deftest for-queue
  (let ((mq (init-multi-queue 2))
        (test-seq
         ;; (:q <queue-number> <queue-value>) OR
         ;; (:d <queue-number> <expected-dequeued-value>)
         '((:q 0 :a0)
           (:q 0 :a1)
           (:q 1 :b0)
           (:q 1 :b1)
           (:d 0 :a0)
           (:d 0 :a1)
           (:d 0 :b0) ; steal
           (:d 1 :b1))))
    (dolist (ts test-seq)
      (destructuring-bind (op index value) ts
        (ecase op
          (:q (queue-into mq index value))
          (:d (ok (eq (dequeue-from mq index) value))))))))

;; TODO:Test waiting when dequeueing from empty multi-queue

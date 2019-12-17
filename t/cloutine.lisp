(defpackage cloutine/t/cloutine
  (:use :cl
        :rove
        :cloutine/cloutine)
  (:import-from :cloutine/sync/wait-group
                :wait-group
                :add-wait
                :with-done-wait
                :wait-all)
  (:import-from :bordeaux-threads
                :make-semaphore
                :wait-on-semaphore
                :signal-semaphore))
(in-package cloutine/t/cloutine)

(defmacro with-clt ((num-thread &optional (timeout 0.1)) &body body)
  `(unwind-protect
        (progn (init-cloutine ,num-thread)
               ,@body)
     (progn (sleep ,timeout)
            (destroy-cloutine))))

(deftest cloutine-test
  (with-clt (2)
    (let ((wg (make-instance 'wait-group)))
      (add-wait wg 1)
      (clt (with-done-wait (wg)
             (let ((wg2 (make-instance 'wait-group))
                   (arr (make-array 2)))
               (add-wait wg2 2)
               (dotimes (i 2)
                 (let ((i i))
                   (clt (with-done-wait (wg2)
                          (setf (aref arr i) (+ i 100))))))
               (wait-all wg2)
               (ok (= (aref arr 0) 100))
               (ok (= (aref arr 1) 101)))))
      (wait-all wg))))

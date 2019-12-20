(defpackage cloutine/t/real-threads
  (:use :cl
        :rove
        :cloutine/real-threads)
  (:import-from :cloutine/sync/wait-group
                :wait-group
                :add-wait
                :with-done-wait
                :wait-all))
(in-package cloutine/t/real-threads)

(defmacro with-real-threads ((var n) &body body)
  `(let ((,var (start-real-threads ,n)))
     (unwind-protect
          (progn ,@body)
       (destroy-real-threads ,var))))

(deftest real-threads-test
  (with-real-threads (rts 2)
    (let ((wg (make-instance 'wait-group)))
      (add-wait wg 1)
      (queue-process rts (lambda ()
                           (with-done-wait (wg)
                             (let ((wg2 (make-instance 'wait-group))
                                   (arr (make-array 2)))
                               (add-wait wg2 2)
                               (dotimes (i 2)
                                 (let ((i i))
                                   (queue-process rts (lambda ()
                                                        (with-done-wait (wg2)
                                                          (setf (aref arr i) (+ i 100)))))))
                               (wait-all wg2)
                               (ok (= (aref arr 0) 100))
                               (ok (= (aref arr 1) 101))))))
      (wait-all wg))))

(defpackage cloutine/t/wait-group
  (:use :cl
        :rove
        :cloutine/sync/wait-group)
  (:import-from :bordeaux-threads
                :make-lock
                :with-lock-held
                :make-thread
                :destroy-thread))
(in-package cloutine/t/wait-group)

(defun make-test-thread (timeout-sec func)
  (let ((th (make-thread func)))
    (make-thread (lambda ()
                   (sleep timeout-sec)
                   (destroy-thread th)))))

(deftest wait-group-test
  (testing "Normal case"
    (let ((count 0)
          (lock (make-lock))
          (wg (make-instance 'wait-group)))
      (add-wait wg 3)
      (dotimes (i 3)
        (make-test-thread 0.5
                          (lambda ()
                            (unwind-protect
                                 (progn
                                   (sleep 0.1)
                                   (with-lock-held (lock)
                                     (incf count)))
                              (done-wait wg)))))
      (wait-all wg)
      (ok (= count 3))))
  (testing "with-done-wait"
    (let ((count 0)
          (lock (make-lock))
          (wg (make-instance 'wait-group)))
      (add-wait wg 3)
      (dotimes (i 3)
        (make-test-thread 0.5
                          (lambda ()
                            (with-done-wait (wg)
                              (sleep 0.1)
                              (with-lock-held (lock)
                                (incf count))))))
      (wait-all wg)
      (ok (= count 3))))
  (testing "Error if adding is not enough"
    (let ((wg (make-instance 'wait-group)))
      (add-wait wg 1)
      (ok (done-wait wg))
      (ok (signals (done-wait wg))))))

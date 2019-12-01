(defpackage cloutine/t/real-threads
  (:use :cl
        :rove
        :cloutine/real-threads)
  (:import-from :cloutine/sync/channel
                :make-channel
                :<-chan
                :chan<-)
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
    (let ((ch (make-channel))
          (wg (make-instance 'wait-group)))
      (add-wait wg 1)
      (queue-pp rts (lambda ()
                      (with-done-wait (wg)
                        (dotimes (i 2)
                          (let ((i i))
                            (queue-pp rts (lambda ()
                                            (sleep 0.01)
                                            (chan<- ch i)))))
                        (ok (find (<-chan ch) '(0 1)))
                        (ok (find (<-chan ch) '(0 1))))))
      (wait-all wg))))

(defpackage cloutine/t/channel
  (:use :cl
        :rove
        :cloutine/sync/channel)
  (:import-from :cloutine/cloutine
                :init-cloutine
                :destroy-cloutine
                :clt)
  (:import-from :cloutine/sync/wait-group
                :wait-group
                :add-wait
                :wait-all
                :with-done-wait)
  (:import-from :bordeaux-threads
                :make-semaphore
                :wait-on-semaphore
                :signal-semaphore)
  (:import-from :cl-cont
                :defun/cc
                :with-call/cc))
(in-package cloutine/t/channel)

(defmacro with-clt ((num-thread &optional (timeout 0.1)) &body body)
  `(unwind-protect
        (progn (init-cloutine ,num-thread)
               ,@body)
     (progn (sleep ,timeout)
            (destroy-cloutine))))

(defun/cc test-<-chan (ch expected-val expected-closed-p)
  (let ((val (<-chan ch))
        (closed-p (ch-closed-p ch)))
    (unless closed-p
      (ok (= val expected-val)))
    (if expected-closed-p
        (ok closed-p)
        (ok (not closed-p)))))

(deftest basic-test
  (let ((ch (make-channel))
        (wg (make-instance 'wait-group)))
    (add-wait wg 2)
    (with-clt (2)
      (clt (with-done-wait (wg)
             (chan<- ch 0)
             (chan<- ch 1)
             (chan<- ch 2)))
      (clt (with-done-wait (wg)
             (test-<-chan ch 0 nil)
             (test-<-chan ch 1 nil)
             (test-<-chan ch 2 nil)))
      (wait-all wg))))

(deftest close-test
  (let ((ch (make-channel))
        (wg (make-instance 'wait-group))
        (wait-sem (make-semaphore)))
    (add-wait wg 2)
    (with-clt (2)
      (clt (with-done-wait (wg)
             (chan<- ch 0)
             (chan<- ch 1)
             (wait-on-semaphore wait-sem)
             (close-channel ch)
             ;; (ok (signals (chan<- ch 2)))
             ))
      (clt (with-done-wait (wg)
             (test-<-chan ch 0 nil)
             (test-<-chan ch 1 nil)
             (signal-semaphore wait-sem)
             (test-<-chan ch nil t)))
      (wait-all wg))))

(deftest max-resource-test
  (testing "Wait when resource is full"
    (let ((ch (make-channel 2))
          (count 0)
          (wg (make-instance 'wait-group)))
      (add-wait wg 1)
      (with-clt (1)
        (clt (with-done-wait (wg)
               (chan<- ch 0)
               (incf count) ; 1
               (chan<- ch 1)
               (incf count) ; 2
               (test-<-chan ch 0 nil)
               (chan<- ch 2)
               (incf count) ; 3
               (chan<- ch 3)
               (incf count) ; 4 (never reach)
               )))
      (wait-all wg)
      (ok (= count 3))))
  (testing "Resolve wait after some data is retrieved from channel"
    (let ((ch (make-channel 2))
          (count 0)
          (wait-sem (make-semaphore))
          (wg (make-instance 'wait-group)))
      (add-wait wg 2)
      (with-clt (2)
        (clt (with-done-wait (wg)
               (chan<- ch 0)
               (incf count) ; 1
               (chan<- ch 1)
               (incf count) ; 2
               (signal-semaphore wait-sem)
               (chan<- ch 2)
               (incf count) ; 3
               ))
        (clt (with-done-wait (wg)
               (wait-on-semaphore wait-sem)
               (ok (= count 2))
               (test-<-chan ch 0 nil))))
      (wait-all wg)
      (ok (= count 3)))))

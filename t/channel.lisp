(defpackage cloutine/t/channel
  (:use :cl
        :rove
        :cloutine/sync/channel)
  (:import-from :bordeaux-threads
                :make-thread
                :destroy-thread
                :make-semaphore
                :wait-on-semaphore
                :signal-semaphore))
(in-package cloutine/t/channel)

(defun make-test-thread (timeout-sec sem func)
  (let ((th (make-thread
             (lambda ()
               (unwind-protect
                    (funcall func)
                 (signal-semaphore sem))))))
    (make-thread (lambda ()
                   (sleep timeout-sec)
                   (destroy-thread th)))))

(defun make-sems (n)
  (loop :for x :from 0 :below n :collect (make-semaphore)))

(defun wait-sems (sems)
  (dolist (sem sems)
    (wait-on-semaphore sem)))

(defun test-<-chan (ch expected-val expected-open-p)
  (multiple-value-bind (val open-p) (<-chan ch)
    (when open-p
      (ok (= val expected-val)))
    (if expected-open-p
        (ok open-p)
        (ok (not open-p)))))

(deftest basic-test
  (let ((ch (make-channel))
        (sems (make-sems 2)))
    (make-test-thread 1 (nth 0 sems)
                      (lambda ()
                        (chan<- ch 0)
                        (chan<- ch 1)
                        (chan<- ch 2)))
    (make-test-thread 1 (nth 1 sems)
                      (lambda ()
                        (test-<-chan ch 0 t)
                        (test-<-chan ch 1 t)
                        (test-<-chan ch 2 t)))
    (wait-sems sems)))

(deftest close-test
  (let ((ch (make-channel))
        (sems (make-sems 2))
        (wait-sem (make-semaphore)))
    (make-test-thread 1 (nth 0 sems)
                      (lambda ()
                        (chan<- ch 0)
                        (chan<- ch 1)
                        (wait-on-semaphore wait-sem)
                        (close-channel ch)
                        (ok (signals (chan<- ch 2)))))
    (make-test-thread 1 (nth 1 sems)
                      (lambda ()
                        (test-<-chan ch 0 t)
                        (test-<-chan ch 1 t)
                        (signal-semaphore wait-sem)
                        (test-<-chan ch nil nil)))
    (wait-sems sems)))

(deftest max-resource-test
  (testing "Wait when resource is full"
    (let ((ch (make-channel 2))
          (count 0)
          (sems (make-sems 1)))
      (make-test-thread 0.1 (nth 0 sems)
                        (lambda ()
                          (chan<- ch 0)
                          (incf count) ; 1
                          (chan<- ch 1)
                          (incf count) ; 2
                          (test-<-chan ch 0 t)
                          (chan<- ch 2)
                          (incf count) ; 3
                          (chan<- ch 3)
                          (incf count) ; 4 (never reach)
                          ))
      (wait-sems sems)
      (ok (= count 3))))
  (testing "Resolve wait after some data is retrieved from channel"
    (let ((ch (make-channel 2))
          (count 0)
          (wait-sem (make-semaphore))
          (sems (make-sems 1)))
      (make-test-thread 1 (nth 0 sems)
                        (lambda ()
                          (chan<- ch 0)
                          (incf count) ; 1
                          (chan<- ch 1)
                          (incf count) ; 2
                          (signal-semaphore wait-sem)
                          (chan<- ch 2)
                          (incf count) ; 3
                          ))
      (wait-on-semaphore wait-sem)
      (ok (= count 2))
      (test-<-chan ch 0 t)
      (wait-sems sems)
      (ok (= count 3)))))

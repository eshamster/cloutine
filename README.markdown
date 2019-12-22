[![Build Status](https://travis-ci.com/eshamster/cloutine.svg?token=iyjMkbymh2qKutWVzm5i&branch=master)](https://travis-ci.com/eshamster/cloutine)

# cloutine - prototype of coroutine like goroutine

## Examples

```lisp
CL-USER> (ql:quickload :cloutine :silent t)
(:CLOUTINE)
CL-USER> (use-package :cloutine)
T
;; Initialize with 2 threads.
CL-USER> (init-cloutine 2)
#<CLOUTINE/REAL-THREADS::REAL-THREADS #x3020011BEDDD>
CL-USER> (let ((chan (make-channel)))
           ;; clt is a macro to generate cloutine
           (clt (loop
                   ;; Read value from channel.
                   ;; If the channel is empty, wait untile a value is written or it is closed.
                   (let ((x (<-chan chan)))
                     (when (channel-closed-value-p x)
                       (return))
                     (print x))))
           (clt (dotimes (i 5)
                  ;; Write value to channel.
                  (chan<- chan i))
                ;; The channel is depends on cl-cont,
                ;; so unfortunatelly unwind-protected can't be used for this purpose.
                (close-channel chan)))
NIL

0
1
2
3
4
```

Of course, the number of real threads (it is specifed by `init-cloutine`) doesn't limit the number of cloutine. Here is the example.

```lisp
CL-USER> (ql:quickload :cloutine :silent t)
(:CLOUTINE)
CL-USER> (use-package :cloutine)
T
;; bt = bordeaux-threads
CL-USER> (defparameter *lock* (bt:make-lock))
*LOCK*
CL-USER> (init-cloutine 3)
#<CLOUTINE/REAL-THREADS::REAL-THREADS #x3020011BECCD>
;; There are three 3 threads.
;; Then, launch 5 (or 6) cloutine in the same time.
CL-USER> (let ((chan (make-channel)))
           (dotimes (i 5)
             (let ((i i))
               (clt (loop
                       (sleep 0.001)
                       (let ((x (<-chan chan)))
                         (when (channel-closed-value-p x)
                           (return))
                         (bt:with-lock-held (*lock*)
                           ;; *real-thread-index* is index of a real thread.
                           (format t "thread: ~D, cloutine: ~D, value: ~D~%"
                                   *real-thread-index* i x)))))))
           (clt (dotimes (i 15)
                  (chan<- chan i)
                  (when (= (mod i 5) 0)
                    (sleep 0.001)))
                (close-channel chan)))
NIL
thread: 2, cloutine: 1, value: 0
thread: 0, cloutine: 0, value: 1
thread: 2, cloutine: 1, value: 5
thread: 0, cloutine: 2, value: 2
thread: 2, cloutine: 1, value: 7
thread: 0, cloutine: 2, value: 8
thread: 1, cloutine: 3, value: 3
thread: 2, cloutine: 1, value: 9
thread: 0, cloutine: 2, value: 10
thread: 2, cloutine: 1, value: 11
thread: 1, cloutine: 3, value: 12
thread: 0, cloutine: 2, value: 13
thread: 2, cloutine: 1, value: 14
thread: 1, cloutine: 4, value: 4
thread: 0, cloutine: 0, value: 6
```


## Installation

This project is not registered to the Quicklisp repository. So you need to clone this project to a directory where `ql:quickload` can detect. If you are a [Roswell](https://github.com/roswell/roswell/) \
user, the following is easy.

```sh
$ ros install eshamster/cloutine
```

## Author

* eshamster (hamgoostar@gmail.com)

## Copyright

Copyright (c) 2019 eshamster (hamgoostar@gmail.com)

## License

Licensed under the LLGPL License.

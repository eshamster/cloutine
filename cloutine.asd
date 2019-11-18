#|
  This file is a part of cloutine project.
  Copyright (c) 2019 eshamster (hamgoostar@gmail.com)
|#

#|
  cloutine is a prototype of coroutine like goroutine

  Author: eshamster (hamgoostar@gmail.com)
|#

(defsystem cloutine
  :version "0.1"
  :class :package-inferred-system
  :author "eshamster"
  :license "LLGPL"
  :depends-on (:cloutine/main)
  :description "cloutine is a prototype of coroutine like goroutine"
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op cloutine/t))))

(defsystem cloutine/t
  :class :package-inferred-system
  :depends-on (:rove)
  :perform (test-op (o c) (symbol-call :rove '#:run c)))

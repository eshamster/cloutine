(uiop/package:define-package :cloutine/main
  (:nicknames :cloutine)
  (:use-reexport :cloutine/cloutine
                 :cloutine/sync/channel
                 :cloutine/sync/wait-group)
  (:import-from :cloutine/real-threads
                :*real-thread-index*)
  (:export :*real-thread-index*))

(in-package :cl-user)

(defpackage :stash
  (:use :cl :bordeaux-threads :usocket)
  (:export #:initialize
	   #:serve
	   #:handle-request
	   #:terminate
	   #:client
	   #:set-key-in-store
	   #:get-value-from-store
	   #:remove-from-store
	   #:string-split
	   #:parse
	   #:hash-keys
	   #:strip-whitespaces))

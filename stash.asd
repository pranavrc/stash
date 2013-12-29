(defpackage #:stash-system
  (:use :cl :asdf))

(in-package :stash-system)

(defsystem stash
  :name "stash"
  :author "Pranav Ravichandran"
  :description "In-memory key-value store in Common Lisp."
  :license "MIT"
  :serial t
  :components
  ((:file "package")
   (:file "stash"))
  :depends-on (bordeaux-threads usocket))

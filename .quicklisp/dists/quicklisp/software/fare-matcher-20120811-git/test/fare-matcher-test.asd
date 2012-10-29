;;; -*- Lisp -*-
(in-package :cl)

(asdf:defsystem :fare-matcher-test
  :depends-on (:fare-matcher :hu.dwim.stefil)
  :serial t
  :components ((:file "packages")
	       (:file "matcher")
               (:file "quasiquote")))

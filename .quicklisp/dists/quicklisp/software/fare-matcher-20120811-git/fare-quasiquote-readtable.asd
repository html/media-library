;;; -*- Lisp -*-

(asdf:defsystem :fare-quasiquote-readtable
  :depends-on (:named-readtables :fare-matcher)
  :components ((:file "quasiquote-readtable")))

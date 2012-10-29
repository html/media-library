;;; -*- Lisp -*-

(defsystem :fare-matcher
  :description "Lisp2-style Erlang/ML-like Extensible Pattern-Matcher for CL"
  :long-description "fare-matcher allows you to do pattern matching as in ML,
and to define your own extensions."
  :depends-on (:xcvb-utils)
  :serial t
  :components
  ((:file "packages")
   (:file "matcher")
   #-gcl ; it currently fails. quasiquote necessitates a big revamp anyway
   (:file "quasiquote")
   (:file "pp-quasiquote")
   ;;#-gcl ; 2.7.0-64.1 cannot defgeneric in a eval-now
   (:file "clos-match")
   (:file "mrd-extensions")))

;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;; named readtables for fare-quasiquote
;;; Copyright (c) 2011-2011 Fahree Reedaw <fare@tunes.org>
;;; See README.quasiquote

#+xcvb (module (:depends-on ("quasiquote" (:asdf "named-readtables"))))

(in-package :fare-quasiquote)

(eval-now
  (named-readtables:defreadtable :fare-quasiquote-mixin
    (:macro-char #\` #'read-read-time-backquote)
    (:macro-char #\, #'read-comma)
    (:macro-char #\# :dispatch)
    (:dispatch-macro-char #\# #\( #'read-hash-paren))

  (named-readtables:defreadtable :fare-quasiquote
    (:fuze :standard :fare-quasiquote-mixin)))

;; (in-readtable :fare-quasiquote)

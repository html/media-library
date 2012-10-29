;;; -*- Mode: Lisp ; Syntax: ANSI-Common-Lisp -*-
;;; ASDF-Utils, a stable interface to utilities originally from ASDF.
;;;
;;; Free Software available under an MIT-style license.
;;; Copyright (c) 2010 - 2012, Francois-Rene Rideau
;;;

(defsystem :asdf-utils
  :licence "MIT"
  :description "Utilities from ASDF, repackaged"
  :long-description "A copy of some utilities initially developed as part of ASDF"
  :components
  ((:file "package")
   (:file "utils" :depends-on ("package"))))

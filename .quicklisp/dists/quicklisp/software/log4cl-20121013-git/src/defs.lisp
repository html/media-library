;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;
;;; Copyright (c) 2012, Max Mikhanosha. All rights reserved.
;;;
;;; This file is licensed to You under the Apache License, Version 2.0
;;; (the "License"); you may not use this file except in compliance
;;; with the License.  You may obtain a copy of the License at
;;; http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.

(in-package #:log4cl-impl)
;;
;; Global variables and constants
;;
;; Define the log levels. Similar to Log4J, except that we add extra
;; nice log levels named "user1" through "user9" with "trace" log
;; level in-between user4 and user5
;;
;; Reasoning behind extra levels is: 
;;
;; Unlike log4j the design of log4cl allows for mulitple log levels to
;; be enabled simultaneously. This is currently not used, but may be
;; used in the future
;;
;; This will allow for more fine grained control of logging in the
;; future where it would be possible to enable TRACE but not DEBUG or
;; INFO
;;     
(defconstant +log-level-unset+  16)
(defconstant +log-level-user9+  15)
(defconstant +log-level-user8+  14)
(defconstant +log-level-user7+  13)
(defconstant +log-level-user6+  12)
(defconstant +log-level-user5+  11)
(defconstant +log-level-trace+  10)
(defconstant +log-level-user4+  9)
(defconstant +log-level-user3+  8)
(defconstant +log-level-user2+  7)
(defconstant +log-level-user1+  6)
(defconstant +log-level-debug+  5)
(defconstant +log-level-info+   4)
(defconstant +log-level-warn+   3)
(defconstant +log-level-error+  2)
(defconstant +log-level-fatal+  1)
(defconstant +log-level-off+    0)
(defconstant +min-log-level+ +log-level-fatal+)
(defconstant +max-log-level+ +log-level-user9+)

;; For converting log levels from string
(defparameter +log-level-from-letter+ "OFEWID1234T56789U")

(defparameter +log-level-symbols+
  '(off fatal error warn info
    debug user1 user2 user3 user4 trace
    user5 user6 user7 user8 user9 unset))

(defparameter +log-level-macro-symbols+
  (remove-if (lambda (x) (member x '(off unset)))
             +log-level-symbols+))

(defparameter +log-level-from-string+ 
  (mapcar 'string-upcase (mapcar 'symbol-name +log-level-symbols+)))

;; For converting level to string
(defparameter +log-level-to-keyword+
  (coerce '(:off :fatal :error :warn :info :debug 
            :user1 :user2 :user3 :user4 :trace :user5 :user6
            :user7 :user8 :user9)
          'simple-vector))

(defparameter +log-level-to-string+
  (map 'simple-vector #'string-upcase +log-level-to-keyword+))

(defparameter +log-level-to-lc-string+
  (map 'simple-vector #'string-downcase +log-level-to-keyword+))

(defvar *log-indent* 0
  "Indent level can be used to indent logging info, is printed by %I
pattern format")

(defvar *ndc-context* nil
  "Value that is printed by %x pattern format")

(defvar *log-event-time* nil
  "Value of (GET-UNIVERSAL-TIME) for the current log event")

(defvar *inside-user-log-function* nil
  "True when we are inside of user log function, used to distinguish
errors that are signaled the log statement itself, vs errors in layout
or appender.")

(define-condition log4cl-error (simple-error program-error) ()
  (:documentation "Base class for all LOG4CL errors"))

(defun log4cl-error (message &rest args)
  (error 'log4cl-error
         :format-control message
         :format-arguments args))

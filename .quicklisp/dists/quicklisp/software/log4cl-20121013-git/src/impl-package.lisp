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

;; Implementation package for LOG4CL, that can be included in the :USE
;; list of other packages. All logging functions that otherwise would
;; conflict with CL package or with common words are named with LOG-
;; prefix
;;
;; Use this package if you are extending LOG4CL or writing your own
;; appenders


(cl:defpackage #:log4cl-impl
  (:use #:cl #:bordeaux-threads)
  (:export
   ;; log levels
   #:+log-level-unset+ #:+log-level-unset+ #:+log-level-user9+
   #:+log-level-user8+ #:+log-level-user7+ #:+log-level-user6+
   #:+log-level-user5+ #:+log-level-user4+ #:+log-level-user3+
   #:+log-level-user2+ #:+log-level-user1+ #:+log-level-trace+
   #:+log-level-debug+ #:+log-level-info+ #:+log-level-warn+
   #:+log-level-error+ #:+log-level-fatal+ #:+log-level-off+
   ;; logging macros
   #:log-fatal #:log-error #:log-warn #:log-info #:log-debug #:log-trace
   #:log-user1 #:log-user2 #:log-user3 #:log-user4 #:log-user5 #:log-user6
   #:log-user7 #:log-user8 #:log-user9 #:log-sexp #:log-sexp-with-level
   ;; sexp version of logging macros
   #:log-sexp-fatal #:log-sexp-error #:log-sexp-warn #:log-sexp-info #:log-sexp-debug #:log-sexp-trace
   #:log-sexp-user1 #:log-sexp-user2 #:log-sexp-user3 #:log-sexp-user4 #:log-sexp-user5 #:log-sexp-user6
   #:log-sexp-user7 #:log-sexp-user8 #:log-sexp-user9 
   #:log-indented
   ;; logger access functions
   #:make-log-level #:make-logger
   #:set-log-level
   #:logger-parent
   #:logger-log-level
   #:logger-appenders 
   #:effective-log-level
   #:effective-appenders
   #:add-appender
   #:appender-added
   #:appender-removed
   #:logger-added
   #:logger-removed
   #:stream-appender
   #:log-config
   #:logger-name
   #:logger-category
   #:logger-depth
   #:naming-option
   #:log-level-from-object
   #:resolve-logger-form
   #:resolve-default-logging-form
   #:enclosing-scope-block-name
   #:reset-logging-configuration
   #:clear-logging-configuration
   ;; special variables
   #:*hierarchy* #:*root-logger* #:*default-logger-name* #:*log-indent*
   #:*ndc-context*
   ;; hierarchy
   #:hierarchy-index
   #:with-log-hierarchy
   #:in-log-hierarchy
   #:with-package-log-hierarchy
   #:in-package-log-hierarchy
   ;; layouts & appenders 
   #:layout #:layout-to-stream #:appender-do-append
   ;; standard layouts
   #:default-layout #:simple-layout
   ;; standard appenders
   #:appender
   #:stream-appender
   #:console-appender
   #:serialized-appender
   #:fixed-stream-appender
   #:appender-stream
   #:pattern-layout
   #:pattern-layout-error
   #:+min-log-level+
   #:+max-log-level+
   #:log-level-to-string
   #:with-ndc-context
   #:with-log-indent
   #:logger-additivity
   #:appender-error
   #:handle-appender-error
   #:counting-appender
   #:file-appender-base
   #:file-appender
   #:rolling-file-appender-base
   #:time-rolling-file-appender
   #:maybe-roll-file
   #:backup-log-file
   #:appender-logger-count
   #:close-appender
   #:remove-appender
   #:remove-all-appenders
   #:appender-filename
   #:daily-file-appender
   #:+self-logger+
   #:package-wrapper
   #:logger-categories
   #:property-parser
   #:parse-property-stream
   #:property-configurator
   #:conversion-pattern
   #:property-parser-error
   #:configure
   #:logger-children
   #:logger-descendants
   #:map-logger-children
   #:map-logger-descendants
   #:start-hierarchy-watcher-thread 
   #:stop-hierarchy-watcher-thread
   #:add-watch-token
   #:remove-watch-token
   #:watch-token-check
   #:log4cl-error
   #:save
   #:*configurations-file*
   #:*save-configurations-to-file*
   #:*max-configurations*
   #:restore
   #:same-configuration-p
   #:all-configurations
   #:list-configurations
   #:configuration-element
   #:configuration))




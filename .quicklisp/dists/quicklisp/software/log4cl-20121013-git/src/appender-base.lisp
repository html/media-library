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

;; Base APPENDER class and generics that logging core needs to see

(defclass appender ()
  ((layout :initform (make-instance 'simple-layout)
           :initarg :layout)
   (error :initform nil :accessor appender-error)
   (logger-count :initform 0 :accessor appender-logger-count
                 :type (integer 0)))
  (:documentation "Appender is log message sink, and is responsible
for physically delivering the log message, somewhere. The formatting
of message is done by layout.

Appenders can be called from multiple threads and are responsible for
serializing access to any resources.

Appender will not be appended into if its ERROR slot is non-nil.

ERROR slot will be automatically set to the condition object, if a
condition was raised while writing to the appender"))

(defgeneric appender-added (logger appender)
  (:documentation "Called when appender is added to a logger. Default
method is used to keep logger count, and if re-implemented
the (CALL-NEXT-METHOD) needs to be called."))

(defgeneric appender-removed (logger appender)
  (:documentation "Called when appender is removed from a logger
 Default method is used to keep logger refcount, and calls
CLOSE-APPENDER when it reaches zero. If re-implemented
the (CALL-NEXT-METHOD) needs to be called"))

(defgeneric close-appender (appender)
  (:documentation "Called when appender refcount reaches zero after
being positive. Should close any streams or files that appender had
opened."))

(defgeneric appender-do-append (appender logger level log-func)
  (:documentation
   "Writes the log message into the appender. Text of the log message
is specified indirectly via LOG-FUNC argument, which will be a
function that accepts a stream, and writes the text of log message to
it.

  This function should first figure out or obtain the stream to write
the log message to, and then call the LAYOUT-TO-STREAM function to have
layout do actual formatting.

If appender destination is ultimately not a stream, then it can
obtain the full text of the log message by calling LAYOUT-TO-STREAM
inside of WITH-OUTPUT-TO-STRING

Example:

 (defmethod appender-do-append ((self custom-appender) logger level log-func)
   (let ((stream (custom-appender-destination)))
     (layout-to-stream (slot-value self 'layout)
                       stream logger level log-func))
   (values))

Return value of this function is ignored"))

(defgeneric handle-appender-error (appender condition)
  (:documentation "Called when a condition is raised doing writing to
  the appender by APPENDER-DO-APPEND call.

  Before this method is called, the ERROR slot of the appender is set
  to CONDITION, thus preventing this appender from being used
  recursively. Thus its safe to use log statements from inside methods
  implementing this function

  Default method will log the condition.

  If this method resets the ERROR slot of the appender back to NIL, it
  is assumed that method had fixed whatever was wrong the appender,
  and the APPENDER-DO-APPEND will be attempted again"))

(defgeneric property-initarg-from-string (instance property value)
  (:documentation "Called on appenders and layouts to possibly convert
property value from a string into whatever its supposed to be. Default
method will handle numeric, boolean and string properties, by calling
PROPERTY-ALIST function"))

(defgeneric property-alist (instance)
  (:documentation "Should return list of valid object properties, each
element of the list being (INITARG SLOT TYPE) with INITARG being the
keyword, SLOT is the slot name for the property and TYPE one of:

Type                    | Description
------------------------|------------------------------------------------------
NUMBER or :NUMBER       | Integer property, converted by (parse-integer)
------------------------|------------------------------------------------------
BOOLEAN or :BOOLEAN     | Boolean, accepts \"true\" \"t\" \"on\" \"false\" 
                        | \"off\" \"nil\" and empty string
------------------------|------------------------------------------------------
STRING or :STRING       | Value as-is after the equal sign in NAME = <value>
                        | Whitespace is not stripped
------------------------|------------------------------------------------------
:STRING-SKIP-WHITESPACE | Value with the leading whitespace removed

Overriding this method to add extra properties is the only thing
needed to allow extra properties in custom appenders/layouts to be
configurable from by property file configurator. See also
PROPERTY-INITARG-FROM-STRING"))


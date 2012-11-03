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

(defgeneric log-level-from-object (obj package)
  (:documentation "Should return numeric log level from the user
representation, can be specialized per-package to have custom log
level names. Default implementation converts object to string and
parses \"fatal\" \"debug\" and so on. Called by MAKE-LOG-LEVEL
function"))

(defgeneric naming-option (package option)
  (:documentation "Return the automatic logger naming option
for the specified package. Valid options are keywords:

  :CATEGORY-SEPARATOR
    : String that separates category names, default method returns
      \":\"

  :CATEGORY-CASE
    : Determining how logger naming converts symbols to in the
      category name.

    Valid values are: 
    - NIL        :  As printed by PRINC (ie affected by active *READTABLE*)
    - :UPCASE    :  Convert to upper case
    - :DOWNCASE  :  Convert to lower case
    - :INVERT    :  Invert in the same way inverted READTABLE-CASE does it
    - :PRESERVE  :  Do not change

    Note that pattern layout offers similar facility that changes how
    logger category is printed on the output side

  :EXPR-VALUE-SEPARATOR
    : A string that separates expression and value printed
      by (LOG-SEXP). Default is equal sign

  :EXPR-VALUE-SUFFIX
    : A string inserted into the format statement after each
      expression and value pair printed by (LOG-SEXP). Default is
      \" ~:_\" (a space followed by conditional newline)"))


(defgeneric package-wrapper (package categories explicit-p)
  (:documentation
   "Allows packages to optionally massage logger names in their
namespace. CATEGORIES will be a list of of category names from parent
to child, and EXPLICIT-P will be non-NIL if that list was specified as
an explicit list constant in a logging macro.

Default method will prefix the passed category list with the current
package shortest nickname. 

Example:

    ;; Some package wishes to always wrap their logger names in weird prefix and suffix
    (defmethod package-wrapper ((pkg (eql *package*)) categories explicit-p)
      (if explicit-p categories
        (append '(foo bar) categories '(baz))))

Will result in the macro (MAKE-LOGGER :NAME) returning logger named
FOO:BAR:NAME:BAZ"))

(defgeneric resolve-logger-form (package env args)
  (:documentation "Is called by all logging macros to figure out the
logger to log into. PACKAGE and ENV are the current value of *PACKAGE*
and the macro environment of the logging macro, and ARGS
are its arguments.

Returns two values, first being either a logger, or a form that when
evaluated will return a logger, and second value being list of
arguments to be passed to the format statement that will log the
message.

When second value returned is NIL, then logging macro will not log any
message but will rather expand into a non-NIL value if logging is
enabled on that logger."))

(defgeneric resolve-default-logger-form (package env args)
  (:documentation "Is called by RESOLVE-LOGGER-FORM when logging macro
arguments do not specify the logger to log into. See
RESOLVE-LOGGER-FORM for return values"))

(defgeneric enclosing-scope-block-name (package env)
  (:documentation "Is called by RESOLVE-DEFAULT-LOGGER-FORM to try to
determine the enclosing lexical scope name. For example if logging
macro is being expanded while compiling local function BAR inside of a
definition of function FOO, the implementation of this method should
strive to return '(FOO BAR) if possible.

For CLOS method it is recommended that return value be a generic
function name, followed by optional qualifier, and then followed by
any non-T specializers, with EQL specializers flattened to their
values, for example for the :AROUND method FOO with lambda list
of ((OBJ1 BAR) (OPTION (EQL :BAZ)) OBJ3) should strive to return
'(FOO AROUND BAR BAZ) "))

#-(or sbcl)
(defmethod enclosing-scope-block-name (package env)
  "Default method that always return NIL"
  (declare (ignore package env)))

(defmethod log-level-from-object (arg package)
  "Converts human readable log level description in ARG into numeric log level.

Supported values for ARG are:

- Symbol or string which name matches log level, e.g: :debug, :info,
  DEBUG, USER1, :err \"off\"

- 1-character long symbol or string, used as a shortcut. All standard
  levels can be uniquely identified by their first
  character: (o)ff (f)atal (e)rror (w)arn (i)nfo (d)ebug (t)race (u)nset,

- 1 character digit 1 through 9 identifying user1 through user9 levels." 
  (declare (ignore package))
  (cond ((symbolp arg)
         (make-log-level (symbol-name arg)))
        ((stringp arg)
         (let ((len (length arg))
               match)
           (if (= 1 len)
               (setf match (position (char-upcase (char arg 0))
                                     +log-level-from-letter+))
               (let* ((name (string-upcase arg))
                      (len (length name)))
                 (loop 
                   for level from 0
                   for level-name in +log-level-from-string+
                   for tmp = (mismatch name level-name)
                   if (or (null tmp)
                          (= tmp len))
                   do (if match
                          (log4cl-error "~s matches more then one log level" arg)
                          (setf match level)))))
           (or match 
               (log4cl-error "~s does not match any log levels" arg))))
        ((and (numberp arg)
              (>= arg 0)
              (<= arg +log-level-unset+))
         arg)
        (t (log4cl-error "~s does not match any log levels" arg))))

(defmethod resolve-default-logger-form (package env args)
  "Returns the logger named after the enclosing lexical environment"
  (values (%get-logger
           (package-wrapper package
                            (enclosing-scope-block-name package env)
                            nil)
           (naming-option package :category-separator)
           (naming-option package :category-case))
          args))

(defmethod package-wrapper (package categories explicit-p)
  "Find the PACKAGES shortest name or nickname, and prefix CATEGORIES
list with it"
  (if explicit-p categories
      (append (split-into-categories (shortest-package-name package)
                                     package)
              categories)))

(defun shortest-package-name (package)
  "Return the shortest name or nickname of the package"
  (let ((name (package-name package)))
    (dolist (nickname (package-nicknames package))
      (when (< (length nickname) (length name))
        (setq name nickname)))
    name))

(defun join-categories (separator list)
  "Return a string with each element of LIST printed separated by
SEPARATOR"
  (let ((*print-pretty* nil)
        (*print-circle* nil))
    (with-output-to-string (s) 
      (princ (pop list) s)
      (dolist (elem list)
        (princ separator s)
        (princ elem s)))))

(defmethod naming-option (package option)
  "Return default values for naming options which are:
    :CATEGORY-SEPARATOR \":\""
  (declare (ignore package))
  (case option
    (:category-separator ":")
    (:expr-value-separator "=")
    (:expr-value-suffix " ~:_")))

(defmethod resolve-logger-form (package env args)
  "- When first element of args is NIL or a constant string, calls
 RESOLVE-DEFAULT-LOGGER-FORM that will try to obtain logger name from
 the environment

- When first argument is a :KEYWORD, returns logger named <KEYWORD>

- When first argument is a quoted symbol, returns logger named
  <current-package>.<symbol>

- Otherwise returns the form `(GET-LOGGER ,(FIRST ARGS) ,@(REST ARGS))'"
  (cond
    ((or (null args)
         (stringp (first args)))
     (resolve-default-logger-form package env args))
    ((keywordp (first args))
     (values (%get-logger
              (package-wrapper
               package
               (split-into-categories (symbol-name (first args))
                                      package)
               nil)
              (naming-option package :category-separator)
              (naming-option package :category-case))
             (rest args)))
    ((constantp (first args))
     (let ((value (eval (first args))))
       (cond ((symbolp value)
              (values
               (%get-logger
                (package-wrapper
                 package
                 (split-into-categories (symbol-name value) package)
                 nil)
                (naming-option package :category-separator)
                (naming-option package :category-case))
               (rest args)))
             ((listp value)
              (values
               (%get-logger (package-wrapper package value t)
                                    (naming-option package :category-separator)
                                    (naming-option package :category-case))
               (rest args)))
             (t (values (first args) (rest args))))))
    (t
     (values (first args) (rest args)))))

(defun write-string-modify-case (string stream
                                 &optional
                                 case
                                 (start 0)
                                 (end (length string)))
  "Helper function that writes STRING to STREAM, optionally doing case
conversion."
  (declare (type simple-string string)
           (type stream stream)
           (type fixnum start end))
  (cond ((eq case :upcase)
         (loop for i fixnum from start below end
               do (write-char (char-upcase (schar string i))
                              stream)))
        ((eq case :downcase)
         (loop for i fixnum from start below end
               do (write-char (char-downcase (schar string i))
                              stream)))
        ((eq case :invert)
         (let ((all-up t)
               (all-down t))
           (loop for i fixnum from start below end
                 as c = (schar string i)
                 if (upper-case-p c)
                 do (setq all-down nil)
                 if (lower-case-p c)
                 do (setq all-up nil))
           (cond
             (all-down (write-string-modify-case
                        string stream :upcase start end))
             (all-up (write-string-modify-case
                      string stream :downcase start end))
             (t (write-string string stream :start start
                                            :end end)))))
        (t (write-string string stream :start start
                                       :end end)))
  (values))


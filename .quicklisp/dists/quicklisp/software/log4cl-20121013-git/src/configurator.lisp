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

;;;
;;; Contains (log-config) function and default logging initialization
;;; 
(in-package #:log4cl-impl)

(defun clear-logging-configuration ()
  "Delete all loggers configuration, leaving only LOG4CL.SELF"
  (labels ((reset (logger)
             (remove-all-appenders logger)
             (setf (svref (logger-state logger) *hierarchy*)
                   (make-logger-state))
             (map-logger-children #'reset logger)))
    (reset *root-logger*)
    (when *self-log-config*
      (apply 'log-config +self-logger+ *self-log-config*)))
  (values))

(defun reset-logging-configuration ()
  "Clear the logging configuration in the current hierarchy, and
configure root logger with INFO log level and a simple console
appender"
  (clear-logging-configuration)
  (add-appender *root-logger* (make-instance 'console-appender))
  (setf (logger-log-level *root-logger*) +log-level-warn+)
  (log-info "Logging configuration was reset to sane defaults"))

(defun log-config (&rest args)
  "User friendly way of configuring loggers. General syntax is:

    (LOG-CONFIG [LOGGER-IDENTIFIER] OPTION1 OPTION2...)

LOGGER-IDENTIFIER can be one of:

* Logger instance ie result of (make-logger) expansion, or any form
  that returns a logger.

* A list of logger categories, basically a shortcut for (MAKE-LOGGER
  '(CAT1 CAT2 CAT3)). An error will be given if logger does not exist. If you want
  to ensure logger is created, even if it did not exist before, use
  (LOG-CONFIG (MAKE-LOGGER ...) ...)

If not specified, default logger will be root logger

Valid options can be:

  Option       | Description
---------------|---------------------------------------------------------------
 :INFO         | Or any other keyword identifying a log level, which can be    
 :DEBUG        | shortened to its shortest unambiguous prefix, such as :D      
---------------|---------------------------------------------------------------
 :CLEAR        | Removes log level and appenders from any child loggers,       
               | appenders are not removed from non-additive loggers           
---------------|---------------------------------------------------------------
 :ALL          | Changes :CLEAR to remove appenders from non-additive          
               | loggers                                                       
---------------|---------------------------------------------------------------
 :SANE         | Removes logger appenders, adds console appender with          
               | pattern layout that makes messages look like this:            
               |                                                               
               | [11:22:25] INFO  {category.name} - message
               |
               | If used with :DAILY then console appender is not added, unless
               | :CONSOLE or :THIS-CONSOLE is explicitly used
---------------|---------------------------------------------------------------
 :DAILY FILE   | Adds file appender logging to the named file, which will      
               | be rolled over every midnight into FILE.YYYYMMDD; Removes any 
               | other FILE-APPENDER-BASE'ed appenders from the logger
---------------|---------------------------------------------------------------
 :CONSOLE      | Adds CONSOLE-APPENDER to the logger. Console appender logs
               | into the *DEBUG-IO* at the call site.
               |
 :THIS-CONSOLE | Adds FIXED-STREAM-APPENDER to the logger, with :stream argument
               | taken from the current value of *DEBUG-IO*
---------------|---------------------------------------------------------------
:STREAM stream | Adds FIXED-STREAM-APPENDER logging to specified stream
---------------|---------------------------------------------------------------
 :PATTERN      | For any new appenders added, specifies the conversion pattern for the
               | PATTERN-LAYOUT
---------------|---------------------------------------------------------------
 :TWOLINE      | Changes default pattern layout to print user log message      
  or :2LINE    | log message on 2nd line after the headers                     
---------------|---------------------------------------------------------------
 :PROPERTIES   | Configure with PROPERTY-CONFIGURATOR by parsing specified     
 FILE          | properties file                                               
---------------|---------------------------------------------------------------
 :WATCH        | Used with :PROPERTIES, uses watcher thread to check           
               | properties file modification time, and reloads if it changes  
---------------|---------------------------------------------------------------
 :IMMEDIATE-   | Used with :SANE, :DAILY or :CONSOLE to create new appenders
  FLUSH        | with :IMMEDIATE-FLUSH T option, which prevents automatic
               | startup of hierarchy watcher thread, which is used for
               | auto-flushing. 
---------------|---------------------------------------------------------------
 :OWN          | For :SANE and :DAILY makes logger non-additive                
               | otherwise additive flag will be set                           
---------------|---------------------------------------------------------------

Examples:

* (LOG-CONFIG :D) -- Changes root logger level to debug

* (LOG-CONFIG :SANE) -- Changes root logger level to info, removes its
   appenders, adds console appender with pattern layout

* (LOG-CONFIG :SANE :THIS-CONSOLE) -- Same as above but adds fixed
   stream appender logging to current value of *DEBUG-IO* instead of
   regular console appender..

* (LOG-CONFIG :WARN :SANE :CLEAR :ALL) -- Changes root logger level to
  warnings, removes its appenders, adds console appender with pattern
  layout; then resets all child loggers log levels, and removes their
  appenders.

* (LOG-CONFIG (MAKE-LOGGER :FOOBAR) :SANE :OWN :D :DAILY \"debug.log\")

  Configures the specified logger with debug log level, logging into
  file debug.log which will be rolled over daily, and makes it
  non-additive ie any messages will not be propagated to logger
  parents.
" 
  (let ((logger nil)
        sane clear all own daily pattern 
        twoline level layout console
        orig-args
        self appenders
        immediate-flush
        properties watch
        this-console
        stream)
    (declare (type (or null stream) stream))
    (cond ((logger-p (car args))
           (setq logger (pop args)))
          ((consp (car args))
           (setq logger (%get-logger
                         (pop args)
                         (naming-option *package* :category-separator)
                         (naming-option *package* :category-case))))
          ((member :self args)
           (setq logger (make-logger '(log4cl-impl self))
                 self t)
           (setq args (remove :self args))))
    (setq logger (or logger *root-logger*))
    (unless (setq orig-args args)
      (return-from log-config (show-logger-settings logger)))
    (loop
      (let ((arg (or (pop args) (return))))
        (case arg
          (:self ; still in the arglist means 1st arg was a logger
           (log4cl-error "Specifying a logger is incompatible with :SELF"))
          (:sane (setq sane t))
          (:clear (setq clear t))
          (:all (setq all t))
          (:own (setq own t))
          (:immediate-flush (setq immediate-flush t))
          ((:twoline :two-line) (setq twoline t))
          (:console (setq console t))
          (:this-console (setq this-console t
                               console t))
          (:watch (setq watch t))
          (:daily
           (setq daily (or (pop args)
                           (log4cl-error ":DAILY missing argument"))))
          (:properties
           (setq properties (or (pop args)
                                (log4cl-error ":PROPERTIES missing argument"))))
          (:stream
           (setq stream (or (pop args)
                            (log4cl-error ":STREAM missing argument"))
                 console t
                 this-console t))
          (:pattern
           (setq pattern (or (pop args)
                             (log4cl-error ":PATTERN missing argument"))))
          (t (let ((lvl (handler-case (log-level-from-object arg *package*)
                          (log4cl-error ()))))
               (cond ((and level lvl)
                      (log4cl-error "Only one log level can be specified"))
                     (lvl (setq level lvl))
                     ((keywordp arg)
                      (log4cl-error "Invalid LOG-CONFIG keyword ~s" arg))
                     (t (log4cl-error
                         "Don't know what do with argument ~S" arg))))))))
    (or logger (setq logger *root-logger*))
    (or level sane clear daily properties own console
        (log4cl-error "A log level or one of :SANE :CLEAR :OWN :DAILY :CONSOLE or :PROPERTIES must be specified"))
    (or (not properties)
        (not (or sane daily pattern console level))
        (log4cl-error ":PROPERTIES can't be used with :SANE :DAILY :PATTERN or log level"))
    (when level
      (set-log-level logger level nil))
    (when clear
      (map-logger-descendants
       (lambda (l)
         (set-log-level l +log-level-unset+ nil)
         (when (or (logger-additivity l) all)
           (remove-all-appenders-internal l nil)))
       logger))
    (when own
      (set-additivity logger nil nil))
    (when (or daily sane console)
      (let ((default-pattern "[%D{%H:%M:%S}] [%P] <%c{}{}{:downcase}> - %m%n")
            (twoline-pattern "[%D{%H:%M:%S}] [%-5P] <%c{}{}{:downcase}>%n  *%I{>} %m%n"))
        (setq layout (make-instance 'pattern-layout
                      :conversion-pattern
                      (or pattern
                          (if twoline twoline-pattern
                              default-pattern)))))
      (if sane (remove-all-appenders-internal logger nil))
      ;; create daily appender
      (when daily
        (dolist (a (logger-appenders logger))
          (when (typep a 'file-appender-base)
            (remove-appender-internal logger a nil)))
        (push (make-instance 'daily-file-appender
               :name-format daily
               :backup-name-format (format nil "~a.%Y%m%d" daily)
               :layout layout)
            appenders))
      ;; create console appender
      (when (or (and sane (not daily))
                console)
        (push
            (if this-console
                (make-instance 'fixed-stream-appender
                 :stream (or stream *debug-io*)
                 :layout layout)
                (make-instance 'console-appender :layout layout))
            appenders))
      ;; now add all of them to the logger
      (dolist (a appenders)
        (when immediate-flush
          (setf (slot-value a 'immediate-flush) t))
        (add-appender-internal logger a nil))
      (set-additivity logger (not own) nil))
    (when properties
      (configure (make-instance 'property-configurator) properties
                 :auto-reload watch))
    (when self
      ;; This is special adhoc case of configuring the LOG4CL-iMPL:SELF.  We need
      ;; special processing, because we want self-logging to survive
      ;; the (clear-logging-configuration), which is done doing tests
      (let ((config (cons :own
                          ;; we don't remember these
                          (remove-if (lambda (x)
                                       (member x '(:own :self :clear :all)))
                                     orig-args))))
        ;; if specified new appenders, simply remember new configuration
        (if (or sane daily) (setq *self-log-config* config)
            ;; otherwise merge specified config and the remembered one
            ;; This is so (log-config :self :d) would still do same
            ;; thing as (log-config :self :d :sane), if old value of
            ;; *self-log-config* contained :sane
            (let (tmp doit)
              (when (setq tmp (member :sane *self-log-config*))
                (push :sane config)
                (setq doit t))
              (when (setq tmp (member :daily *self-log-config*))
                (setq config (append config (subseq tmp 0 2)))
                (setq doit t))
              (when (and (setq tmp (member :pattern *self-log-config*))
                         (not pattern))
                (setq config (append config (subseq tmp 0 2)))
                (setq doit t))
              (when (and (setq tmp (member :twoline *self-log-config*))
                         (not pattern))
                (setq config (cons :twoline (remove :twoline config))))
              (when doit (setq *self-log-config* config))))))
    ;; finally recalculate reach-ability
    (adjust-logger logger)))

(defun show-logger-settings (logger)
  "Print logger settings and its children to *STANDARD-OUTPUT*

Example output:

+ROOT, WARN
 |
 +-#<CONSOLE-APPENDER>
 |     with #<PATTERN-LAYOUT>
 |              :pattern [%P] %c %m%n
 |     :immediate-flush: nil
 |     :flush-interval: 1
 +-LOG4CL
   |
   +-SELF (non-additive), DEBUG
   | |  :config (:SANE, :OWN :TWOLINE :D)
   | |
   | +-#<CONSOLE-APPENDER 0x123123>,
   | |     with #<PATTERN-LAYOUT>
   | |              :pattern [%P] %c %m%n
   | |     :immediate-flush: nil
   | |     :flush-interval: 1
   | +-#<DAILY-ROLLING-APPENDER 0x12345>
   |       with #<SIMPLE-LAYOUT 1234>
   |       :immediate-flush NIL
   |       :flush-interval: 1
   | 
   +-OTHER, DEBUG
"
  (let ((indents '())
        (interesting-cache (make-hash-table)))
    (labels ((interesting-logger-p (l)
               ;; return if logger is worth mentioning, its only
               ;; interesting if its a root logger, is non-additive or
               ;; has custom log level or appenders
               (multiple-value-bind (result present)
                   (gethash l interesting-cache)
                 (if present result
                     (setf (gethash l interesting-cache)
                           (or (eq l logger)
                               (logger-appenders l)
                               (not (logger-additivity l))
                               (logger-log-level l)
                               (some #'interesting-logger-p (logger-children l)))))))
             (print-indent (&optional node-p)
               (dolist (elem (reverse indents))
                 (let* ((lastp (eq elem (car indents)))
                        (width (car elem))
                        (nodes-left (cdr elem)))
                   (loop repeat (1- width)
                         do (write-char #\Space))
                   (write-char (if (and lastp node-p)
                                   #\+
                                   ;; only continue drawing vertical line down
                                   ;; if there are more child nodes to be printed
                                   ;; at this level
                                   (if (plusp nodes-left)
                                       #\|
                                       #\Space)))))
               ;; Each time we print a child node, we decrement
               ;; number of child nodes left at this level, so that
               ;; we know when to stop drawing the vertical line linking
               ;; to more nodes of the same level
               (when node-p
                 (when indents
                   (assert (plusp (cdar indents)))
                   (decf (cdar indents)))))
             (print-one-logger (l)
               (let* ((lvl (logger-log-level l))
                      (appenders (logger-appenders l))
                      (additivity (logger-additivity l))
                      (children (remove-if-not #'interesting-logger-p (logger-children l)))
                      (cnt-nodes (+ (length appenders)
                                    (length children))))
                 (print-indent t)
                 (cond (indents
                        (format t "-~A" (logger-name l))
                        (push (cons 2 cnt-nodes) indents))
                       ;; start vertical line at column 0 for root node
                       (t (format t "~A" (logger-name l))
                          (push (cons 1 cnt-nodes) indents)))
                 (unless additivity
                   (write-string " (non-additive)"))
                 (when lvl
                   (format t ", ~A" (log-level-to-string lvl)))
                 (terpri)
                 (when appenders
                   (dolist (a appenders)
                     (print-one-appender a)))
                 (when (some #'interesting-logger-p children)
                   (dolist (l children)
                     (when (interesting-logger-p l)
                       (print-indent) (terpri)
                       (print-one-logger l))))
                 (pop indents)))
             (print-properties (obj)
               (let* ((prop-alist (property-alist obj))
                      (name-width (loop for prop in prop-alist maximize
                                           (length (format nil "~s" (first prop)))))
                      (indent (with-output-to-string (*standard-output*)
                                (print-indent)))
                      (*print-pretty* t))
                 (loop
                   for (initarg slot nil) in prop-alist
                   do (pprint-logical-block (nil nil :per-line-prefix indent)
                        (pprint-indent :block 0)
                        (write initarg :case :downcase)
                        (pprint-tab :section-relative 1 (1+ name-width))
                        (pprint-newline :miser)
                        (pprint-indent :block 0)
                        (write (slot-value obj slot)))
                   do (terpri))))
             (print-one-appender (a)
               ;; empty line for spacing
               (print-indent) (terpri)
               ;; now the appender node
               (print-indent t) (format t "-~A~%" a) 
               ;; indent appender attributes and layout under the appender,
               ;; don't draw the tree for them
               (push (cons 5 0) indents)
               (print-layout (slot-value a 'layout))
               (print-properties a)
               (pop indents))
             (print-layout (layout)
               (print-indent)
               (format t "with ~A~%" layout)
               (push (cons 5 0) indents)
               (print-properties layout)
               (pop indents)))
      (print-one-logger logger))
    (values)))

;; do default configuration
(defvar *default-init-done-p* nil)

(defun perform-default-init ()
  (unless *default-init-done-p*
    (setq *default-init-done-p* t)
    (clear-logging-configuration)
    (log-config :i :sane :immediate-flush)))

(perform-default-init)

;;;
;;; Logging configuration quick save / restore
;;; 

(defclass configuration-element ()
  ((logger :initarg :logger :type logger
           :initform (error "Required argument ~s missing" :logger)
           :reader logger-of)
   (level :initarg :level
          :type keyword
          :initform (error "Required argument ~s missing" :level)
          :reader level-of))
  (:documentation "Holds logger and its log level"))

(defclass configuration ()
  ((name :type atom :initarg :name 
         :initform (error "Required argument ~s missing" :name)
         :accessor name-of)
   (elements :initarg :elements
             :accessor elements-of
             :initform (remember-logging-configuration)))
  (:documentation "Used to remember log levels for a set of loggers"))

(defun remember-logging-configuration (&optional (logger *root-logger*))
  "Utility method to make a list of logging configuration starting from LOGGER. 
Returns a list of CONFIGURATION-ELEMENT objects"
  (flet ((make-element (logger level)
           (make-instance 'configuration-element
            :logger logger
            :level (if level (aref +log-level-to-keyword+ level) :unset))))
    (cons (make-element logger (logger-log-level logger)) 
          (loop for logger in (logger-descendants logger)
                for level = (logger-log-level logger)
                if level collect (make-element logger level)))))

(defmethod print-object ((elem configuration-element) stream)
  (with-slots (logger level) elem
    (if (not *print-readably*)
        (print-unreadable-object (elem stream :type t)
          (princ (if (logger-parent logger) (logger-category logger)
                     "+ROOT+") stream)
          (princ #\Space stream)
          (prin1 level stream))
        (format stream  "#.~S"
                `(make-instance 'configuration-element
                  :logger
                  (%get-logger ',(logger-categories logger)
                               ,(logger-category-separator logger)
                               nil)
                  :level ,level)))))

(defmethod print-object ((cnf configuration) stream)
  (with-slots (name elements) cnf
    (if (not *print-readably*)
        (print-unreadable-object (cnf stream :type t)
          (format stream "~S (~d)"
                  name (count :unset elements :key #'level-of :test-not #'eq)))
        (format stream  "#.~S"
                `(make-instance 'configuration :name ',name :elements ',elements)))))

(defvar *configurations* nil
  "List of all LOGGER-CONFIGURATION objects")

(defvar *max-configurations* 30
  "Maximum number of configurations in *CONFIGURATIONS* list")

(defvar *save-configurations-to-file* t
  "When non-NIL SAVE will also write configurations to a
*CONFIGURATIONS-FILE*")

(defvar *configurations-file* ".log4cl-configurations.lisp-expr"
  "The file where configurations will be saved. Is merged with result
of USER-HOMEDIR-PATHNAME")

(defun same-configuration-p (c1 c2)
  "Compare two logging configurations and return T if they have
exactly same loggers and levels"
  (declare (type configuration c1 c2))
  (or (eq c1 c2)
      (not (flet ((test (e1 e2) 
                    (and (eq (logger-of e1) (logger-of e2))
                         (eq (level-of e1) (level-of e2)))))
             (set-exclusive-or (elements-of c1) (elements-of c2) :test #'test)))))

(defun save (&optional name)
  "Save current logging configuration into configuration list.

NAME -- specifies the name of this logging configuration, if NAME is not
specified, one is automatically provided as \"Saved on <timestamp>\".

If its equivalent to some other configuration, save it only if it had
a different name, otherwise lift the older equivalent configuration to
the top of the list.

When *SAVE-CONFIGURATIONS-TO-FILE* is T (default) the configuration
list list will also be saved to a file
\".log4cl-configurations.lisp-expr\" in user home directory. File name
can be customized by changing *CONFIGURATIONS-FILE* variable"
  (save-configuration
   (make-instance 'configuration
    :name (or name
              (with-output-to-string (s)
                (format-time s "Saved on %Y-%m-%d %H:%M:%S"
                             (get-universal-time) nil))))
   name))

(defun apply-logging-configuration (cnf)
  "Restores logging configuration"
  (let ((root (logger-of (first (elements-of cnf)))))
    (map-logger-descendants (lambda (logger)
                              (set-log-level logger +log-level-unset+ nil))
                            root)
    (mapc (lambda (elem)
            (set-log-level (logger-of elem) (level-of elem) nil))
          (elements-of cnf))
    (adjust-logger root)))

(defun make-autosave-configuration ()
  (make-instance 'configuration
   :name (with-output-to-string (s)
           (format-time s "Autosave on %Y-%m-%d %H:%M:%S"
                        (get-universal-time) nil))))

(defun maybe-restore-configurations ()
  "If configurations list empty and *CONFIGURATIONS-FILE* exists in
home directory restore configuration list from it"
  (when (and (null *configurations*)
             (probe-file (merge-pathnames *configurations-file*
                                          (user-homedir-pathname))))
    (setq *configurations* (read-configurations-from-file))))

(defun restore (&optional configuration from-end)
  "Restore logging configuration CONFIGURATION, which can be a name,
a CONFIGURATION instance, or a number indicating Nth (zero
based) configuration in the *CONFIGURATIONS* list. NIL is treated as
zero.

When searching for the 



Before restoring the configuration, the current logging configuration
is automatically saved under the name \"Autosave <timestamp>\", unless
an equivalent configuration is already in the list

If CONFIGURATION is NIL restores first element of *CONFIGURATIONS* that is
not equivalent to the current configuration. So successive (RESTORE) will
swap last two configurations"

  (maybe-restore-configurations)
  
  (let* ((current (make-autosave-configuration))
         (current-dup (find current *configurations* :test #'same-configuration-p))
         (cnf 
           (cond ((null configuration)
                  ;; restore the first configuration not equivalent to the current one
                  (or (find current *configurations* :test-not #'same-configuration-p
                                                     :from-end from-end)
                      (error (if *configurations*
                                 "All stored configurations are equivalent to the current one"
                                 "There are no stored configurations to restore"))))
                 ((typep configuration 'configuration)
                  configuration)
                 ((integerp configuration)
                  (or (<= 0 configuration (length *configurations*))
                      (error "Invalid configuration index ~D" configuration))
                  (or (find current *configurations* :test-not #'same-configuration-p
                                                     :start configuration
                                                     :from-end from-end)
                      (error "All configurations starting from ~D are equivalent to the current one"
                             configuration)))
                 (t (or
                     ;; first try to find the named configuration that is different from current
                     (find configuration *configurations*
                           :test (lambda (name cnf)
                                   (and (equal (name-of cnf) name)
                                        (not (same-configuration-p cnf current))))
                           :from-end from-end)
                     ;; next just use name
                     (find configuration *configurations* :key #'name-of :test #'equal
                                                          :from-end from-end)
                     (error "Logging configuration ~S not found" configuration)))))
         (same-as-current-p (same-configuration-p cnf current)))
    ;; (log-sexp save save-dup (find save *configurations* :test #'same-configuration-p))
    (cond (same-as-current-p
           ;; simply move to the top
           (setq *configurations* (remove cnf *configurations*))
           (setq *configurations* (remove current-dup *configurations*))
           (push cnf *configurations*))
          (t 
           ;; When we have equivalent configuration to the current one,
           ;; move it to the top rather then overwriting it with "Autosave" entry
           (when current-dup
             (setq *configurations* (remove current-dup *configurations*))
             (setq current current-dup))
           (setf *configurations* (remove cnf *configurations*)) 
           (push current *configurations*) 
           (push cnf *configurations*) 
           (apply-logging-configuration cnf))) 
    (trim-configuration-list '*configurations*) 
    (when *save-configurations-to-file*
      (save-configurations-to-file)) 
    (first *configurations*)))

(defun trim-configuration-list (&optional (var '*configurations*))
  "Trim the list in global variable VAR to at most
*MAX-CONFIGURATIONS* elements"
  (let ((value (symbol-value var))) 
    (set var
         (subseq value 0 (min *max-configurations* (length value))))))

(defun auto-named-p (cnf)
  "Test if logging configuration name was auto-generated"
  (let ((name (name-of cnf)))
    (and (stringp name)
         (or (eql 0 (search "Autosave on" name)) 
             (eql 0 (search "Saved on" name))))))

(defun save-configuration (cnf had-name-p)
  "Save CNF logging configuration into *CONFIGURATIONS* list. If its equivalent
to some other configuration, save it only if it had a different name, otherwise
lift the older equivalent configuration to the top of the list"
  (declare (type configuration cnf))
  (let ((old (find cnf *configurations* :test #'same-configuration-p)))
    (cond
      ;; If new configuration has user-given name, and equivalent old
      ;; configuration has same name, or its name was auto-generated,
      ;; then remove the old
      ((and old had-name-p
            (or (equal (name-of cnf) (name-of old))
                (auto-named-p old)))
       (setq *configurations* (remove old *configurations*))
       (push cnf *configurations*))
      ;; When new configuration is auto-named, and older equivalent exists,
      ;; raise the older one in the list instead
      ((and old (not had-name-p))
       (setq *configurations* (remove old *configurations*))
       (push old *configurations*))
      (t (push cnf *configurations*)))
    (trim-configuration-list '*configurations*)
    (when *save-configurations-to-file*
      (save-configurations-to-file))
    (first *configurations*)))

(defun save-configurations-to-file (&optional (file *configurations-file*))
  (let ((file (merge-pathnames file (user-homedir-pathname)))
        (*package* (find-package :cl)))
    (with-open-file (out file :direction :output :if-exists :supersede
                              :if-does-not-exist :create)
      (write *configurations* :stream out :length nil :readably t :circle nil :pretty nil))))

(defun read-configurations-from-file (&optional (file *configurations-file*))
  (let ((file (merge-pathnames file (user-homedir-pathname)))
        (*package* (find-package :cl))
        (*read-eval* t))
    (with-open-file (input file)
      (read input))))

(defun all-configurations ()
  "Returns the *CONFIGURATIONS* list"
  *configurations*)

(defun list-configurations (&optional (stream *standard-output*))
  "Prints the *CONFIGURATIONS* list"
  (maybe-restore-configurations)
  (loop for cnt from 0 
        for cnf in *configurations*
        do (format stream "~4:<~d.~> ~A~%" cnt cnf)))

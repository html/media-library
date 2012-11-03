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

(defmethod property-alist ((instance appender))
  "Abstract appender has no properties"
  '())

(defun log-appender-error (appender condition)
  (log-error '(log4cl) "Appender ~s disabled because of ~s" appender condition))

(defmethod handle-appender-error (appender condition)
  (log-appender-error appender condition))

(defclass counting-appender (appender)
  ((count :initform 0))
  (:documentation "Count the number of times APPENDER-DO-APPEND was called"))

(defmethod appender-do-append ((appender counting-appender) logger level log-func)
  (with-slots (layout count)
      appender
    (incf count)
    ;; we need to actually format the log message, to invoke any side effects
    ;; that formatting it may produce, this is used in testing error handling
    (with-output-to-string (s)
      (layout-to-stream layout s logger level log-func))
    (when (next-method-p)
      (call-next-method))))

(defclass serialized-appender (appender)
  ((%lock :initform (make-lock)))
  (:documentation "Appender that serializes itself using a lock"))

(defclass stream-appender (serialized-appender)
  ((immediate-flush
    :initform #+bordeaux-threads nil
              #-bordeaux-threads t
    :initarg :immediate-flush)
   (flush-interval   :initform 1 :initarg :flush-interval)
   (%last-flush-time  :initform 0)
   (%output-since-flush :initform nil))
  (:documentation "Appender that writes message to stream. Stream is
obtained on each output by calling APPENDER-STREAM function.

Properties:

IMMEDIATE-FLUSH

: When non-NIL will call FINISH-OUTPUT after every log message

FLUSH-INTERVAL

: When set, will only flush if previous flush was earlier than
FLUSH-INTERVAL seconds ago. In addition a background thread will be
used to flush all appenders with FLUSH-INTERVAL set. See
ADD-WATCH-TOKEN"))

(defmethod property-alist ((instance stream-appender))
  '((:immediate-flush immediate-flush boolean)
    (:flush-interval flush-interval number)))

(defgeneric appender-stream (appender) 
  (:documentation "Should return the stream to which appender will write log messages"))

(defclass fixed-stream-appender-base (stream-appender)
  ((stream :accessor appender-stream))
  (:documentation "Appender that writes message to the stream in STREAM slot"))

(defclass fixed-stream-appender (fixed-stream-appender-base)
  ((stream :initarg :stream :accessor appender-stream))
  (:documentation "Appender that writes message to the stream in STREAM slot"))

(defmethod property-alist ((instance fixed-stream-appender))
  (append (call-next-method)
          '((:stream stream :symbol-value))))

(defclass console-appender (stream-appender) () 
  (:documentation "A stream appender that writes messages to
*debug-io* stream.  The *debug-io* is late-binding, that is its the
value of that variable in the thread and at the moment of log message
being written.  If instead you want an appender that would write log
messages to the *debug-io* stream active when appender was created,
use FIXED-STREAM-APPENDER class"))

#+bordeaux-threads
(defmethod appender-added :after (logger (appender stream-appender))
  "Add appender to the watch tokens in the current hierarchy,
unless IMMEDAITE-FLUSH property is set."
  (declare (ignore logger))
  (with-slots (immediate-flush)
      appender
    (when (not immediate-flush)
      (add-watch-token appender :test #'eq))))

#+bordeaux-threads
(defmethod appender-removed :after (logger (appender stream-appender))
  "When appender refcount is zero, remove it from watch tokens"
  (declare (ignore logger))
  (with-slots (logger-count immediate-flush)
      appender
    (when (zerop logger-count)
      (remove-watch-token appender :test #'eq))))

(defmethod watch-token-check ((appender stream-appender))
  (with-slots (immediate-flush flush-interval %last-flush-time %lock
               %output-since-flush)
      appender
    (let ((time *watcher-event-time*))
      (when (and (not immediate-flush)
                 flush-interval
                 %output-since-flush)
        (let ((since-last-flush (- time %last-flush-time)))
          ;; flush it now if by the next heartbeat we'll be late for
          ;; example if flush-interval is 2, and heartbeat 5, and its
          ;; been 1 second since last flush, then it will be (> (+ 1
          ;; 5) 2) which would flush right now, since next opportunity
          ;; to flush will be only 5 seconds later.
          ;;
          ;; Same calculation with flush-interval 2, heartbeat 0.5 and
          ;; 1 second since last flush will be (> (+ 1 0.5) 2) which
          ;; will be false, because we'll come back in 0.5 seconds and
          ;; flush then.
          (when (> (+ since-last-flush *hierarchy-watcher-heartbeat*)
                   flush-interval)
            (with-lock-held (%lock)
              (setf %last-flush-time    time
                    %output-since-flush nil)
              (finish-output (appender-stream appender)))))))))

(defun maybe-flush-appender-stream (appender stream)
  "Flush the APPENDER's stream if needed"
  (with-slots (immediate-flush flush-interval %last-flush-time
               %output-since-flush)
      appender
    (cond (immediate-flush
           (finish-output stream)
           (setf %output-since-flush nil
                 %last-flush-time    (log-event-time)))
          (flush-interval
           (let ((time (log-event-time)))
             (when (and (>= (- time %last-flush-time) flush-interval))
               (finish-output stream)
               (setf %output-since-flush nil
                     %last-flush-time    time)))))))

(defmethod appender-do-append :around
    ((this serialized-appender) logger level log-func)
  (declare (ignore logger level log-func))
  (with-lock-held ((slot-value this '%lock))
    (call-next-method)))

(defmethod appender-do-append ((this stream-appender) logger level log-func)
  (let ((stream (appender-stream this)))
    (with-slots (layout %output-since-flush) this
      (layout-to-stream layout stream logger level log-func)
      (setf %output-since-flush t)
      (maybe-flush-appender-stream this stream)))
  (values))

;; Save one generic function dispatch by accessing STREAM slot directly
(defmethod appender-do-append ((this fixed-stream-appender-base)
                               logger
			       level
                               log-func)
  (with-slots (layout stream %output-since-flush) this
    (layout-to-stream layout stream logger level log-func)
    (setf %output-since-flush t)
    (maybe-flush-appender-stream this stream))
  (values))

(defmethod appender-stream ((this console-appender))
  "Returns current value of *DEBUG-IO*"
  *debug-io*)

(defgeneric appender-filename (appender)
  (:documentation "Returns the appenders file name"))

(defun maybe-close-file (appender)
  (when (and (slot-boundp appender 'stream))
    (close (slot-value appender 'stream))
    (slot-makunbound appender 'stream)))

(defclass file-appender-base (fixed-stream-appender-base) () 
  (:documentation "Appender that writes to a file and closes it when
its no longer attached to loggers"))

(defmethod close-appender ((appender file-appender-base))
  (maybe-close-file appender))

(defclass file-appender (file-appender-base)
  ((filename :initarg :file)) 
  (:documentation "Appender that writes to a file with a fixed file
name"))

(defmethod property-alist ((instance file-appender))
  (append (call-next-method)
          '((:file filename :string-skip-whitespace))))

(defmethod appender-filename ((appender file-appender-base))
  (slot-value appender 'filename))

(defclass rolling-file-appender-base (file-appender-base)
  ((%rollover-check-period :initform 60 :initarg :rollover-check-period)
   (%next-rollover-time :initform 0))
  (:documentation
  "File appender that periodically checks if it needs to rollover the
log file.

Properties:

ROLLOVER-CHECK-PERIOD

: An integer, when current time advances past the boundary evenly divisible by this
number a call to MAYBE-ROLL-FILE will be made to check if log file needs
to be rolled over"))

(defmethod property-alist ((instance rolling-file-appender-base))
  (append (call-next-method)
          '((:rollover-check-period %rollover-check-period number))))

(defclass daily-file-appender (rolling-file-appender-base)
  ((backup-name-format  :initform nil :initarg :backup-name-format)
   (name-format :initarg :name-format)
   (utc-p :initform nil :initarg :utc)
   ;; File name of the currently active log file
   (%current-file-name :initform nil)
   ;; The name that the currently active file will be renamed into
   (%next-backup-name :initform nil))
  (:documentation "An appender that writes to the file named by
expanding a pattern.  The expansion is done by the same
converter as the %d conversion pattern of the PATTERN-LAYOUT, which is
a subset of patterns supported by strftime POSIX function.

Properties:

NAME-FORMAT
   : Expanded with date formatter to get the name of the current log file

BACKUP-NAME-FORMAT
   : Expanded with date formatter to get the name of the backup log
   file

UTC-P
   : Should be non-NIL if name and backup patterns expand the UTC time
   instead of local. Defaults to NIL.

MAYBE-ROLL-FILE method works as follows. It expands both name and
backup format (if present).

If either of them differs from their previous values, current log file
will be closed, and a new current log file will be opened.

The old log file will be renamed to %NEXT-BACKUP-NAME, which is a
value of the backup format expansion remembered when original log file
was opened.  The new value of the backup format expansion is
remembered in the %NEXT-BACKUP-NAME slot.

In below examples it is assumed that current log file was created an
2012-02-21, and the event being logged is the first one on the next
day.

  1) Example: NAME-FORMAT is \"test.log\" and backup is unset, will
     always log to the file named test.log

  2) Example: NAME-FORMAT is \"test.%Y%m%d.log\" and
     BACKUP-NAME-FORMAT is unset. Will log into the file
     test.20120221.log file, on the rollover it will be closed and
     test.20120222.file will be opened.

  3) Example: NAME-FORMAT is \"test.log\" and BACKUP-NAME-FORMAT is
     \"test.%Y%m%d.log\". Will log into the file test.log. On rollover
     test.log will be renamed to test.20120221.log, and new test.log
     will be created.

  4) Example: NAME-FORMAT is \"test.%Y%m%d\" and BACKUP-NAME-FORMAT is
     \"test.log.bak\". Will log into the file test.20120210.log and
     when the day changes, will rename it to test.log.bak (erasing old
     one if it exists)"))

(defmethod property-alist ((instance daily-file-appender))
  (append (call-next-method)
          '((:name-format name-format :string-skip-whitespace)
            (:backup-name-format backup-name-format :string-skip-whitespace)
            (:utc utc-p boolean))))

(defmethod appender-filename ((appender daily-file-appender))
  (slot-value appender '%current-file-name))

(defun next-time-boundary (time check-period)
  "Given universal time TIME return next boundary evenly divisible by
CHECK-PERIOD seconds "
  (declare (type unsigned-byte time check-period))
  (* check-period (truncate (+ time check-period) check-period)))

(defmethod appender-do-append :before ((this rolling-file-appender-base)
                                       logger level log-func)
  (declare (ignore logger level log-func))
  (let ((time (log-event-time)))
    (with-slots (%next-rollover-time %rollover-check-period) this
      ;; (log-sexp time %next-rollover-time (>= time %next-rollover-time))
      (when (>= time %next-rollover-time)
        (setf %next-rollover-time (next-time-boundary time %rollover-check-period))
        (maybe-roll-file this))))
  (values))

(defgeneric maybe-roll-file (appender)
  (:documentation "Should rollover the log file file if needed"))

(defmethod slot-unbound (class (appender file-appender-base)
                         (slot-name (eql 'stream)))
  (declare (ignore class slot-name))
  (create-appender-file appender))

(defun create-appender-file (appender)
  (let ((filename (appender-filename appender)))
    (maybe-close-file appender)
    (setf (slot-value appender 'stream)
          (open (ensure-directories-exist filename)
                #+ccl :sharing #+ccl :external
                :direction :output
                :if-exists :append
                :if-does-not-exist :create))))

(defun expand-name-format (pattern time utc-p)
  (let ((*print-pretty* nil))
    (with-output-to-string (s)
      (format-time
       s (coerce (if (pathnamep pattern) (format nil "~a" pattern)
                     pattern) 'simple-string)
       time utc-p))))

(defgeneric backup-log-file (appender log-filename backup-filename)
  (:documentation "Should move or rename LOG-FILENAME into the
BACKUP-FILENAME. When this function is called, LOG-FILENAME is already
closed.

Implemented as generic function so its possible to write extensions
that compress the backup log files automatically, or append to
them. One possible extension could be having daily log file and a
weekly backup, that is appended to each day")
  (:method (appender log-filename backup-filename)
    (declare (ignore appender))
    (rename-file log-filename backup-filename)))

(defmethod maybe-roll-file ((appender daily-file-appender)) 
  "Expands FILENAME and BACKUP patterns, and if one of them changed,
switches to the new log file"
  (with-slots (name-format backup-name-format
               %current-file-name %next-backup-name utc-p) appender
    (let* ((time (log-event-time))
           (new-file (expand-name-format name-format time utc-p))
           (new-bak (expand-name-format
                     (or backup-name-format name-format) time utc-p)))
      ;; (log-sexp time new-file new-bak %current-file-name %next-backup-name)
      (unless (and (equal new-file %current-file-name)
                   (equal new-bak %next-backup-name))
        (when %current-file-name
          (maybe-close-file appender)
          (unless (equal %current-file-name %next-backup-name)
            (backup-log-file appender %current-file-name %next-backup-name)))
        (setq %current-file-name new-file
              %next-backup-name new-bak)))))



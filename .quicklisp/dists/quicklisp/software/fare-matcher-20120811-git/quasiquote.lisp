;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;; fare-matcher friendly implementation of Quasiquote
;;; Copyright (c) 2002-2011 Fahree Reedaw <fare@tunes.org>
;;; See README.quasiquote

#+xcvb (module (:depends-on ("packages" "matcher")))

(in-package :fare-quasiquote)

(declaim (optimize (speed 1) (safety 3) (debug 3)))

(eval-when (:compile-toplevel :load-toplevel :execute)
;;;; uncomment some of the lines below to disable according simplifications:
;;(pushnew :quasiquote-quotes-literals *features*)
;;(pushnew :quasiquote-at-macro-expansion-time *features*)

;; the below instruction enables pattern-matching for the simplifier.
(copy-function-matcher
 list  cl:list
 list* cl:list*
 cons  cl:cons
 quote cl:quote
 vector cl:vector)

(defvar *quasiquote-tokens*
  '(unquote unquote-splicing unquote-splicing
    list list* append nconc cons vector n-vector knil))

(make-single-arg-form quote kwote)
(make-single-arg-form quasiquote)
(make-single-arg-form unquote)
(make-single-arg-form unquote-splicing)
(make-single-arg-form unquote-nsplicing)
(defun make-vector-form (&rest x) (list* 'vector x))
(defun vector-form-p (x)
  (and (listp x) #|(alexandria:proper-list-p x)|# (eq (car x) 'vector)))

(defmacro quote (x) (list 'cl:quote x))
(defmacro quasiquote (x) (quasiquote-expand x))
(defmacro unquote (x)
  (declare (ignore x))
  (error "unquote only allowed within quasiquote"))
(defmacro unquote-splicing (x)
  (declare (ignore x))
  (error "unquote-splicing disallowed outside quasiquote"))
(defmacro unquote-nsplicing (x)
  (declare (ignore x))
  (error "unquote-nsplicing disallowed outside quasiquote"))

(define-symbol-matcher knil
  '#'(lambda (form)
       (or (null form)
	   (and (quotep form) (null (single-arg form)))
	   (m%fail))))
(defparameter knil
  #+quasiquote-quotes-literals (kwote nil)
  #-quasiquote-quotes-literals nil)

(defparameter *quasiquote-level* 0
  "current depth of quasiquote nesting")
(defparameter *simplify* t
  "should we simplify backquoted expressions")

(defun unquote-xsplicing-p (x)
  (or (unquote-splicing-p x) (unquote-nsplicing-p x)))

(defun quasiquote-expand (x)
  (let ((*quasiquote-level* 0))
    (multiple-value-bind (top arg)
        (quasiquote-expand-0 x)
      (when (eq top 'unquote-splicing)
        (error ",@ after backquote in ~S" x))
      (when (eq top 'unquote-nsplicing)
        (error ",. after backquote in ~S" x))
      (quasiquote-expand-1 top arg))))

(defun quasiquote-expand-0 (x)
  "Given an expression x under a backquote, return two values:
1- a token identifying a topmost function to apply on
2- an argument
When combining backquoted expressions, tokens are used for simplifications."
  (cond
   ((null x)
    (values nil nil))
   ((literalp x)
    (values #+quasiquote-quotes-literals 'quote #-quasiquote-quotes-literals :literal x))
   ((or (symbolp x) (quotep x))
    (values 'quote x))
   ((unquote-splicing-p x)
    (values 'unquote-splicing (single-arg x)))
   ((unquote-nsplicing-p x)
    (values 'unquote-nsplicing (single-arg x)))
   ((unquotep x)
    (values 'unquote (single-arg x)))
   ((vector-form-p x)
    (multiple-value-bind (top contents) (quasiquote-expand-0 (cdr x))
      (values 'vector (quasiquote-expand-1 top contents))))
   ;;#+quasiquote-at-macro-expansion-time
   ((simple-vector-p x)
    (values 'vector (quasiquote-expand (coerce x 'cl:list))))
   ((quasiquotep x)
    ;; shouldn't be happening unless #+quasiquote-at-macro-expansion-time
    (quasiquote-expand-0 (quasiquote-expand (single-arg x))))
   ((consp x)
    (multiple-value-bind (atop a) (quasiquote-expand-0 (car x))
      (multiple-value-bind (dtop d) (quasiquote-expand-0 (cdr x))
        (when (eq dtop 'unquote-splicing)
          (error ",@ after dot"))
        (when (eq dtop 'unquote-nsplicing)
          (error ",. after dot"))
        (cond
          ((eq atop 'unquote-splicing)
           (if (null dtop)
               (if (unquote-xsplicing-p a)
                   (values 'append (list a))
                   (expand-unquote a))
               (values 'append
                       (cond ((eq dtop 'append)
                              (cons a d))
                             (t (list a (quasiquote-expand-1 dtop d)))))))
          ((eq atop 'unquote-nsplicing)
           (if (null dtop)
               (if (unquote-xsplicing-p a)
                   (values 'nconc (list a))
                   (expand-unquote a))
               (values 'nconc
                       (cond ((eq dtop 'nconc)
                              (cons a d))
                             (t (list a (quasiquote-expand-1 dtop d)))))))
          ((null dtop)
           (if (member atop '(quote :literal nil))
               (values 'quote (list a))
               (values 'list (list (quasiquote-expand-1 atop a)))))
          ((member dtop '(quote :literal))
           (if (member atop '(quote :literal nil))
               (values 'quote (cons a d))
               (values 'list* (list (quasiquote-expand-1 atop a)
                                    (quasiquote-expand-1 dtop d)))))
          (t (let ((qa (quasiquote-expand-1 atop a)))
               (if (member dtop '(list list*))
                   (values dtop (cons qa d))
                   (values 'list*
                           (list qa (quasiquote-expand-1 dtop d))))))))))
   (t
    (error "unrecognized object in quasiquote"))))

(defun expand-unquote (x)
  (cond
    ((null x)
     (values nil nil))
    ((literalp x)
     (values :literal x))
    ((symbolp x)
     (values 'unquote x))
    ((simple-vector-p x) ;; XXX - test this.
     (values 'vector (quasiquote-expand (coerce x 'cl:list))))
    ((not (consp x))
     (error "unrecognized object in unquote"))
    ((and (quotep x)
          (not (unquote-xsplicing-p (single-arg x))))
     (values 'quote (single-arg x)))
    ((member (car x) '(append list list* nconc))
     (values (car x) (cdr x)))
    ((eq (car x) 'cons)
     (values 'list* (cdr x)))
    (t (values 'unquote x))))

(defun quasiquote-expand-1 (top x)
  "Given a top token and an expression, give the quasiquoting
of the result of the top operation applied to the expression"
  (cond
    ((member top '(unquote :literal nil))
     x)
    ((eq top 'quote)
     (kwote x))
    ((eq top 'list*)
     (cond ((and (null (cddr x))
                 (not (unquote-xsplicing-p (car x)))
                 (not (unquote-xsplicing-p (cadr x))))
            (k-cons (car x) (cadr x)))
           ((unquote-xsplicing-p (car (last x)))
            (k-append
             (apply 'k-list (butlast x))
             (car (last x))))
           (t
            (apply 'k-list* x))))
    ((eq top 'vector)
     (k-vector x))
    (t
     (cons (ecase top
             ((list) 'list)
             ((append) 'append)
             ((nconc) 'nconc))
           x))))

; we want our own tokens, but they must evaluate the usual way.
(defsubst list (&rest r) (apply #'cl:list r))
(defsubst append (&rest r) (apply #'cl:append r))
(defsubst list* (&rest r) (apply #'cl:list* r))
(defsubst nconc (&rest r) (apply #'cl:nconc r))
(defsubst cons (x y) (cl:cons x y))
(defsubst vector (&rest r) (apply #'cl:vector r))
(defsubst make-vector (l) (coerce l 'simple-vector))
(defsubst clobberable (x) x) ;; marks x as being unique and clobberable by nconc

(defun k-vector (l) (list 'make-vector l))
(defun k-list (&rest r) (cons 'list r))
(defun k-append (&rest r) (cons 'append r))
(defun k-list* (&rest r) (cons 'list* r))
(defun k-cons (x y) (list 'cons x y))
(defun insert (x) x)

(defun list-extender (c)
  (case c
    ((cons list*) 'list*)
    ((list) 'list)
    (t (error "not a list constructor ~A" c))))

(defun self-evaluating-p (x)
  (or (literalp x)
      (not (or (symbolp x) (combinationp x)
               #|#+quasiquote-at-macro-expansion-time|# (simple-vector-p x)
	       ))))
(defun constant-form-p (x)
  (or #-quasiquote-quotes-literals (self-evaluating-p x)
      (quotep x)))
(defun all-constant-forms-p (l)
  (every #'constant-form-p l))
(defun unfold-constant-form (x)
  (if (quotep x) (single-arg x) x))
(defun unfold-constant-forms (l)
  (mapcar #'unfold-constant-form l))
(defun protect-constant-form (x)
  (if (self-evaluating-p x) x (kwote x)))
(defun protect-constant-forms (l)
  (mapcar #'protect-constant-form l))


(define-macro-matcher quasiquote
  #'(lambda (x) (pattern-matcher (quasiquote-expand x))))

;; Note: it would be a *very bad* idea to use quasiquote:quote
;; in the expansion of the macro-character #\'

(defun call-with-quasiquote-reader (thunk)
  (let ((*quasiquote-level* (1+ *quasiquote-level*)))
    (make-quasiquote (funcall thunk))))

(defun call-with-unquote-reader (thunk)
  (let ((*quasiquote-level* (1- *quasiquote-level*)))
    (unless (>= *quasiquote-level* 0) (error "unquote outside quasiquote"))
    (make-unquote (funcall thunk))))

(defun call-with-unquote-splicing-reader (thunk)
  (let ((*quasiquote-level* (1- *quasiquote-level*)))
    (unless (>= *quasiquote-level* 0) (error "unquote-splicing outside quasiquote"))
    (make-unquote-splicing (funcall thunk))))

(defun call-with-unquote-nsplicing-reader (thunk)
  (let ((*quasiquote-level* (1- *quasiquote-level*)))
    (unless (>= *quasiquote-level* 0) (error "unquote-nsplicing outside quasiquote"))
    (make-unquote-nsplicing (funcall thunk))))

(defun read-quasiquote (stream)
  (call-with-quasiquote-reader (lambda () (read stream t nil t))))

(defun read-unquote (stream)
  (call-with-unquote-reader (lambda () (read stream t nil t))))

(defun read-unquote-splicing (stream)
  (call-with-unquote-splicing-reader (lambda () (read stream t nil t))))

(defun read-unquote-nsplicing (stream)
  (call-with-unquote-nsplicing-reader (lambda () (read stream t nil t))))

(defun n-vector (n contents)
  (if (null n) (coerce contents 'simple-vector)
    (let ((a (make-array n :element-type t)))
      (when (and (null contents) (> n 0))
	(error "non-zero length vector with empty contents"))
      (loop for i below n with x
	do (unless (null contents) (setq x (pop contents)))
	do (setf (aref a i) x))
      (when contents
	(error "provided contents larger than declared vector length"))
      a)))

(defun read-vector (stream n)
  ;; http://www.lisp.org/HyperSpec/Body/sec_2-4-8-3.html
  (let ((contents (read-delimited-list #\) stream t)))
    (if (> *quasiquote-level* 0)
	(make-unquote (list 'n-vector n (quasiquote-expand contents)))
	(n-vector n contents))))

(defun read-read-time-backquote (stream char)
  (declare (ignore char))
  (values (macroexpand-1 (read-quasiquote stream))))
(defun read-macroexpand-time-backquote (stream char)
  (declare (ignore char))
  (read-quasiquote stream))
(defun read-backquote (stream char)
  #-quasiquote-at-macro-expansion-time (read-read-time-backquote stream char)
  #+quasiquote-at-macro-expansion-time (read-macroexpand-time-backquote stream char))
(defun backquote-reader (expansion-time)
  (ecase expansion-time
    ((read) #'read-read-time-backquote)
    ((macroexpand) #'read-macroexpand-time-backquote)
    ((nil) #'read-backquote)))
(defun read-comma (stream char)
  (declare (ignore char))
  (case (peek-char nil stream t nil t)
    ((#\@)
     (read-char stream t nil t)
     (read-unquote-splicing stream))
    ((#\.)
     (read-char stream t nil t)
     (read-unquote-nsplicing stream))
    (otherwise (read-unquote stream))))
(defun read-hash-paren (stream subchar arg)
  (declare (ignore subchar))
  (read-vector stream arg))

(defun enable-quasiquote (&key expansion-time (readtable *readtable*))
  (set-macro-character #\` (backquote-reader expansion-time) nil readtable)
  (set-macro-character #\, #'read-comma nil readtable)
  (when (eq expansion-time 'read)
    (set-dispatch-macro-character #\# #\( #'read-hash-paren readtable))
  t)

);eval-when

;;(trace quasiquote-expand quasiquote-expand-0 quasiquote-expand-1 expand-unquote)


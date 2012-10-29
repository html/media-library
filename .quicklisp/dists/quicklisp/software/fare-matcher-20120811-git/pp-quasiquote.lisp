;;;; pretty-printing of backquote expansions

#+xcvb (module (:depends-on ("quasiquote")))

;;;; This software is derived from the CMU CL system via SBCL.
;;;; CMU CL was written at Carnegie Mellon University and released into
;;;; the public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty.

(in-package :fare-quasiquote)

(defun unparse-quasiquote-1 (form splicing)
  (ecase splicing
    ((nil)
     (list 'unquote form))
    (:append
     (list (list 'unquote-splicing form)))
    (:nconc
     (list (list 'unquote-nsplicing form)))))

(defun unparse-quasiquote (form &optional splicing)
  "Given a lisp form containing the magic functions LIST, LIST*,
  APPEND, etc. produced by the backquote reader macro, will return a
  corresponding backquote input form. In this form, `,' `,@' and `,.' are
  represented by lists whose cars are UNQUOTE, UNQUOTE-SPLICING, and
  UNQUOTE-NSPLICING respectively, and whose cadrs are the form after the comma.
  SPLICING indicates whether a comma-escape return should be modified for
  splicing with other forms: a value of :APPEND or :NCONC meaning that an extra
  level of parentheses should be added."
  (cond
   ((atom form)
    (unparse-quasiquote-1 form splicing))
   ((not (null (cdr (last form))))
    ;; FIXME: this probably throws a recursive error
    (error "found illegal dotted quasiquote form: ~S" form))
   (t
    (case (car form)
      ((list cl:list)
       (mapcar #'unparse-quasiquote (cdr form)))
      ((list* cl:list*)
       (do ((tail (cdr form) (cdr tail))
            (accum nil))
           ((null (cdr tail))
            (nconc (nreverse accum)
                   (unparse-quasiquote (car tail) :append)))
         (push (unparse-quasiquote (car tail)) accum)))
      ((append cl:append)
       (apply #'cl:append
              (mapcar (lambda (el) (unparse-quasiquote el :append))
                      (cdr form))))
      ((nconc cl:nconc)
       (apply #'cl:append
              (mapcar (lambda (el) (unparse-quasiquote el :nconc))
                      (cdr form))))
      ((cons cl:cons)
       (cl:cons (unparse-quasiquote (cadr form) nil)
                (unparse-quasiquote (caddr form) :append)))
      ((vector cl:vector)
       (coerce (unparse-quasiquote (cadr form)) 'cl:vector))
      ((quote cl:quote)
       (cond
         ((atom (cadr form)) (cadr form))
         ((and (consp (cadr form))
               (member (caadr form) *quasiquote-tokens*))
          (unparse-quasiquote-1 form splicing))
         (t (cons (unparse-quasiquote (list 'cl:quote (caadr form)))
                  (unparse-quasiquote (list 'cl:quote (cdadr form)))))))
      (t
       (unparse-quasiquote-1 form splicing))))))

(defun pprint-quasiquote (stream form &rest noise)
  (declare (ignore noise))
  (write-char #\` stream)
  (write (unparse-quasiquote form) :stream stream))

(defun pprint-unquote (stream form &rest noise)
  (declare (ignore noise))
  (ecase (car form)
    ((unquote)
     (write-char #\, stream))
    ((unquote-splicing)
     (write-string ",@" stream))
    ((unquote-nsplicing)
     (write-string ",." stream)))
  (let ((output (with-output-to-string (s)
                  (write (cadr form) :stream s
                         :level (min 1 (or *print-level* 1))
                         :length (min 1 (or *print-length* 1))))))
    (unless (= (length output) 0)
      (when (and (eql (car form) 'unquote)
                 (or (char= (char output 0) #\.)
                     (char= (char output 0) #\@)))
        (write-char #\Space stream))
      (write (cadr form) :stream stream))))

;;; This is called by !PPRINT-COLD-INIT, fairly late, because
;;; SET-PPRINT-DISPATCH doesn't work until the compiler works.
;;;
;;; FIXME: It might be cleaner to just make these be toplevel forms and
;;; enforce the delay by putting this file late in the build sequence.
(defun !backq-pp-cold-init ()
  (set-pprint-dispatch '(cons (eql list)) #'pprint-quasiquote)
  (set-pprint-dispatch '(cons (eql list*)) #'pprint-quasiquote)
  (set-pprint-dispatch '(cons (eql append)) #'pprint-quasiquote)
  (set-pprint-dispatch '(cons (eql nconc)) #'pprint-quasiquote)
  (set-pprint-dispatch '(cons (eql cons)) #'pprint-quasiquote)
  (set-pprint-dispatch '(cons (eql vector)) #'pprint-quasiquote)

  (set-pprint-dispatch '(cons (eql unquote)) #'pprint-unquote)
  (set-pprint-dispatch '(cons (eql unquote-splicing)) #'pprint-unquote)
  (set-pprint-dispatch '(cons (eql unquote-nsplicing)) #'pprint-unquote))

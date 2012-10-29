#+xcvb (module (:depends-on ("packages")))

(in-package :fare-matcher-test)

;; This version of princ allows one to see
;; inside of your implementation's version of quasiquoted expressions...

(defun rprinc (x)
  "hand-made princ that allows to see inside quasiquotes
(results are implementation-dependent)"
  (labels
      ((rprinc-list (x)
         (princ "(")
	 (rprinc-list-contents x)
	 (princ ")"))
       (rprinc-list-contents (x)
         (rprinc (car x))
	 (rprinc-cdr (cdr x)))
       (rprinc-cdr (x)
         (if x (if (consp x)
		   (progn
		     (princ " ")
		     (rprinc-list-contents x))
		 (progn
		   (princ " . ")
		   (rprinc x))))))
    (cond
     ((consp x) (rprinc-list x))
     (t (princ x)))
    x))

;; You can test the quasiquote implementation like this:

(defvar *saved-readtable* *readtable*)
(defparameter *fq-readtable* (copy-readtable *saved-readtable*))
(enable-quasiquote :readtable *fq-readtable*)

(defun fq (s)
  (let ((*readtable* *fq-readtable*))
    (read-from-string s)))

(defparameter b 11)

(deftest test-quasiquote ()
  (macrolet ((q (x y)
               `(is (equal (fq ,x) ',y))))
    (q "``a" (quote (quote a)))
    (q "`(a ,b)" (list (quote a) b))
    (q "``(a ,b)" (quote (list (quote a) b)))
    (q "`(a ,b)" (list (quote a) b))
    (q "`(a ,x ,@y)" (list* (quote a) x y))
    ;;(is (equal (ifmatch `(a ,x ,@y) '(a b c d) (list x y)) '(b (c d))))
    (q "`(1 2 3)" (quote (1 2 3)))
    (q "`(a ,b ,@c .,d)" (list* (quote a) b (append c d)))
    (q "`(,@c .,d)" (append c d))))

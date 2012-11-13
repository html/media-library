(in-package :cl-user)

(defun object->simple-plist (object &rest filters)
  (loop for i in (sb-mop:class-direct-slots (find-class (class-name  (class-of object)))) append 
        (let* ((slot (intern (string (sb-mop:slot-definition-name i)) "KEYWORD"))
               (value (if (slot-boundp object (sb-mop:slot-definition-name i))
                        (slot-value object (sb-mop:slot-definition-name i))
                        "Unbound")))
          (list slot (if (getf filters slot) (funcall (getf filters slot) value) value)))))

(require :prevalence-serialized-i18n)
(prevalence-serialized-i18n::load-data "russian-translation.serialized")

;(print (mapcar #'object->simple-plist prevalence-serialized-i18n::*translations*))
;(print (mapcar #'object->simple-plist prevalence-serialized-i18n::*translation-strings*))

(export '(*potential-strings-to-translate* *translation-function*))
(defvar *potential-strings-to-translate* nil)
(defvar *standard-double-quote-writer* (get-macro-character #\"))
(defvar *packages-to-translate* (list :media-library :weblocks :weblocks-filtering-widget :weblocks-dataseq-records-count-panel :weblocks-twitter-bootstrap-application))
(defvar *packages-used* nil)
(defvar *translation-function* #'prevalence-serialized-i18n::translate)
(defvar *inside-quote-p* nil)

(defun translate (string)
  (funcall *translation-function* string))

(defun globalization-double-quote-reader (stream macro-char)
  (declare (ignore macro-char))
  (flet ((copy-string-until-double-quote 
           (stream out)
           (loop for char = (read-char stream t nil t)
                 while (char/= char #\")
                 do (write-char
                      (cond ((char= char #\\)
                             (let ((next-char (read-char stream t nil t)))
                               (case next-char
                                 (#\t #\Tab)
                                 (#\n #\Newline)
                                 (otherwise next-char))))
                            (t char))
                      out)))
         (package-is-needed-for-translation-p 
           ()
           ;(format t "~%DISPATCH DOUBLE-QUOTE: Checking package ~A, inside qoute ~A~%" (package-name *package*) *inside-quote-p*)
           (find (intern (package-name *package*) "KEYWORD") *packages-to-translate*))
         (package-is-pushed-as-used 
           ()
           (find (package-name *package*) *packages-used* :test #'string=)))

    (let ((out-string))
      (with-output-to-string (out)
        (copy-string-until-double-quote stream out)
        (setf out-string (get-output-stream-string out)))

      (when (string= out-string "Email")
        (error (package-name *package*)))

      (when (not (package-is-needed-for-translation-p))
        (return-from globalization-double-quote-reader out-string))


      (unless (package-is-pushed-as-used)
        (push (package-name *package*) *packages-used*))

      (pushnew out-string *potential-strings-to-translate* :test #'string=)

      ;`(,(intern "TRANSLATE" "WEBLOCKS-STRINGS-TRANSLATION-APP") ,out-string)

      (if (or t *inside-quote-p*)
        (translate out-string)
        `(cl-user::translate ,out-string)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (set-macro-character #\" 'globalization-double-quote-reader))

(let* ((single-quote #\')
       (old-macro-character (get-macro-character single-quote)))
  (set-macro-character single-quote 
                       (lambda (stream macro-char)
                         (let* ((*inside-quote-p* t)
                                (ret (funcall old-macro-character stream macro-char)))
                           ;(format t "Eval quote: ~A~%" (prin1-to-string ret))
                           ret))))

; This could be test
(with-output-to-string (out)
  (let ((*standard-output* out))
    (print "test") 
    (print '("test")))) 

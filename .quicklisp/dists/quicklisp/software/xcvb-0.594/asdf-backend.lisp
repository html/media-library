#+xcvb
(module
 (:compile-depends-on ("simplifying-traversal" "commands")
  :load-depends-on ("simplifying-traversal" "logging" "commands")))

(in-package :xcvb)

(defclass asdf-traversal (simplifying-traversal)
  ())

(defvar *target-builds* (make-hashset :test 'equal)
  "A list of asdf system we supersede")

(defgeneric build-in-target-p (env build))

(defmethod build-in-target-p ((env asdf-traversal) build)
  (declare (ignorable env))
  (gethash (fullname build) *target-builds*))

(defmethod issue-dependency ((env asdf-traversal) (grain lisp-module-grain))
  (if (build-in-target-p env (build-module-grain-for grain))
      (call-next-method)
      (issue-asdf-equivalents env grain (typep grain 'build-module-grain)))
  (values))

(defun grain-asdf-equivalents (grain &optional (build (build-module-grain-for grain)))
  (finalize-grain build)
  (loop :with fname = (fullname grain)
    :with name = (etypecase grain
                   (build-module-grain fname)
                   (lisp-module-grain (second fname)))
    :for (asdf-name xcvb-name) :in (asdf-supersessions build)
    :when (equal xcvb-name name)
    :collect asdf-name))

(defun issue-asdf-equivalents (env grain errorp)
  (let* ((build (build-module-grain-for grain))
         (a (grain-asdf-equivalents grain build)))
    (cond
      (a
       (dolist (s a)
         (pushnew s *asdf-system-dependencies* :test 'equal))
       (values))
      ((equal (fullname build) "/asdf")
       (values)) ;; special case: ASDF is assumed to be there already when using an ASDF
      (errorp
       (error "depending on grain ~A but it has no ASDF equivalent" (fullname build)))
      ((eq build grain)
       (values))
      (t
       (issue-asdf-equivalents env build t)))))

(defmethod graph-for-build-module-grain ((env asdf-traversal) grain)
  (if (build-in-target-p env grain)
    (call-next-method)
    (issue-asdf-equivalents env grain t))
  (values))

(defun write-asd-prelude (s)
  (format s
   ";;; This file was automatically generated by XCVB ~A with the arguments~%~
    ;;;    ~{~A~^ ~}~%~
    ;;; It may have been specialized to the target implementation ~A~%~
    ;;; with the following features:~%~
    ;;;    ~(~S~)~%~%~
   (in-package :asdf)~%~%"
   *xcvb-version* *arguments* *lisp-implementation-type* *features*))

(defun write-asd-file (&key build-names output-path asdf-name)
  "Writes an asd file to OUTPUT-PATH
covering the builds specified by BUILD-NAMES.
Declare asd system as ASDF-NAME."
  (assert (consp build-names))
  (let* ((env (make-instance 'asdf-traversal))
         (*use-cfasls* nil)
         (*asdf-system-dependencies* nil)
         (*require-dependencies* nil)
         (builds (mapcar (lambda (n) (registered-build n :ensure-build t))
                         build-names))
         (first-build (finalize-grain (first builds)))
         (asdf-name
          (coerce-asdf-system-name
           (or asdf-name
               (first (grain-asdf-equivalents first-build))
               (pathname-name (fullname first-build)))))
         (default-output-path
          (subpathname (grain-pathname first-build) (strcat asdf-name ".asd")))
         (output-path
          (if output-path
            (merge-pathnames*
             (ensure-pathname-absolute output-path)
             default-output-path)
            default-output-path))
         (*target-builds* (make-hashset :test 'equal :list (mapcar #'fullname builds))))
    (log-format 6 "T=~A building dependency graph" (get-universal-time))
    (dolist (b builds)
      (graph-for-build-module-grain env b))
    (log-format 6 "T=~A creating asd file ~A" (get-universal-time) output-path)
    (do-write-asd-file env
      :output-path output-path
      :asdf-name asdf-name)))

(defun do-write-asd-file (env &key output-path asdf-name)
  (let* ((output-path (merge-pathnames* output-path))
         (_ (ensure-directories-exist output-path))
         ;; bind *default-pathname-defaults* to the asdf file's directory.
         (*default-pathname-defaults* (pathname-directory-pathname output-path)))
    (declare (ignore _))
    (with-open-file (out output-path :direction :output :if-exists :supersede)
      (write-asd-prelude out)
      (let ((form (make-asdf-form env asdf-name)))
        (with-safe-io-syntax (:package :asdf)
          (let ((*print-case* :downcase))
            (format out "~@[~{(require ~S)~%~}~%~]" (reverse *require-dependencies*))
            (write form :stream out :pretty t :miser-width 79)
            (terpri out)))))))

(defun keywordify-asdf-name (name)
  (kintern "~:@(~A~)" name))

(defgeneric asdf-spec (env grain))
(defmethod asdf-spec (env (grain lisp-file-grain))
  (let* ((namestring (grain-namestring env grain))
         (pathname (pathname namestring))
         (enough (enough-namestring namestring))
         (noext (asdf-dependency-grovel::strip-extension enough "lisp"))
         (around-compile (effective-around-compile grain))
         (encoding (effective-encoding grain)))
    `(:file ,noext
            ,@(when (or (absolute-pathname-p (pathname enough))
                        (not (equal (coerce-pathname
                                     noext :type "lisp" :defaults *default-pathname-defaults*)
                                    pathname)))
                `(:pathname ,pathname))
            ,@(when around-compile
                `(:around-compile ,around-compile))
            ,@(unless (eq encoding :utf-8)
                `(:encoding ,encoding)))))
(defmethod asdf-spec (env (grain source-grain))
  `(:static-file ,(enough-namestring (grain-namestring env grain))))

(defmethod asdf-spec (env (build build-module-grain))
  (declare (ignorable env build))
  ;; should that be an error?
  nil)

(defun make-asdf-form (env asdf-name)
  ;; we can assume computations is topologically sorted.
  ;; TODO: ASDF is stupid, so we should try to optimize dependencies by removing extra ones:
  ;; for each dependency of current node, starting with the most recent one,
  ;; add the dependency and remove all those that it includes.
  ;; NOTE: we assume *default-pathname-defaults* is set to the destination directory
  ;; for the asdf file.
  `(asdf:defsystem ,(keywordify-asdf-name asdf-name)
     :depends-on ,(mapcar 'keywordify-asdf-name (reverse *asdf-system-dependencies*))
     :encoding :utf-8
     :components ,(loop :with visited = (make-hash-table :test 'equal)
                    :for computation :in (reverse *computations*)
                    :for lisp = (first (computation-inputs computation))
                    :for deps = (rest (computation-inputs computation))
                    :for spec = (and lisp (asdf-spec env lisp))
                    :for build = (and spec (build-module-grain-for lisp))
                    :for includedp = (and build (build-in-target-p env build))
                    :for depends-on = (remove-duplicates
                                       (loop :for dep :in deps
                                         :for dspec = (asdf-spec env dep)
                                         :when (and dspec (typep dep 'lisp-file-grain)
                                                    (build-in-target-p env (build-module-grain-for dep)))
                                         :collect (second dspec))
                                       :test #'equal)
                    :for already-visited = (gethash spec visited)
                    :do (setf (gethash spec visited) t)
                    :when (and includedp (not already-visited)) :collect
                    `(,@spec ,@(when depends-on `(:depends-on ,depends-on))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; XCVB to ASDF ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-command xcvb-to-asdf-command
    (("xcvb-to-asdf" "x2a")
     (&rest keys &key)
     `(,@+multi-build-option-spec+
       (("name" #\n) :type string :optional t :documentation "name of the new ASDF system")
       (("output-path" #\o) :type string :optional t :documentation "pathname for the new ASDF system")
       ,@+source-registry-option-spec+
       ,@+lisp-implementation-option-spec+
       ,@+verbosity-option-spec+)
     "Extract an ASDF system from XCVB"
     "Automatically extract an ASDF system from one or many XCVB builds."
     (build name output-path))
  (apply 'handle-global-options keys)
  (write-asd-file
   :asdf-name name
   :build-names (mapcar #'canonicalize-fullname build)
   :output-path output-path))

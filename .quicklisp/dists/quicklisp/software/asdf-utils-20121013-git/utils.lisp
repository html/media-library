#+xcvb (module (:depends-on ("package")))

(in-package :asdf-utils)

(macrolet
    ((defdef (def* def)
       `(defmacro ,def* (name formals &rest rest)
          `(progn
             #+(or ecl (and gcl (not gcl-pre2.7))) (fmakunbound ',name)
             #-gcl ; gcl 2.7.0 notinline functions lose secondary return values :-(
             ,(when (and #+ecl (symbolp name)) ; fails for setf functions on ecl
                `(declaim (notinline ,name)))
             (,',def ,name ,formals ,@rest)))))
  (defdef defgeneric* defgeneric)
  (defdef defun* defun))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun* find-symbol* (s p)
    (find-symbol (string s) p)))

(defun* strcat (&rest strings)
  (apply 'concatenate 'string strings))

(defmacro while-collecting ((&rest collectors) &body body)
  "COLLECTORS should be a list of names for collections.  A collector
defines a function that, when applied to an argument inside BODY, will
add its argument to the corresponding collection.  Returns multiple values,
a list for each collection, in order.
   E.g.,
\(while-collecting \(foo bar\)
           \(dolist \(x '\(\(a 1\) \(b 2\) \(c 3\)\)\)
             \(foo \(first x\)\)
             \(bar \(second x\)\)\)\)
Returns two values: \(A B C\) and \(1 2 3\)."
  (let ((vars (mapcar #'(lambda (x) (gensym (symbol-name x))) collectors))
        (initial-values (mapcar (constantly nil) collectors)))
    `(let ,(mapcar #'list vars initial-values)
       (flet ,(mapcar #'(lambda (c v) `(,c (x) (push x ,v) (values))) collectors vars)
         ,@body
         (values ,@(mapcar #'(lambda (v) `(reverse ,v)) vars))))))

(defun* pathname-directory-pathname (pathname)
  "Returns a new pathname with same HOST, DEVICE, DIRECTORY as PATHNAME,
and NIL NAME, TYPE and VERSION components"
  (when pathname
    (make-pathname :name nil :type nil :version nil :defaults pathname)))

(defun* normalize-pathname-directory-component (directory)
  "Given a pathname directory component, return an equivalent form that is a list"
  (cond
    #-(or cmu sbcl scl) ;; these implementations already normalize directory components.
    ((stringp directory) `(:absolute ,directory) directory)
    #+gcl
    ((and (consp directory) (stringp (first directory)))
     `(:absolute ,@directory))
    ((or (null directory)
         (and (consp directory) (member (first directory) '(:absolute :relative))))
     directory)
    (t
     (error "Unrecognized pathname directory component ~S" directory))))

(defun* merge-pathname-directory-components (specified defaults)
  ;; Helper for merge-pathnames* that handles directory components.
  (let ((directory (normalize-pathname-directory-component specified)))
    (ecase (first directory)
      ((nil) defaults)
      (:absolute specified)
      (:relative
       (let ((defdir (normalize-pathname-directory-component defaults))
             (reldir (cdr directory)))
         (cond
           ((null defdir)
            directory)
           ((not (eq :back (first reldir)))
            (append defdir reldir))
           (t
            (loop :with defabs = (first defdir)
              :with defrev = (reverse (rest defdir))
              :while (and (eq :back (car reldir))
                          (or (and (eq :absolute defabs) (null defrev))
                              (stringp (car defrev))))
              :do (pop reldir) (pop defrev)
              :finally (return (cons defabs (append (reverse defrev) reldir)))))))))))

(defun* make-pathname-component-logical (x)
  "Make a pathname component suitable for use in a logical-pathname"
  (typecase x
    ((eql :unspecific) nil)
    #+clisp (string (string-upcase x))
    #+clisp (cons (mapcar 'make-pathname-component-logical x))
    (t x)))

(defun* make-pathname-logical (pathname host)
  "Take a PATHNAME's directory, name, type and version components,
and make a new pathname with corresponding components and specified logical HOST"
  (make-pathname
   :host host
   :directory (make-pathname-component-logical (pathname-directory pathname))
   :name (make-pathname-component-logical (pathname-name pathname))
   :type (make-pathname-component-logical (pathname-type pathname))
   :version (make-pathname-component-logical (pathname-version pathname))))

(defun* merge-pathnames* (specified &optional (defaults *default-pathname-defaults*))
  "MERGE-PATHNAMES* is like MERGE-PATHNAMES except that
if the SPECIFIED pathname does not have an absolute directory,
then the HOST and DEVICE both come from the DEFAULTS, whereas
if the SPECIFIED pathname does have an absolute directory,
then the HOST and DEVICE both come from the SPECIFIED.
Also, if either argument is NIL, then the other argument is returned unmodified."
  (when (null specified) (return-from merge-pathnames* defaults))
  (when (null defaults) (return-from merge-pathnames* specified))
  #+scl
  (ext:resolve-pathname specified defaults)
  #-scl
  (let* ((specified (pathname specified))
         (defaults (pathname defaults))
         (directory (normalize-pathname-directory-component (pathname-directory specified)))
         (name (or (pathname-name specified) (pathname-name defaults)))
         (type (or (pathname-type specified) (pathname-type defaults)))
         (version (or (pathname-version specified) (pathname-version defaults))))
    (labels ((unspecific-handler (p)
               (if (typep p 'logical-pathname) #'make-pathname-component-logical #'identity)))
      (multiple-value-bind (host device directory unspecific-handler)
          (ecase (first directory)
            ((:absolute)
             (values (pathname-host specified)
                     (pathname-device specified)
                     directory
                     (unspecific-handler specified)))
            ((nil :relative)
             (values (pathname-host defaults)
                     (pathname-device defaults)
                     (merge-pathname-directory-components directory (pathname-directory defaults))
                     (unspecific-handler defaults))))
        (make-pathname :host host :device device :directory directory
                       :name (funcall unspecific-handler name)
                       :type (funcall unspecific-handler type)
                       :version (funcall unspecific-handler version))))))

(defun* pathname-parent-directory-pathname (pathname)
  "Returns a new pathname with same HOST, DEVICE, DIRECTORY as PATHNAME,
and NIL NAME, TYPE and VERSION components"
  (when pathname
    (make-pathname :name nil :type nil :version nil
                   :directory (merge-pathname-directory-components
                               '(:relative :back) (pathname-directory pathname))
                   :defaults pathname)))

(define-modify-macro appendf (&rest args)
  append "Append onto list") ;; only to be used on short lists.

(define-modify-macro orf (&rest args)
  or "or a flag")

(defun* first-char (s)
  (and (stringp s) (plusp (length s)) (char s 0)))

(defun* last-char (s)
  (and (stringp s) (plusp (length s)) (char s (1- (length s)))))

(defun* split-string (string &key max (separator '(#\Space #\Tab)))
  "Split STRING into a list of components separated by
any of the characters in the sequence SEPARATOR.
If MAX is specified, then no more than max(1,MAX) components will be returned,
starting the separation from the end, e.g. when called with arguments
 \"a.b.c.d.e\" :max 3 :separator \".\" it will return (\"a.b.c\" \"d\" \"e\")."
  (catch nil
    (let ((list nil) (words 0) (end (length string)))
      (flet ((separatorp (char) (find char separator))
             (done () (throw nil (cons (subseq string 0 end) list))))
        (loop
          :for start = (if (and max (>= words (1- max)))
                           (done)
                           (position-if #'separatorp string :end end :from-end t)) :do
          (when (null start)
            (done))
          (push (subseq string (1+ start) end) list)
          (incf words)
          (setf end start))))))

(defun* split-name-type (filename)
  (let ((unspecific
         ;; Giving :unspecific as argument to make-pathname is not portable.
         ;; See CLHS make-pathname and 19.2.2.2.3.
         ;; We only use it on implementations that support it,
         #+(or abcl allegro clozure cmu gcl genera lispworks mkcl sbcl scl xcl) :unspecific
         #+(or clisp ecl #|These haven't been tested:|# cormanlisp mcl) nil))
    (destructuring-bind (name &optional (type unspecific))
        (split-string filename :max 2 :separator ".")
      (if (equal name "")
          (values filename unspecific)
          (values name type)))))

(defun* component-name-to-pathname-components (s &key force-directory force-relative)
  "Splits the path string S, returning three values:
A flag that is either :absolute or :relative, indicating
   how the rest of the values are to be interpreted.
A directory path --- a list of strings, suitable for
   use with MAKE-PATHNAME when prepended with the flag
   value.
A filename with type extension, possibly NIL in the
   case of a directory pathname.
FORCE-DIRECTORY forces S to be interpreted as a directory
pathname \(third return value will be NIL, final component
of S will be treated as part of the directory path.

The intention of this function is to support structured component names,
e.g., \(:file \"foo/bar\"\), which will be unpacked to relative
pathnames."
  (check-type s string)
  (when (find #\: s)
    (error "A portable ASDF pathname designator cannot include a #\: character: ~S" s))
  (let* ((components (split-string s :separator "/"))
         (last-comp (car (last components))))
    (multiple-value-bind (relative components)
        (if (equal (first components) "")
            (if (equal (first-char s) #\/)
                (progn
                  (when force-relative
                    (error "Absolute pathname designator not allowed: ~S" s))
                  (values :absolute (cdr components)))
                (values :relative nil))
          (values :relative components))
      (setf components (remove-if #'(lambda (x) (member x '("" ".") :test #'equal)) components))
      (setf components (substitute :back ".." components :test #'equal))
      (cond
        ((equal last-comp "")
         (values relative components nil)) ; "" already removed
        (force-directory
         (values relative components nil))
        (t
         (values relative (butlast components) last-comp))))))

(defun* remove-keys (key-names args)
  (loop :for (name val) :on args :by #'cddr
    :unless (member (symbol-name name) key-names
                    :key #'symbol-name :test 'equal)
    :append (list name val)))

(defun* remove-keyword (key args)
  (loop :for (k v) :on args :by #'cddr
    :unless (eq k key)
    :append (list k v)))

(defun* getenv (x)
  (declare (ignorable x))
  #+(or abcl clisp ecl xcl) (ext:getenv x)
  #+allegro (sys:getenv x)
  #+clozure (ccl:getenv x)
  #+(or cmu scl) (cdr (assoc x ext:*environment-list* :test #'string=))
  #+cormanlisp
  (let* ((buffer (ct:malloc 1))
         (cname (ct:lisp-string-to-c-string x))
         (needed-size (win:getenvironmentvariable cname buffer 0))
         (buffer1 (ct:malloc (1+ needed-size))))
    (prog1 (if (zerop (win:getenvironmentvariable cname buffer1 needed-size))
               nil
               (ct:c-string-to-lisp-string buffer1))
      (ct:free buffer)
      (ct:free buffer1)))
  #+gcl (system:getenv x)
  #+genera nil
  #+lispworks (lispworks:environment-variable x)
  #+mcl (ccl:with-cstrs ((name x))
          (let ((value (_getenv name)))
            (unless (ccl:%null-ptr-p value)
              (ccl:%get-cstring value))))
  #+mkcl (#.(or (find-symbol* 'getenv :si) (find-symbol* 'getenv :mk-ext)) x)
  #+sbcl (sb-ext:posix-getenv x)
  #-(or abcl allegro clisp clozure cmu cormanlisp ecl gcl genera lispworks mcl mkcl sbcl scl xcl)
  (error "~S is not supported on your implementation" 'getenv))

(defun* directory-pathname-p (pathname)
  "Does PATHNAME represent a directory?

A directory-pathname is a pathname _without_ a filename. The three
ways that the filename components can be missing are for it to be NIL,
:UNSPECIFIC or the empty string.

Note that this does _not_ check to see that PATHNAME points to an
actually-existing directory."
  (when pathname
    (let ((pathname (pathname pathname)))
      (flet ((check-one (x)
               (member x '(nil :unspecific "") :test 'equal)))
        (and (not (wild-pathname-p pathname))
             (check-one (pathname-name pathname))
             (check-one (pathname-type pathname))
             t)))))

(defun* ensure-directory-pathname (pathspec)
  "Converts the non-wild pathname designator PATHSPEC to directory form."
  (cond
   ((stringp pathspec)
    (ensure-directory-pathname (pathname pathspec)))
   ((not (pathnamep pathspec))
    (error "Invalid pathname designator ~S" pathspec))
   ((wild-pathname-p pathspec)
    (error "Can't reliably convert wild pathname ~S" pathspec))
   ((directory-pathname-p pathspec)
    pathspec)
   (t
    (make-pathname :directory (append (or (pathname-directory pathspec)
                                          (list :relative))
                                      (list (file-namestring pathspec)))
                   :name nil :type nil :version nil
                   :defaults pathspec))))

(defun* absolute-pathname-p (pathspec)
  (and (typep pathspec '(or pathname string))
       (eq :absolute (car (pathname-directory (pathname pathspec))))))

(defun* coerce-pathname (name &key type defaults)
  "coerce NAME into a PATHNAME.
When given a string, portably decompose it into a relative pathname:
#\\/ separates subdirectories. The last #\\/-separated string is as follows:
if TYPE is NIL, its last #\\. if any separates name and type from from type;
if TYPE is a string, it is the type, and the whole string is the name;
if TYPE is :DIRECTORY, the string is a directory component;
if the string is empty, it's a directory.
Any directory named .. is read as :BACK.
Host, device and version components are taken from DEFAULTS."
  ;; The defaults are required notably because they provide the default host
  ;; to the below make-pathname, which may crucially matter to people using
  ;; merge-pathnames with non-default hosts,  e.g. for logical-pathnames.
  ;; NOTE that the host and device slots will be taken from the defaults,
  ;; but that should only matter if you later merge relative pathnames with
  ;; CL:MERGE-PATHNAMES instead of ASDF:MERGE-PATHNAMES*
  (etypecase name
    ((or null pathname)
     name)
    (symbol
     (coerce-pathname (string-downcase name) :type type :defaults defaults))
    (string
     (multiple-value-bind (relative path filename)
         (component-name-to-pathname-components name :force-directory (eq type :directory)
                                                :force-relative t)
       (multiple-value-bind (name type)
           (cond
             ((or (eq type :directory) (null filename))
              (values nil nil))
             (type
              (values filename type))
             (t
              (split-name-type filename)))
         (apply 'make-pathname :directory (cons relative path) :name name :type type
                (when defaults `(:defaults ,defaults))))))))

(defun* merge-component-name-type (name &key type defaults)
  ;; For backwards compatibility only, for people using internals.
  ;; Will be removed in a future release, e.g. 2.016.
  (warn "Please don't use ASDF::MERGE-COMPONENT-NAME-TYPE. Use ASDF:COERCE-PATHNAME.")
  (coerce-pathname name :type type :defaults defaults))

(defun* subpathname (pathname subpath &key type)
  (and pathname (merge-pathnames* (coerce-pathname subpath :type type)
                                  (pathname-directory-pathname pathname))))

(defun* subpathname* (pathname subpath &key type)
  (and pathname
       (subpathname (ensure-directory-pathname pathname) subpath :type type)))

(defun* length=n-p (x n) ;is it that (= (length x) n) ?
  (check-type n (integer 0 *))
  (loop
    :for l = x :then (cdr l)
    :for i :downfrom n :do
    (cond
      ((zerop i) (return (null l)))
      ((not (consp l)) (return nil)))))

(defun* string-suffix-p (s suffix)
  (check-type s string)
  (check-type suffix string)
  (let ((start (- (length s) (length suffix))))
    (and (<= 0 start)
         (string-equal s suffix :start1 start))))

(defun* read-file-forms (file)
  (with-open-file (in file)
    (loop :with eof = (list nil)
     :for form = (read in nil eof)
     :until (eq form eof)
     :collect form)))

(defun* pathname-root (pathname)
  (make-pathname :directory '(:absolute)
                 :name nil :type nil :version nil
                 :defaults pathname ;; host device, and on scl, *some*
                 ;; scheme-specific parts: port username password, not others:
                 . #.(or #+scl '(:parameters nil :query nil :fragment nil))))

(defun* probe-file* (p)
  "when given a pathname P, probes the filesystem for a file or directory
with given pathname and if it exists return its truename."
  (etypecase p
    (null nil)
    (string (probe-file* (parse-namestring p)))
    (pathname (unless (wild-pathname-p p)
                #.(or #+(or allegro clozure cmu cormanlisp ecl lispworks mkcl sbcl scl)
                      '(probe-file p)
                      #+clisp (aif (find-symbol* '#:probe-pathname :ext)
                                   `(ignore-errors (,it p)))
                      '(ignore-errors (truename p)))))))

(defun* truenamize (pathname &optional (defaults *default-pathname-defaults*))
  "Resolve as much of a pathname as possible"
  (block nil
    (when (typep pathname '(or null logical-pathname)) (return pathname))
    (let ((p (merge-pathnames* pathname defaults)))
      (when (typep p 'logical-pathname) (return p))
      (let ((found (probe-file* p)))
        (when found (return found)))
      (unless (absolute-pathname-p p)
        (let ((true-defaults (ignore-errors (truename defaults))))
          (when true-defaults
            (setf p (merge-pathnames pathname true-defaults)))))
      (unless (absolute-pathname-p p) (return p))
      (let ((sofar (probe-file* (pathname-root p))))
        (unless sofar (return p))
        (flet ((solution (directories)
                 (merge-pathnames*
                  (make-pathname :host nil :device nil
                                 :directory `(:relative ,@directories)
                                 :name (pathname-name p)
                                 :type (pathname-type p)
                                 :version (pathname-version p))
                  sofar)))
          (loop :with directory = (normalize-pathname-directory-component
                                   (pathname-directory p))
            :for component :in (cdr directory)
            :for rest :on (cdr directory)
            :for more = (probe-file*
                         (merge-pathnames*
                          (make-pathname :directory `(:relative ,component))
                          sofar)) :do
            (if more
                (setf sofar more)
                (return (solution rest)))
            :finally
            (return (solution nil))))))))

(defun* resolve-symlinks (path)
  #-allegro (truenamize path)
  #+allegro (if (typep path 'logical-pathname)
                path
                (excl:pathname-resolve-symbolic-links path)))

(defun* ensure-pathname-absolute (path)
  (cond
    ((absolute-pathname-p path) path)
    ((stringp path) (ensure-pathname-absolute (pathname path)))
    ((not (pathnamep path)) (error "not a valid pathname designator ~S" path))
    (t (let ((resolved (resolve-symlinks path)))
         (assert (absolute-pathname-p resolved))
         resolved))))

(defparameter *wild* #-cormanlisp :wild #+cormanlisp "*")
(defparameter *wild-file*
  (make-pathname :name *wild* :type *wild*
                 :version (or #-(or abcl xcl) *wild*) :directory nil))
(defparameter *wild-directory*
  (make-pathname :directory `(:relative ,*wild*) :name nil :type nil :version nil))
(defparameter *wild-inferiors*
  (make-pathname :directory '(:relative :wild-inferiors) :name nil :type nil :version nil))
(defparameter *wild-path*
  (merge-pathnames *wild-file* *wild-inferiors*))

(defun* wilden (path)
  (merge-pathnames* *wild-path* path))

#-scl
(defun* directory-separator-for-host (&optional (pathname *default-pathname-defaults*))
  (let ((foo (make-pathname :directory '(:absolute "FOO") :defaults pathname)))
    (last-char (namestring foo))))

#-scl
(defun* directorize-pathname-host-device (pathname)
  (let* ((root (pathname-root pathname))
         (wild-root (wilden root))
         (absolute-pathname (merge-pathnames* pathname root))
         (separator (directory-separator-for-host root))
         (root-namestring (namestring root))
         (root-string
          (substitute-if #\/
                         #'(lambda (x) (or (eql x #\:)
                                           (eql x separator)))
                         root-namestring)))
    (multiple-value-bind (relative path filename)
        (component-name-to-pathname-components root-string :force-directory t)
      (declare (ignore relative filename))
      (let ((new-base
             (make-pathname :defaults root
                            :directory `(:absolute ,@path))))
        (translate-pathname absolute-pathname wild-root (wilden new-base))))))

#+scl
(defun* directorize-pathname-host-device (pathname)
  (let ((scheme (ext:pathname-scheme pathname))
        (host (pathname-host pathname))
        (port (ext:pathname-port pathname))
        (directory (pathname-directory pathname)))
    (flet ((specificp (x) (and x (not (eq x :unspecific)))))
      (if (or (specificp port)
              (and (specificp host) (plusp (length host)))
              (specificp scheme))
        (let ((prefix ""))
          (when (specificp port)
            (setf prefix (format nil ":~D" port)))
          (when (and (specificp host) (plusp (length host)))
            (setf prefix (strcat host prefix)))
          (setf prefix (strcat ":" prefix))
          (when (specificp scheme)
            (setf prefix (strcat scheme prefix)))
          (assert (and directory (eq (first directory) :absolute)))
          (make-pathname :directory `(:absolute ,prefix ,@(rest directory))
                         :defaults pathname)))
    pathname)))

(defun* featurep (x &optional (features *features*))
  (cond
    ((atom x)
     (and (member x features) t))
    ((eq :not (car x))
     (assert (null (cddr x)))
     (not (featurep (cadr x) features)))
    ((eq :or (car x))
     (some #'(lambda (x) (featurep x features)) (cdr x)))
    ((eq :and (car x))
     (every #'(lambda (x) (featurep x features)) (cdr x)))
    (t
     (error "Malformed feature specification ~S" x))))

(defun* os-unix-p ()
  (featurep '(:or :unix :cygwin :darwin)))

(defun* os-windows-p ()
  (and (not (os-unix-p)) (featurep '(:or :win32 :windows :mswindows :mingw32))))

(defun* find-class* (x &optional (errorp t) environment)
  (etypecase x
    ((or standard-class built-in-class) x)
    (symbol (find-class x errorp environment))))

(defun* hostname ()
  ;; Note: untested on RMCL
  #+(or abcl clozure cmucl ecl genera lispworks mcl mkcl sbcl scl xcl) (machine-instance)
  #+cormanlisp "localhost" ;; is there a better way? Does it matter?
  #+allegro (excl.osi:gethostname)
  #+clisp (first (split-string (machine-instance) :separator " "))
  #+gcl (system:gethostname))

;;; ---------------------------------------------------------------------------
;;; Generic support for configuration files

(defun* inter-directory-separator ()
  (if (os-unix-p) #\: #\;))

(defun* user-homedir ()
  (truenamize
   (pathname-directory-pathname
    #+cormanlisp (ensure-directory-pathname (user-homedir-pathname))
    #+mcl (current-user-homedir-pathname)
    #-(or cormanlisp mcl) (user-homedir-pathname))))

(defun* ensure-pathname* (x want-absolute want-directory fmt &rest args)
  (when (plusp (length x))
    (let ((p (if want-directory (ensure-directory-pathname x) (pathname x))))
      (when want-absolute
        (unless (absolute-pathname-p p)
          (cerror "ignore relative pathname"
                  "Invalid relative pathname ~A~@[ ~?~]" x fmt args)
          (return-from ensure-pathname* nil)))
      p)))
(defun* split-pathnames* (x want-absolute want-directory fmt &rest args)
  (loop :for dir :in (split-string
                      x :separator (string (inter-directory-separator)))
        :collect (apply 'ensure-pathname* dir want-absolute want-directory fmt args)))
(defun* getenv-pathname (x &key want-absolute want-directory &aux (s (getenv x)))
  (ensure-pathname* s want-absolute want-directory "from (getenv ~S)" x))
(defun* getenv-pathnames (x &key want-absolute want-directory &aux (s (getenv x)))
  (and (plusp (length s))
       (split-pathnames* s want-absolute want-directory "from (getenv ~S) = ~S" x s)))
(defun* getenv-absolute-directory (x)
  (getenv-pathname x :want-absolute t :want-directory t))
(defun* getenv-absolute-directories (x)
  (getenv-pathnames x :want-absolute t :want-directory t))

(defun* hidden-file-p (pathname)
  (equal (first-char (pathname-name pathname)) #\.))

(defun* directory* (pathname-spec &rest keys &key &allow-other-keys)
  (apply 'directory pathname-spec
         (append keys '#.(or #+allegro '(:directories-are-files nil :follow-symbolic-links nil)
                             #+clozure '(:follow-links nil)
                             #+clisp '(:circle t :if-does-not-exist :ignore)
                             #+(or cmu scl) '(:follow-links nil :truenamep nil)
                             #+sbcl (when (find-symbol* :resolve-symlinks '#:sb-impl)
                                      '(:resolve-symlinks nil))))))

(defun* delete-file-if-exists (x)
  (when (and x (probe-file* x))
    (delete-file x)))

(defun* filter-logical-directory-results (directory entries merger)
  (if (typep directory 'logical-pathname)
      ;; Try hard to not resolve logical-pathname into physical pathnames;
      ;; otherwise logical-pathname users/lovers will be disappointed.
      ;; If directory* could use some implementation-dependent magic,
      ;; we will have logical pathnames already; otherwise,
      ;; we only keep pathnames for which specifying the name and
      ;; translating the LPN commute.
      (loop :for f :in entries
        :for p = (or (and (typep f 'logical-pathname) f)
                     (let* ((u (ignore-errors (funcall merger f))))
                       ;; The first u avoids a cumbersome (truename u) error.
                       ;; At this point f should already be a truename,
                       ;; but isn't quite in CLISP, for doesn't have :version :newest
                       (and u (equal (ignore-errors (truename u)) (truename f)) u)))
        :when p :collect p)
      entries))

(defun* directory-files (directory &optional (pattern *wild-file*))
  (let ((dir (pathname directory)))
    (when (typep dir 'logical-pathname)
      ;; Because of the filtering we do below,
      ;; logical pathnames have restrictions on wild patterns.
      ;; Not that the results are very portable when you use these patterns on physical pathnames.
      (when (wild-pathname-p dir)
        (error "Invalid wild pattern in logical directory ~S" directory))
      (unless (member (pathname-directory pattern) '(() (:relative)) :test 'equal)
        (error "Invalid file pattern ~S for logical directory ~S" pattern directory))
      (setf pattern (make-pathname-logical pattern (pathname-host dir))))
    (let ((entries (ignore-errors (directory* (merge-pathnames* pattern dir)))))
      (filter-logical-directory-results
       directory entries
       #'(lambda (f)
           (make-pathname :defaults dir
                          :name (make-pathname-component-logical (pathname-name f))
                          :type (make-pathname-component-logical (pathname-type f))
                          :version (make-pathname-component-logical (pathname-version f))))))))

(defun* subdirectories (directory)
  (let* ((directory (ensure-directory-pathname directory))
         #-(or abcl cormanlisp genera xcl)
         (wild (merge-pathnames*
                #-(or abcl allegro cmu lispworks sbcl scl xcl)
                *wild-directory*
                #+(or abcl allegro cmu lispworks sbcl scl xcl) "*.*"
                directory))
         (dirs
          #-(or abcl cormanlisp genera xcl)
          (ignore-errors
            (directory* wild . #.(or #+clozure '(:directories t :files nil)
                                     #+mcl '(:directories t))))
          #+(or abcl xcl) (system:list-directory directory)
          #+cormanlisp (cl::directory-subdirs directory)
          #+genera (fs:directory-list directory))
         #+(or abcl allegro cmu genera lispworks sbcl scl xcl)
         (dirs (loop :for x :in dirs
                 :for d = #+(or abcl xcl) (extensions:probe-directory x)
                          #+allegro (excl:probe-directory x)
                          #+(or cmu sbcl scl) (directory-pathname-p x)
                          #+genera (getf (cdr x) :directory)
                          #+lispworks (lw:file-directory-p x)
                 :when d :collect #+(or abcl allegro xcl) d
                                  #+genera (ensure-directory-pathname (first x))
                                  #+(or cmu lispworks sbcl scl) x)))
    (filter-logical-directory-results
     directory dirs
     (let ((prefix (or (normalize-pathname-directory-component (pathname-directory directory))
                       '(:absolute)))) ; because allegro returns NIL for #p"FOO:"
       #'(lambda (d)
           (let ((dir (normalize-pathname-directory-component (pathname-directory d))))
             (and (consp dir) (consp (cdr dir))
                  (make-pathname
                   :defaults directory :name nil :type nil :version nil
                   :directory (append prefix (make-pathname-component-logical (last dir)))))))))))

(defun* get-folder-path (folder)
  (or ;; this semi-portably implements a subset of the functionality of lispworks' sys:get-folder-path
   #+(and lispworks mswindows) (sys:get-folder-path folder)
   ;; read-windows-registry HKEY_LOCAL_MACHINE\SOFTWARE\Microsoft\Windows\CurrentVersion\Explorer\User Shell Folders\AppData
   (ecase folder
    (:local-appdata (getenv-absolute-directory "LOCALAPPDATA"))
    (:appdata (getenv-absolute-directory "APPDATA"))
    (:common-appdata (or (getenv-absolute-directory "ALLUSERSAPPDATA")
			 (subpathname* (getenv-absolute-directory "ALLUSERSPROFILE") "Application Data/"))))))

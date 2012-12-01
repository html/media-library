(load "src/weblocks-filtering-widget-updates.lisp")
(load "src/prevalence-ids-from-1-updates.lisp")
(in-package :weblocks-twitter-bootstrap-application)


(defmethod render-widget-body ((obj flash) &rest args)
  (declare (special *on-ajax-complete-scripts* *dirty-widgets*))
  (let ((messages (weblocks::flash-messages-to-show obj)))
    (when messages
      (with-html
	(:div :class "view row"
	      (with-extra-tags
		(htm
		 (:div :class "messages span8 clearfix"
		      (mapc (lambda (msg)
                      (htm (:div :class "alert" 
                            (:button :type "button" :class "close" :data-dismiss "alert" "x")
                            (:strong (apply #'render-widget msg args)))))
			    messages))))))
      (send-script (ps* `((@ ($ ,(dom-id obj)) show)))))))

(defun render-button (name  &key (value (prevalence-serialized-i18n:translate (humanize-name name))) id (class "submit btn"))
  "Renders a button in a form.

   'name' - name of the html control. The name is attributized before
   being rendered.
   'value' - a value on html control. Humanized name is default.
   'id' - id of the html control. Default is nil.
   'class' - a class used for styling. By default, \"submit\"."
  (with-html
    (:input :name (attributize-name name) :type "submit" :id id :class class
     :value value :onclick "disableIrrelevantButtons(this);")
    (str "&nbsp;")))

(in-package :weblocks)

(defun update-dialog-on-request ()
  "This callback function is called by 'handle-client-request'. If a
request is a refresh and a dialog was shown, appropriate JS is
inserted into the page to redraw the dialog."
  (let ((current-dialog (current-dialog)))
    (when (and current-dialog
	       (refresh-request-p))
      (with-javascript
        (ps* `(funcall j-query
                       (lambda ()
                         ,(make-dialog-js (dialog-title current-dialog)
                                          (dialog-widget current-dialog)
                                          (dialog-css-class current-dialog)
                                          (dialog-close current-dialog)))))))))

(defmethod handle-client-request ((app weblocks-webapp))
  (progn				;save it for splitting this up
    (when (null *session*)
      (when (get-request-action-name)
	(expired-action-handler app))
      (start-session)
      (setf (webapp-session-value 'last-request-uri) :none)
      (when *rewrite-for-session-urls*
        (redirect (request-uri*))))
    (when *maintain-last-session*
      (bordeaux-threads:with-lock-held (*maintain-last-session*)
	(setf *last-session* *session*)))
    (let ((*request-hook* (make-instance 'request-hooks))
          *dirty-widgets*)
      (when (null (root-widget))
	(let ((root-widget (make-instance 'widget :name "root")))
	  (setf (root-widget) root-widget)
	  (let (finished?)
	    (unwind-protect
		 (progn
                   (handler-bind ((error (lambda (c) 
                                           (warn "Error initializing user session: ~A" c)
                                           (when *backtrace-on-session-init-error*
                                             (format t "~%~A~%" (print-trivial-backtrace c)))
                                           (signal c))))
                       (funcall (webapp-init-user-session) root-widget))
		   (setf finished? t))
	      (unless finished?
		(setf (root-widget) nil)
		(reset-webapp-session))))
	  (push 'update-dialog-on-request (request-hook :session :post-action)))
	(when (and *rewrite-for-session-urls*
                   (cookie-in (session-cookie-name *weblocks-server*)))
	  (redirect (remove-session-from-uri (request-uri*)))))

      (let ((*weblocks-output-stream* (make-string-output-stream))
	    (*uri-tokens* (make-instance 'uri-tokens :tokens (tokenize-uri (request-uri*))))
	    *before-ajax-complete-scripts*
            *on-ajax-complete-scripts*
	    *page-dependencies*
            *current-page-title*
            *current-page-description*
            *current-page-keywords*
            *current-page-headers*
	    (cl-who::*indent* (weblocks-webapp-html-indent-p app)))
	(declare (special *weblocks-output-stream*
                          *dirty-widgets*
			  *on-ajax-complete-scripts*
                          *uri-tokens*
                          *page-dependencies*
                          *current-page-title*
                          *current-page-description*
                          *current-page-keywords*
                          *current-page-headers*))
	(when (pure-request-p)
	  (abort-request-handler (eval-action))) ; FIXME: what about the txn hook?

        (webapp-update-thread-status "Processing action")
        (timing "action processing (w/ hooks)"
          (eval-hook :pre-action)
          (with-dynamic-hooks (:dynamic-action)
            (eval-action))
          (eval-hook :post-action))

	(when (and (not (ajax-request-p))
		   (find *action-string* (get-parameters*)
			 :key #'car :test #'string-equal))
	  (redirect (remove-action-from-uri (request-uri*))))

        (timing "rendering (w/ hooks)"
          (eval-hook :pre-render)
          (with-dynamic-hooks (:dynamic-render)
            (if (ajax-request-p)
              (handle-ajax-request app)
              (handle-normal-request app)))
          (eval-hook :post-render))

        (if (member (return-code*) *approved-return-codes*)
          (progn 
            (unless (ajax-request-p)
              (setf (webapp-session-value 'last-request-uri) (all-tokens *uri-tokens*)))
            (get-output-stream-string *weblocks-output-stream*))
          (handle-http-error app (return-code*)))))))

(defun/cc do-dialog (title callee &key css-class close)
  (declare (special *on-ajax-complete-scripts*))
  "Presents 'callee' to the user in a modal dialog, saves the
continuation, and returns from the delimited computation. When
'callee' answers, removes the modal interface and reactives the
computation. If the modal interface isn't available, automatically
scales down to 'do-modal' instead."
  (assert (stringp title))
  (if (ajax-request-p)
      (prog2
	  (when (current-dialog)
	    (error "Multiple dialogs not allowed."))
	  (call callee (lambda (new-callee)
			 (setf (current-dialog) (make-dialog :title title
							     :widget new-callee
							     :close close
							     :css-class css-class))
                         (send-script (ps* (make-dialog-js title new-callee css-class close)) :before-load)))
	(setf (current-dialog) nil)
        (send-script (ps (remove-dialog))))
      (do-modal title callee :css-class css-class)))

(defun last-composition-id ()
  (or (car (sort (mapcar #'weblocks:object-id (weblocks-utils:all-of 'media-library::composition)) #'>)) 0))

(defmethod parse-view-field-value ((parser file-upload-parser) value obj
   (view form-view) (field form-view-field) &rest args)
  (declare (ignore args))
  (when (null value)
    (return-from parse-view-field-value (values t nil)))
  (when (stringp value)
    (error "The value of the upload field is incorrect. Please turn on
    multipart requests and turn off ajax."))
  (flet ((octet-string->utf-8 (s)
           "Kludge to fix librfc2388 bug."
           (hunchentoot::octets-to-string
             (map 'vector
                  (lambda (c)
                    (let ((x (char-int c)))
                      (assert (and (not (minusp x))
                                   (< x 256)))
                      x))
                  s)))
         (fix-cyrillic-file-name (s)
           (babel:octets-to-string 
             (babel:string-to-octets s :encoding :latin1)
             :encoding :utf-8)))
    (let* ((temp-path (first value))
           (browser-name (fix-cyrillic-file-name (octet-string->utf-8 (second value))))
           (file-name (etypecase (file-upload-parser-file-name parser)
                        (symbol (ecase (file-upload-parser-file-name parser)
                                  (:browser browser-name)
                                  (:unique (concatenate 'string 
                                                        (hunchentoot::create-random-string)
                                                        (or (cl-ppcre:scan-to-strings "\\..*$" browser-name) 
                                                            "")))
                                  ; XXX there is small possibility for two files with same id if they are created during one moment
                                  (:browser-with-cyrillic-transliteration 
                                    (media-library::transform-file-name-for-media-library 
                                      browser-name 
                                      (1+ (last-composition-id))))))
                        (string (file-upload-parser-file-name parser)))))
      (copy-file temp-path
        (merge-pathnames file-name
                         (file-upload-parser-upload-directory parser))
        :if-exists :supersede)
      (values t value file-name))))

(in-package :media-library)

(log:config :daily "pub/log.txt")
(setf weblocks:*max-raw-input-length* 500)

(defmacro with-yaclml (&body body)
  "A wrapper around cl-yaclml with-yaclml-stream macro."
  `(yaclml:with-yaclml-stream *weblocks-output-stream*
     ,@body))

;; Define callback function to initialize new sessions
(defun get-upload-directory ()
  (merge-pathnames 
    (make-pathname :directory '(:relative "upload"))
    (compute-webapp-public-files-path (weblocks:get-webapp 'media-library))))

(defun get-file-id3-info (file)
  (with-output-to-string (s)
    (external-program:run "/bin/sh" (list "script/get-id3-tags-info" file ) :output s)
    s))

(defvar *http-auth-config-file*  "conf/http-auth.conf")

(defun http-auth-user ()
  (handler-case 
    (with-open-file (in *http-auth-config-file*)
      (let ((user (read-line in))) 
        (when (> (length user) 0)
          user)))
    (end-of-file () nil)))

(defun http-auth-password ()
  (with-open-file (in *http-auth-config-file*)
    (read-line in)
    (read-line in)))

(defun export-rss ()
  (unless (multiple-value-bind (user password) (hunchentoot:authorization)
            (or (not (http-auth-user))
                (and
                  (http-auth-password)
                  (string= (http-auth-user) user)
                  (string= (http-auth-password) password))))
    (hunchentoot:require-authorization))
  (setf (hunchentoot:header-out :content-type) "text/xml")
  (let ((app-domain  "http://contentchaos.com"))
    (write-string
    (cl-who:with-html-output-to-string (s nil :prologue "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>")
      (:rss :version "2.0" :|xmlns:atom| "http://www.w3.org/2005/Atom"
       (:channel (:title "Mp3 chaos")
        (:link (esc app-domain))
        (:|atom:link| :href "http://contentchaos.com/feed.rss" :rel "self" :type "application/rss+xml")
        (:description "Content chaos")
        (loop for model in (weblocks-utils:all-of 'composition)
              do (let ((file-url (format nil "~A~A" app-domain (composition-file-url model))))
                   (htm (:item (:title (esc (slot-value model 'file)))
                               (:link (esc file-url))
                               (:guid (esc file-url))
                               (:description (esc (composition-text model)))
                               (:enclosure :url file-url :length (with-open-file (in (composition-file-name model)) (file-length in)) :type "audio/mp3")
                               (when (composition-created-at-rfc-822 model)
                                 (str (format nil "<pubDate>~A</pubDate>" (composition-created-at-rfc-822 model)))))))))))
    weblocks:*weblocks-output-stream*)))

(push 
  (hunchentoot:create-regex-dispatcher 
    "^/feed\\.rss" 
    #'export-rss)
  weblocks::*dispatch-table*)

(defun replace-search-values (str)
  (let ((widget (first (get-widgets-by-type 'weblocks-filtering-widget::custom-filtering-widget))))
    (when widget 
      (let ((value-to-replace (getf (getf (slot-value widget 'weblocks-filtering-widget::filters) :value) :compare-value)))
        (when (stringp value-to-replace)
          (setf str (cl-ppcre:regex-replace-all (cl-ppcre:create-scanner 
                                                  (format nil "(~A)" (cl-ppcre:quote-meta-chars value-to-replace))
                                                  :case-insensitive-mode t) str "<b style=\"background-color:#808080;\">\\1</b>")))))
    str))

(defun make-library-grid ()
  (make-instance 'library-grid 
                 :data-class 'composition 
                 :on-add-item (lambda (grid item)
                                (eval `(log:info ,(format nil "Added composition id ~A file ~A by user ~A" (weblocks:object-id item) (slot-value item 'file) (current-user-name)))))
                 :on-delete-items-completed (lambda (grid ids)
                                    (declare (ignore grid))
                                    (eval `(log:info ,(format nil "Deleted compositions ~A by user ~A" ids (current-user-name)))))
                 :view (defview nil (:type table)
                                (id :present-as html 
                                    :order-by 'id
                                    :reader (lambda (item)
                                              (format nil "#~A" (weblocks:object-id item))))
                                (cached-artist 
                                  :present-as html 
                                  :label "Artist"
                                  :order-by 'cached-artist
                                  :reader (lambda (item)
                                            (replace-search-values (composition-cached-artist item))))
                                (cached-track-title 
                                  :present-as html 
                                  :label "Track title"
                                  :order-by 'cached-track-title
                                  :reader (lambda (item)
                                            (replace-search-values (composition-cached-track-title item))))
                                (text 
                                  :label "Text"
                                  :present-as html 
                                  :order-by 'text
                                  :reader (lambda (item)
                                            (replace-search-values (composition-text item))))
                                (cached-bit-rate 
                                  :present-as html 
                                  :label "BR"
                                  :order-by 'cached-bit-rate
                                  :reader (lambda (item)
                                            (replace-search-values (composition-cached-bit-rate item))))
                                (created-at :present-as (date :format "%d.%m.%Y %H:%M") 
                                            :label "Created at"
                                            :order-by 'item-created-at
                                            :reader (lambda (item)
                                                      (slot-value item 'item-created-at)))
                                (updated-at :present-as (date :format "%d.%m.%Y %H:%M") 
                                            :label "Updated at"
                                            :order-by 'item-updated-at
                                            :reader (lambda (item)
                                                      (slot-value item 'item-updated-at))))
                 :item-form-view (library-grid-form-view t)))

(defview login-view (:type form :persistp nil
                              :inherit-from 'default-login-view
                              :buttons '((:submit . "Login") (:cancel . "Cancel"))
                              :caption "Login"
                              :focusp t)
         (password 
           :label "Password"
           :requiredp t
           :required-indicator nil
           :present-as (password :max-length 40)
           :writer (lambda (pwd obj)
                     (setf (slot-value obj 'password)
                           (hash-password pwd)))))

(defun login-successfull-p (email password)
  (when (and 
          (string= email "test@spamavert.com")
          (string= password (weblocks:hash-password "test"))) 
    (setf (%current-user) (list :ok t :name "Administrator"))
    (eval `(log:info , (format nil "Logged in as ~A from ip ~A" (current-user-name) (hunchentoot:remote-addr*))))
    t))

(defun/cc show-login-form (&rest args)
  (do-login (lambda (login-widget object) 
              (login-successfull-p 
                (slot-value object 'email)
                (slot-value object 'password)))
            :view 'login-view))

(defun/cc admin-page (&rest args)
  (let* ((grid (make-library-grid))
         (records-count-widget 
           (make-instance 
             'weblocks-dataseq-records-count-panel:dataseq-records-count-panel 
             :dataseq-instance grid))
         (filtering-widget (make-instance 
                             'weblocks-filtering-widget::custom-filtering-widget 
                             :dataseq-instance grid
                             :form-fields (list 
                                            (list 
                                              :id :any-field
                                              :caption "Any field"
                                              :accessor #'identity)
                                            (list 
                                              :id :text
                                              :caption "Text"
                                              :accessor #'composition-text)
                                            (list 
                                              :id :file-name
                                              :caption "File Name"
                                              :accessor #'composition-file)
                                            (list 
                                              :id :artist
                                              :caption "Artist"
                                              :accessor #'composition-cached-artist)
                                            (list 
                                              :id :track-title
                                              :caption "Track title"
                                              :accessor #'composition-cached-track-title)))))
            (when (show-login-form)
              (do-page 
                (list 
                  (lambda (&rest args)
                    (render-dependency-in-page-head (make-instance 'stylesheet-dependency :url "/pub/stylesheets/style.css"))
                    (render-link 
                      (make-action (lambda (&rest args)
                                     (eval 
                                       `(log:info 
                                          ,(format nil 
                                                   "Logged out as ~A from ip ~A" 
                                                   (current-user-name)
                                                   (hunchentoot::remote-addr*))))
                                     (setf (%current-user) nil)
                                     (init-user-session (root-widget)))) "Logout"
                      :class "logout")
                    (with-yaclml 
                      (<:h2 "Compositions")))
                  filtering-widget
                  records-count-widget
                  grid 
                  (lambda (&rest args)
                    (with-yaclml 
                      (<:div 
                        (<:h2 "Logs (" (<:a :href "/pub/log.txt" :target "_blank" "open in new tab") ")")
                        (<:iframe :style "width:100%;height:100px;" :src "/pub/log.txt")))))))))

(defun init-user-session (comp)
  (setf (composite-widgets comp)
        (list 
          (lambda (&rest args)
            (with-html 
              (:br))
            (render-link 
              #'admin-page
              "Admin"
              :class "btn btn-primary btn-large")))))

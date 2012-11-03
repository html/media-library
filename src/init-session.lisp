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

(in-package :media-library)

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

(defun export-rss ()
  (write-string 
    (cl-who:with-html-output-to-string (s nil :prologue "<?xml version=\"1.0\" encoding=\"ISO-8859-1\" ?>")
      (:rss :version "2.0" :|xmlns:atom| "http://www.w3.org/2005/Atom"
       (:channel (:title "Mp3 chaos")
        (:link "http://contentchaos.com")
        (:|atom:link| :href "http://contentchaos.com/feed.rss" :rel "self" :type "application/rss+xml")
        (:description "Content chaos")
        (loop for model in (weblocks-utils:all-of 'composition)
              do (htm (:item (:title (str (slot-value model 'file)))
                       (:link (str (composition-file-url model)))
                       (:guid  (str (object-id model)))
                       (:description (str (composition-text model)))))))))
    weblocks:*weblocks-output-stream*))

(push 
  (hunchentoot:create-regex-dispatcher 
    "^/feed\\.rss" 
    #'export-rss)
  weblocks::*dispatch-table*)

(defun make-library-grid ()
  (make-instance 'library-grid 
                 :data-class 'composition 
                 :view (defview nil (:type table :inherit-from '(:scaffold composition))
                                (file-name :present-as text :reader (lambda (item)
                                                                      (slot-value item 'file)))
                                (artist :present-as text :reader #'composition-artist)
                                (track-title :present-as text :reader #'composition-track-title))
                 :item-form-view (defview nil (:type form :inherit-from '(:scaffold composition)
                                                 :enctype "multipart/form-data"
                                                 :use-ajax-p nil)
                                            (text 
                                              :requiredp t
                                              :present-as textarea 
                                              :satisfies (lambda (item)
                                                           (or 
                                                             (<= (length item) 160)
                                                             (values nil "Should be less then 160 characters"))))
                                            (textarea-initialization 
                                              :label ""
                                              :present-as html 
                                              :reader (lambda (&rest args)
                                                        (yaclml:with-yaclml-output-to-string 
                                                          (<:br)
                                                          (<:script :type "text/javascript"
                                                                    (<:as-is 
                                                                      (ps:ps 
                                                                        (unless document.textareachanged
                                                                          (setf document.textareachanged t)
                                                                          (with-scripts "/pub/scripts/jquery.textareaCounter.plugin.js" 
                                                                                      (lambda ()
                                                                                        (ps:chain 
                                                                                          (j-query "textarea")
                                                                                          (textarea-count 
                                                                                            (ps:create 
                                                                                              max-character-size 160 
                                                                                              display-format "#input Characters | #left Characters Left"))))))))))))
                                            (mp3-preview 
                                              :present-as html
                                              :reader (lambda (item)
(with-slots (file) item
  (when file 
    (yaclml:with-yaclml-output-to-string
  (<:div :style "float:left;"
         (<:as-is (format nil "
<object type='application/x-shockwave-flash' data='http://flash-mp3-player.net/medias/player_mp3_maxi.swf' width='200' height='20'>
    <param name='movie' value='http://flash-mp3-player.net/medias/player_mp3_maxi.swf' />
    <param name='bgcolor' value='#ffffff' />
    <param name='FlashVars' value='mp3=~a&amp;showvolume=1' />
</object>
                  " (composition-file-url item)))))))))
                  (mp3-id3-data :present-as html 
                                :reader (lambda (item)
                                          (cl-ppcre:regex-replace-all "\\n"
                                                                      (get-file-id3-info (composition-file-name item))
                                                                      "<br/>")))
                  (file :present-as file-upload 
                        :parse-as (file-upload 
                                    :upload-directory (get-upload-directory)
                                    :file-name :browser)
                        :requiredp t
                        :satisfies (lambda (item)
                                     (or 
                                       (string= "mp3" (string-downcase (pathname-type item)))
                                       (values nil "You can only upload mp3 files")))))))

(defview login-view (:type form :persistp nil
                              :inherit-from 'default-login-view
                              :buttons '((:submit . "Login") :cancel)
                              :caption "Login"
                              :focusp t)
         (password :requiredp t
                   :present-as (password :max-length 40)
                   :writer (lambda (pwd obj)
                             (setf (slot-value obj 'password)
                                   (hash-password pwd)))))
(defmacro %current-user ()
  `(webapp-session-value 'current-user))

(defun login-successfull-p (email password)
  (when (and 
          (string= email "test@spamavert.com")
          (string= password (weblocks:hash-password "test"))) 
    (setf (%current-user) (list :ok t))))

(defun/cc show-login-form (&rest args)
  (do-login (lambda (login-widget object) 
              (login-successfull-p 
                (slot-value object 'email)
                (slot-value object 'password)))
            :view 'login-view))

(defun/cc admin-page (&rest args)
  (let ((grid (make-library-grid)))
    (when (show-login-form)
      (do-page 
        (list (make-instance 
                'weblocks-filtering-widget:filtering-widget 
                :dataseq-instance grid
                :form-fields (list 
                               (list 
                                 :id :text
                                 :caption "Text"
                                 :accessor #'composition-text)))
              grid)))))

(defun init-user-session (comp)
  (setf (composite-widgets comp)
        (list 
          (lambda (&rest args)
            (render-link 
              #'admin-page
              "Admin")))))

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

(defun admin-page (&rest args)
  (do-page 
    (make-instance 'gridedit :data-class 'composition 
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
    <param name='FlashVars' value='mp3=/pub/upload/~a&amp;showvolume=1' />
</object>
                  " file))))))))
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
                                                                 (values nil "You can only upload mp3 files"))))))))

(defun init-user-session (comp)
  (setf (composite-widgets comp)
        (list 
          (lambda (&rest args)
            (render-link 
              #'admin-page
              "Admin")))))
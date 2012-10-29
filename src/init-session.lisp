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


;; Define callback function to initialize new sessions
(defun get-upload-directory ()
  (merge-pathnames 
    (make-pathname :directory '(:relative "upload"))
    (compute-webapp-public-files-path (weblocks:get-webapp 'media-library))))

(defun admin-page (&rest args)
  (do-page 
    (make-instance 'gridedit :data-class 'composition 
                   :item-form-view (defview nil (:type form :inherit-from '(:scaffold composition)
                                                 :enctype "multipart/form-data"
                                                 :use-ajax-p nil)
                                            (text :present-as textarea)
                                            (file :present-as file-upload 
                                                  :parse-as (file-upload 
                                                              :upload-directory (get-upload-directory)
                                                              :file-name :unique))))))

(defun init-user-session (comp)
  (setf (composite-widgets comp)
        (list 
          (lambda (&rest args)
            (render-link 
              #'admin-page
              "Admin")))))

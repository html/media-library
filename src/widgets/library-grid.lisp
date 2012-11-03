(in-package :media-library)


(defwidget library-grid (gridedit)
  ())

(defun get-total-compositions-size-used ()
  (with-output-to-string (s)
    (external-program:run "/bin/sh" (list "script/get-disc-size-taken") :output s)
    s))

; Copied from .quicklisp/local-projects/weblocks/src/widgets/datagrid/datagrid.lisp
(defun render-total-items-message (obj)
  "Renders the total items message."
  (with-html
    (:span :class "total-items"
	   (str (let ((total-items-count (dataseq-data-count obj)))
		  (format nil "Total of <b>~A ~A</b>, disk size taken ~A"
			  total-items-count
			  (proper-number-form total-items-count
					      (humanize-name
					       (dataseq-data-class obj)))
        (get-total-compositions-size-used)))))))

; Copied from .quicklisp/local-projects/weblocks/src/widgets/datagrid/datagrid.lisp
(defmethod dataseq-render-mining-bar ((obj datagrid) &rest args)
  (with-html
    (:div :class "data-mining-bar"
	  (when (dataseq-show-total-items-count-p obj)
	    (render-total-items-message obj))
	  (when (dataseq-allow-select-p obj)
	    (apply #'weblocks::render-select-bar obj args)))))

(defmacro library-grid-form-view (file-field-required-p)
  `(defview nil (:type form :inherit-from '(:scaffold composition)
                 :enctype "multipart/form-data"
                 :use-ajax-p nil)
            (cached-artist :present-as hidden :writer (lambda (&rest args) (declare (ignore args))))
            (cached-track-title :present-as hidden :writer (lambda (&rest args) (declare (ignore args))))
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
                                                                        :file-name :browser-with-cyrillic-transliteration)
                                                            :writer (lambda (value item)
                                                                      (when value 
                                                                        (setf (slot-value item 'file) value)
                                                                        (setf (slot-value item 'cached-artist) (composition-artist item))
                                                                        (setf (slot-value item 'cached-track-title) (composition-track-title item))))
                                                            :requiredp ,file-field-required-p
                                                            :satisfies (lambda (item)
                                                                         (if item
                                                                           (or 
                                                                             (string= "mp3" (string-downcase (pathname-type item)))
                                                                             (values nil "You can only upload mp3 files"))
                                                                           t)))))

(defmethod dataedit-create-drilldown-widget ((grid gridedit) item)
  (make-instance 'dataform
		 :data item
		 :class-store (dataseq-class-store grid)
		 :ui-state (if (eql (gridedit-drilldown-type grid) :edit)
			       :form
			       :data)
		 :on-success (lambda (obj)
			       (declare (ignore obj))
                   (eval `(log:info ,(format nil "Updated item with id ~A by user ~A" (weblocks:object-id (dataform-data obj)) (current-user-name))))
			       (flash-message (dataseq-flash grid)
					      (format nil "Modified ~A."
						      (humanize-name (dataseq-data-class grid))))
			       (if (eql (gridedit-drilldown-type grid) :edit)
                                   (progn
                                     (dataedit-reset-state grid)
                                     (throw 'annihilate-dataform nil))
				   (mark-dirty grid)))
		 :on-cancel (when (eql (gridedit-drilldown-type grid) :edit)
			      (lambda (obj)
				(declare (ignore obj))
				(dataedit-reset-state grid)
                                (throw 'annihilate-dataform nil)))
		 :on-close (lambda (obj)
			     (declare (ignore obj))
			     (dataedit-reset-state grid))
		 :data-view (dataedit-item-data-view grid)
		 :form-view (library-grid-form-view nil)))

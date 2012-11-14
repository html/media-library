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
    (:span :class "total-items pull-right"
	   (str (let ((total-items-count (dataseq-data-count obj)))
		  (format nil "Total of <b>~A ~A</b>, disk size taken <b>~A</b>"
			  total-items-count
              (proper-number-form total-items-count
                                  "Composition")
        (get-total-compositions-size-used)))))))

; Copied from .quicklisp/local-projects/weblocks/src/widgets/datagrid/datagrid.lisp
(defmethod dataseq-render-mining-bar ((obj library-grid) &rest args)
  (with-html
    (:div :class "data-mining-bar"
	  (when (dataseq-show-total-items-count-p obj)
	    (render-total-items-message obj))
	  (when (dataseq-allow-select-p obj)
	    (apply #'weblocks::render-select-bar obj args)))))

(defmacro library-grid-form-view (file-field-required-p &optional display-edit-fields-p)
  `(defview nil (:type form :inherit-from '(:scaffold composition)
                 :enctype "multipart/form-data"
                 :use-ajax-p nil 
                 :buttons '((:submit . "Submit") (:cancel . "Cancel")))
            (item-updated-at :present-as hidden :writer (lambda (value item)
                                                          (setf (slot-value item 'item-updated-at) (get-universal-time))))
            ,@(if display-edit-fields-p
                '((cached-artist :label "Artist" :present-as input) 
                  (cached-track-title :label "Track Title" :present-as input))
                '((cached-artist :label "" :present-as hidden :writer (lambda (&rest args) (declare (ignore args))))
                  (cached-track-title :label "" :present-as hidden :writer (lambda (&rest args) (declare (ignore args)))))) 
            (cached-sound-type :present-as hidden :writer (lambda (&rest args) (declare (ignore args))))
            (cached-bit-rate :present-as hidden :writer (lambda (&rest args) (declare (ignore args))))
            (text 
              :label "Text"
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
            ,@(when display-edit-fields-p 
                '((mp3-preview 
                    :label "Mp3 Preview"
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
                                                            (mp3-id3-data 
                                                              :label "Mp3 Id3 Data"
                                                              :present-as html 
                                                              :reader (lambda (item)
                                                                        (cl-ppcre:regex-replace-all "\\n"
                                                                                                    (get-file-id3-info (composition-file-name item))
                                                                                                    "<br/>"))))) 
                                                      (file 
                                                        :label "File"
                                                        :present-as file-upload 
                                                        :parse-as (file-upload 
                                                                    :upload-directory (get-upload-directory)
                                                                    :file-name :browser-with-cyrillic-transliteration)
                                                        :writer (lambda (value item)
                                                                  (when value 
                                                                    (setf (slot-value item 'file) value)
                                                                    (setf (slot-value item 'cached-artist) (composition-artist item))
                                                                    (setf (slot-value item 'cached-track-title) (composition-track-title item))
                                                                    (setf (slot-value item 'cached-bit-rate) (composition-bit-rate item))
                                                                    (setf (slot-value item 'cached-sound-type) (composition-sound-type item))))
                                                        :requiredp ,file-field-required-p
                                                        :satisfies (lambda (item)
                                                                     (if item
                                                                       (or 
                                                                         (string= "mp3" (string-downcase (pathname-type item)))
                                                                         (values nil "You can only upload mp3 files"))
                                                                       t)))))

(defmethod dataedit-create-drilldown-widget ((grid library-grid) item)
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
		 :form-view (library-grid-form-view nil t)))

(defmethod render-dataseq-body :around ((obj library-grid) &rest args)
  (with-html 
    (:div :style "float:right;position:relative;"
     (:div :style "position:absolute;right:0;bottom:20px"
      (when (and (dataseq-allow-operations-p obj)
                 (or (dataseq-item-ops obj)
                     (dataseq-common-ops obj)))
        (apply #'dataseq-render-operations obj args)))))
  (call-next-method))

(defmethod with-table-view-body-row ((view table-view) obj (widget library-grid) &rest args
				     &key alternp &allow-other-keys)
  (if (and (dataseq-allow-drilldown-p widget)
	   (dataseq-on-drilldown widget))
      (let ((row-action (make-action
			 (lambda (&rest args)
			   (declare (ignore args))
			   (when (dataseq-autoset-drilled-down-item-p widget)
			     (setf (dataseq-drilled-down-item widget) obj))
			   (funcall (cdr (dataseq-on-drilldown widget)) widget obj))))
	    (drilled-down-p (and (dataseq-drilled-down-item widget)
				 (eql (object-id (dataseq-drilled-down-item widget))
				      (object-id obj)))))
	(safe-apply (sequence-view-row-prefix-fn view) view obj args)
	(with-html
    (:tr :class (when (or alternp drilled-down-p)
                  (concatenate 'string
                               (when alternp "altern")
                               (when (and alternp drilled-down-p) " ")
                               (when drilled-down-p "drilled-down")))
         (apply #'render-table-view-body-row view obj widget :row-action row-action args)
         (:td :style "white-space:nowrap;"
           (when (not drilled-down-p)
             (render-link row-action "<i class=\"icon-pencil\"></i>" :class "btn btn-small btn-info")))))
	(safe-apply (sequence-view-row-suffix-fn view) view obj args))
      (call-next-method)))

(defmethod with-table-view-header-row ((view table-view) obj widget &rest args)
  (safe-apply (table-view-header-row-prefix-fn view) view obj args)
  (with-html
    (:tr (apply #'render-table-view-header-row view obj widget args)
     (:th "")))
  (safe-apply (table-view-header-row-suffix-fn view) view obj args))

(defmethod render-widget-body :around ((obj library-grid) &rest args)
  (declare (ignore args))
  (dataedit-update-operations obj)
  (call-next-method)
  (when (dataedit-item-widget obj)
    (with-html 
      (:div :style "position:absolute;left:0;top:0;"
       (:div :style "position:relative"
        (:div :class "modal" :style "width:800px;margin-left:-400px;top:10px;margin-top:0;bottom:10px;margin-bottom:0;"
         (:div :class "modal-body" :style "height:100%;max-height:100%;"
          (render-widget (dataedit-item-widget obj)))
         (with-javascript 
           (ps:ps 
             (ps:chain (j-query ".modal") (modal))
             (ps:chain (j-query ".modal-backdrop") (unbind "click") (bind "click" (lambda () nil)))))))))))

(defmethod dataedit-create-new-item-widget ((grid library-grid))
    (make-instance 'dataform
     :data (make-instance (dataseq-data-form-class grid))
     :class-store (dataseq-class-store grid)
     :ui-state :form
     :on-cancel (lambda (obj)
		  (declare (ignore obj))
		  (dataedit-reset-state grid)
		  (throw 'annihilate-dataform nil))
     :on-success (lambda (obj)
                   (safe-funcall (dataedit-on-add-item grid) grid (dataform-data obj))
                   (safe-funcall (dataedit-on-add-item-completed grid) grid (dataform-data obj))
                   (when (or (dataedit-mixin-flash-message-on-first-add-p grid)
                             (> (dataseq-data-count grid) 1))
                     (flash-message (dataseq-flash grid)
                                    (format nil "Added ~A."
                                            (humanize-name (dataseq-data-class grid)))))
                   (dataedit-reset-state grid)
                   (setf (dataseq-sort grid) (cons 'item-created-at :DESC))
                   (throw 'annihilate-dataform nil))
     :data-view (dataedit-item-data-view grid)
     :form-view (dataedit-item-form-view grid)))

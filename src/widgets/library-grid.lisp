(in-package :media-library)

(defvar *max-chars-for-description* 1200)

(defwidget library-grid (gridedit)
  ())

(defconstant +1.5-mb-in-bytes+ 1572864)
(defconstant +1-mb-in-bytes+ 1048576)

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

(defun file-size(file)
  (with-open-file (f file) 
    (file-length f)))

(defun bytes->megabytes (size)
  (float (round-to (/ size 1024 1024) 2)))

(defvar *text-2-placeholder* "{{track-url}}")

(defun replace-urls-in-text (text value)
  (cl-ppcre:regex-replace-all *text-2-placeholder* text value))

; Be careful, function is used in other validation
(defun text-field-satisfies-p(item)
  (setf item (normalize-newlines item))
  (or 
    (<= (length item) *max-chars-for-description*)
    (values nil (format nil "Should be less than ~A characters" *max-chars-for-description*))))

(defun text-2-field-satisfies-p (value)
  (let ((upload-filename (webapp-session-value 'upload-filename)))
    (if (not upload-filename)
      (values nil :validate-later)  
      (progn 
        (delete-webapp-session-value 'upload-filename)
        (cond 
          ((equal upload-filename t) t)
          ((not (ppcre:scan *text-2-placeholder* value)) 
           (progn 
             (values nil 
                     (format nil "Should include ~A~A~A text for urls displaying" 
                             (code-char 171) ; left angle quote
                             *text-2-placeholder* 
                             (code-char 187) ;right angle quote
                             ))))
          (t
           (setf value 
                 (replace-urls-in-text 
                   value
                   (composition-file-full-url-by-filename 
                     upload-filename)))
           (text-field-satisfies-p value)))))))

(defun file-field-satisfies (item)
  (unless (webapp-session-value 'upload-filename)
    (setf (webapp-session-value 'upload-filename) t))
  (cond 
    ((not item) t)
    ((let ((size (file-size (merge-pathnames item (get-upload-directory)))))
       (> size +1.5-mb-in-bytes+)) 
     (values nil "Size is too big, it should be less than 1.5 megabytes. Less than 1 megabyte would be great"))
    (item
      (or 
        (prog1 
          (string= "mp3" (string-downcase (pathname-type item)))
          (setf (webapp-session-value 'upload-filename) item))
        (values nil "You can only upload mp3 files")))))

(load "src/weblocks-table-view-with-ordered-fields.lisp")

(defclass advanced-form-view (form-view)
  ((fields-order :initarg :fields-order :initform nil)))

(defclass advanced-form-view-field (form-view-field)
  ())

(defclass advanced-form-scaffold (table-scaffold)
  ())

(load "src/weblocks-bootstrap-date-entry-presentation.lisp")

(defmacro library-grid-form-view (file-field-required-p &optional display-edit-fields-p)
  `(defview nil (:type advanced-form :inherit-from '(:scaffold composition)
                 :enctype "multipart/form-data"
                 :use-ajax-p t 
                 :buttons '((:submit . "Submit") (:cancel . "Cancel"))
                 :fields-order '(:text :text-2 :pub-date :archived-p :textarea-initialization :file-size :mp3-preview :file))
            (item-updated-at :present-as hidden 
                             :writer (lambda (value item)
                                       (setf (slot-value item 'item-updated-at) (get-universal-time))))
            (item-archive-at 
              :present-as hidden 
              :writer (lambda (value item)
                        (set-archive-time item)))
            (pub-date 
              :label "Publication date"
              :present-as bootstrap-date-entry 
              :reader (lambda(item)
                        (or 
                          (slot-value item 'pub-date)
                          (get-universal-time)))
              :parse-as bootstrap-date)
            (archived-p :present-as hidden)
            (cached-artist :label "Artist" :present-as input) 
            (cached-track-title :label "Track Title" :present-as input)
            (cached-sound-type :present-as hidden :writer (lambda (&rest args) (declare (ignore args))))
            (cached-bit-rate :present-as hidden :writer (lambda (&rest args) (declare (ignore args))))
            (text 
              :label "Text"
              :requiredp t
              :present-as textarea 
              :satisfies #'text-field-satisfies-p)
            (textarea-initialization 
              :label ""
              :present-as html 
              :reader (lambda (&rest args)
                        (yaclml:with-yaclml-output-to-string 
                          (<:script :type "text/javascript"
                                    (<:as-is 
                                      (ps:ps 
                                        (with-scripts 
                                          "/pub/scripts/bootstrap-limit.js" 
                                          (lambda ()
                                            (unless (ps:chain (j-query ".text textarea") (siblings ".text-counter") length)
                                              (ps:chain (j-query "<div class=\"text-counter\"/>") (insert-after ".text textarea")))
                                            (ps:chain 
                                              (j-query ".text textarea")
                                              (limit 
                                                (ps:create 
                                                  max-chars (ps:LISP *max-chars-for-description*) 
                                                  counter (j-query "div.text-counter")))) 
                                            (ps:chain 
                                              (j-query window) 
                                              (unbind "cross")
                                              (bind "cross" (lambda (event)
                                                              (setf event.target.value 
                                                                    (ps:chain event.target.value 
                                                                              (substr 0 (ps:LISP *max-chars-for-description*))))
                                                              (ps:chain (j-query event.target) (trigger "keypress"))
                                                              )))
                                            (ps:chain 
                                              (j-query ".modal .submit.btn.btn-primary") 
                                              (click (lambda ()
                                                       (if (not (ps:chain (j-query ".modal .text textarea") (val) (trim) length))
                                                         (progn 
                                                           (ps:chain (j-query ".modal fieldset .text-error") (remove))
                                                           (ps:chain 
                                                             (j-query ".modal fieldset") 
                                                             (prepend "<div class=\"text-error\"><ul class=\"field-validation-errors\"><li>Текст обязательное поле.</li></ul></div>"))
                                                           false)
                                                         t))))))))))))
            (text-2 
              :label "Text for second rss"
              :requiredp t
              :present-as textarea 
              :satisfies #'text-2-field-satisfies-p 
              :writer (lambda (value item)
                        (setf (composition-text-2 item)
                              (normalize-newlines value))))
            ,@(when display-edit-fields-p 
                '((file-size 
                    :label "File size"
                    :present-as html 
                    :reader (lambda (item)
                              (let ((size (file-size (composition-file-name item))))
                                (yaclml:with-yaclml-output-to-string 
                                  (<:div :class (format nil "badge badge-~A"
                                                        (if (> size +1-mb-in-bytes+) "important" "success"))
                                         (<:as-is (bytes->megabytes size) " mb"))))))
                  (mp3-preview 
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
                                                                                                    "<br/>")))
                                                            (item-archive-at-display :present-as html 
                                                                                     :label "Will be archived at"
                                                                                     :reader (lambda (item)
                                                                                               (metatilities:format-date 
                                                                                                 "%d.%m.%Y %H:%M" 
                                                                                                 (slot-value item 'item-archive-at)))))) 
                                           (file 
                                             :label "File"
                                             :present-as ajax-file-upload 
                                             :parse-as (ajax-file-upload 
                                                         :upload-directory (get-upload-directory)
                                                         :file-name :browser-with-cyrillic-transliteration)
                                             :writer (lambda (value item)
                                                       (when value 
                                                         (setf (slot-value item 'file) value)
                                                         (setf (slot-value item 'cached-bit-rate) (composition-bit-rate item))
                                                         (setf (slot-value item 'cached-sound-type) (composition-sound-type item))))
                                             :reader (lambda (item)
                                                       (setf (webapp-session-value 'upload-filename) (slot-value item 'file)))
                                             :requiredp ,file-field-required-p
                                             :satisfies #'file-field-satisfies)))

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
     (:div :style "position:absolute;right:0;bottom:10px"
      (when (and (dataseq-allow-operations-p obj)
                 (or (dataseq-item-ops obj)
                     (dataseq-common-ops obj)))
        (apply #'dataseq-render-operations obj args)))))
  (call-next-method))

(defun displaying-archived-records-p ()
  (let ((widget (first 
                  (get-widgets-by-type 'weblocks-filtering-widget::custom-filtering-widget))))
    (and widget 
         (slot-value widget 'weblocks-filtering-widget::display-archived-p))))

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
                                    (object-id obj))))
          (delete-action (make-action 
                           (lambda/cc (&rest args)
                             (when (eq :yes 
                                       (do-confirmation "Are you sure you want to delete this record ?" :type :yes/no))
                               (let* ((id (parse-integer (getf args :id)))
                                      (item (first-by-values 'composition :id id)))
                                 (flash-message (dataseq-flash widget)
                                                (format nil "Deleted item with id ~A." id))
                                 (delete-one item)) 
                               (mark-dirty widget)))))
          (restore-action (make-action 
                            (lambda (&rest args)
                              (let ((id (object-id obj))
                                    (new-id (object-id (unarchive-composition obj))))
                                (flash-message (dataseq-flash widget)
                                               (format nil "Restored from archive item with id ~A, new id is ~A." id new-id))) 
                              (mark-dirty widget)))))
	(safe-apply (sequence-view-row-prefix-fn view) view obj args)
	(with-html
    (:tr :class (when (or alternp drilled-down-p)
                  (concatenate 'string
                               (when alternp "altern")
                               (when (and alternp drilled-down-p) " ")
                               (when drilled-down-p "drilled-down")))
         (apply #'render-table-view-body-row view obj widget :row-action row-action args)
         (:td :style "white-space:nowrap;"
          (:div :class "btn-group"
           (when (not drilled-down-p)
             (if (displaying-archived-records-p)
               (render-link restore-action "<i class=\"icon-briefcase\"></i> restore" :class "btn btn-small btn-info")
               (progn 
                 (render-link row-action "<i class=\"icon-pencil\"></i>" :class "btn btn-small btn-info")  
                 (htm
                   (:a :class  "btn btn-small btn-danger"
                    :href (add-get-param-to-url (make-action-url delete-action) "id" (write-to-string (object-id obj))) :onclick (format nil "initiateActionWithArgs(\"~A\", \"~A\", {id: \"~A\"}); return false;"
                                                                                                                                         delete-action (session-name-string-pair) (object-id obj))
                    "<i class=\"icon-trash\"></i>" 
                    )))))))))
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
  (let ((dataedit-item-widget (dataedit-item-widget obj)))
    (setf (dataedit-item-widget obj) nil)
    (call-next-method)
    (setf (dataedit-item-widget obj) dataedit-item-widget)
    (if dataedit-item-widget
      (with-html 
        (:div :style "position:absolute;left:0;top:0;"
         (:div :style "position:relative"
          (:div :class "modal" :style "width:800px;margin-left:-400px;top:10px;margin-top:0;bottom:10px;margin-bottom:0;"
           (:div :class "modal-body" :style "height:100%;max-height:100%;"
            (render-widget (dataedit-item-widget obj)))
           (with-javascript 
             (ps:ps 
               (ps:chain (j-query ".modal") (modal))
               (ps:chain (j-query ".modal-backdrop") (unbind "click") (bind "click" (lambda () nil)))))))))
      (send-script 
        (ps:ps (ps:chain (j-query ".modal-backdrop") (remove)))))))

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

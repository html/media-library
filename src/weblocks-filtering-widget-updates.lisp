(in-package :weblocks-filtering-widget)

(defwidget custom-filtering-widget (filtering-widget)
  ((simple-form-view :initform t :accessor custom-filtering-widget-simple-form-view-p)))

(defmethod render-filter-form ((widget custom-filtering-widget))
  (with-slots (filter-form-visible) widget 
    (when (or filter-form-visible (custom-filtering-widget-simple-form-view-p widget)) 
      (render-widget (get-filter-form widget)))))

(defmethod render-widget-body :around ((widget filtering-widget) &rest args)
  (with-slots (filters filter-form-visible add-filter-action) widget 
    (with-html 
      (cond 
        ((or filter-form-visible (custom-filtering-widget-simple-form-view-p widget))
         (render-filter-form widget)) 
        ((getf filters :value)
         (htm 
           (:div :style "overflow:auto;"
            (:div :style "padding:5px;float:left;"
             (render-filters widget filters)))))
        (t (render-link (lambda (&rest args) (setf filter-form-visible t)) "Add filter")))
      (:div :style "clear:both"))))

(defmacro filtering-form-simple-view (widget)
  `(defview nil 
            (:type form :persistp nil :buttons '((:submit . "Search")) 
             :caption "Searching ...")
            (compare-value :label "" :present-as input)
            (switch-to-complex 
              :label "" 
              :present-as html 
              :reader (lambda (&rest args)
                        (media-library::capture-weblocks-output 
                          (render-link (make-action (lambda (&rest args)
                                                      (setf (custom-filtering-widget-simple-form-view-p widget) nil)
                                                      (setf (slot-value widget 'filtering-form-instance) nil)
                                                      (mark-dirty widget))) "complex search"))))))

(defun filtering-form-complex-view (widget presentation)
  (eval `(defview nil 
                  (:type form :persistp nil :buttons '((:submit . "Search")) 
                   :caption "Complex searching ...")
                  (field :label ,(cl-config:get-value 
                                   :weblocks-filtering-widget.filtering-form-field-caption 
                                   :default "Search for ...") :present-as (,presentation :choices ',(compare-field-form-choices widget))
                         :requiredp t)
                  (compare-type 
                    :label ,(cl-config:get-value 
                              :weblocks-filtering-widget.filtering-form-compare-type-caption
                              :default "which ..")
                    :present-as 
                    (,presentation :choices '(("is like ..." . "like") 
                                              ("is equal to ..." . "equal")))
                    :requiredp t)
                  (compare-value :label 
                                 ,(cl-config:get-value 
                                    :weblocks-filtering-widget.filtering-form-compare-value-caption
                                    :default "value ...") :present-as input)
                  (switch-to-simple 
                    :label "" 
                    :present-as html 
                    :reader (lambda (&rest args)
                              (media-library::capture-weblocks-output 
                                (render-link (make-action (lambda (&rest args)
                                                            (setf (custom-filtering-widget-simple-form-view-p ,widget) t)
                                                            (setf (slot-value ,widget 'filtering-form-instance) nil)
                                                            (mark-dirty ,widget))) "simple search")))))))

(defun make-filtering-form (widget data &rest args)
  (setf (getf data :field) (string (getf data :field)))
  (let* ((presentation (cl-config:get-value 
                         :weblocks-filtering-widget.filtering-form-fields-presentation 
                         :default 'links-choices))
         (view (if (custom-filtering-widget-simple-form-view-p widget)
                 (filtering-form-simple-view widget) 
                 (filtering-form-complex-view widget presentation)))
         (data (apply #'make-instance (list* 'filtering-data data)))
         (form 
           (progn 
             (apply #'make-quickform 
                    view
                    (append 
                      (list 
                        :data data
                        :class 'filtering-form 
                        :answerp nil)
                      args)))))

    (with-slots (filtering-widget-instance) form
      (setf filtering-widget-instance widget)
      form)))

(defmethod get-filter-form ((widget custom-filtering-widget))
  (with-slots (filtering-form-instance) widget
    (or 
      filtering-form-instance
      (setf filtering-form-instance 
            (make-filtering-form 
              widget
              (list :field (getf (first (slot-value widget 'form-fields)) :id) :compare-type "like")
              :on-success (lambda (form object)
                            (with-slots (filter-form-position filters) widget 
                              (let* ((new-filter-value (object->simple-plist object))
                                     (new-filter (list :value  nil
                                                       :id (write-to-string (gensym))
                                                       :and nil 
                                                       :or nil))
                                     (key (if filter-form-position (intern (cdr filter-form-position) "KEYWORD") :and)))
                                (firephp:fb filters new-filter new-filter-value)
                                (if (equal key :top-or)
                                  (progn 
                                    (setf (getf new-filter :or) (list filters))
                                    (setf filters new-filter))
                                  (progn

                                    (setf (getf new-filter-value :field) (intern (getf new-filter-value :field) "KEYWORD")) 
                                    (setf (getf new-filter :value) new-filter-value) 

                                    (cond 
                                      ((or (custom-filtering-widget-simple-form-view-p widget) 
                                           (not (getf filters :value))) (setf filters new-filter))
                                      (t
                                       (setf filters 
                                             (car (map-filters 
                                                    (lambda (item)
                                                      (if (string= (string (getf item :id)) (car filter-form-position))
                                                        (progn
                                                          (push new-filter (getf item key))
                                                          item)
                                                        item))
                                                    (list filters))))))))))
                            (hide-filter-form widget)
                            (mark-dirty widget))
              :on-cancel (lambda (form)
                           (hide-filter-form widget)))))))


(defun values-descriptions-to-values-map (item model-instance)
  (let ((value (getf item :value)))
    (when (and value (listp value))
      (setf (getf item :value)
            (if (equal (getf value :field) :any-field)
              (let ((value-copy-1 (copy-list value))
                    (value-copy-2 (copy-list value))
                    (value-copy-3 (copy-list value))
                    (value-copy-4 (copy-list value)))

                (setf (getf value-copy-1 :field) :text)
                (setf (getf value-copy-2 :field) :file-name)
                (setf (getf value-copy-3 :field) :artist)
                (setf (getf value-copy-4 :field) :track-title)

                (or 
                  (compare-single-value value-copy-1 model-instance)
                  (compare-single-value value-copy-2 model-instance)
                  (compare-single-value value-copy-3 model-instance)
                  (compare-single-value value-copy-4 model-instance)))
              (compare-single-value value model-instance)))))
  item)

(defmethod render-view-field ((field form-view-field) (view form-view)
                                                      (widget filtering-form) presentation value obj 
                                                      &rest args &key validation-errors field-info &allow-other-keys)
  (declare (special *presentation-dom-id*))
  (let* ((attributized-slot-name (if field-info
                                   (attributize-view-field-name field-info)
                                   (attributize-name (view-field-slot-name field))))
         (validation-error (assoc field validation-errors))
         (field-class (concatenate 'string (aif attributized-slot-name it "")
                                   (when validation-error " item-not-validated")))
         (*presentation-dom-id* (gen-id)))
    (with-html
      (:li :class field-class
       (:label :class (attributize-presentation
                        (view-field-presentation field))
               :style "display:block;float:left;width:100px;"
               :for *presentation-dom-id*
               (:span :class "slot-name"
                (:span :class "extra"
                 (unless (empty-p (view-field-label field))
                   (str (view-field-label field)))
                 (let ((required-indicator nil))
                   (when (and (form-view-field-required-p field)
                              required-indicator)
                     (htm (:em :class "required-slot"
                           (if (eq t required-indicator)
                             (str *default-required-indicator*)
                             (str required-indicator))
                           (str "&nbsp;"))))))))
       (:div 
         :style "float:left;width:350px;"
         (apply #'render-view-field-value
              value presentation
              field view widget obj
              :field-info field-info
              args))
       (when validation-error
         (htm (:p :class "validation-error"
               (:em
                 (:span :class "validation-error-heading" "Error:&nbsp;")
                 (str (format nil "~A" (cdr validation-error)))))))
       (:div :style "clear:both")))))



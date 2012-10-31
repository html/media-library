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

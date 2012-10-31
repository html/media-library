(in-package :media-library)

(defclass composition ()
  ((id)
   (text :accessor composition-text)
   (file)))

(defmethod composition-file-name ((obj composition))
  (with-slots (file) obj
    (merge-pathnames file (get-upload-directory))))

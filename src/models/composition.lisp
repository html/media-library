(in-package :media-library)

(defclass composition ()
  ((id)
   (text :accessor composition-text)
   (file)))

(defmethod composition-file-name ((obj composition))
  (with-slots (file) obj
    (merge-pathnames file (get-upload-directory))))

(defmethod composition-file-url ((obj composition))
  (format nil "/pub/upload/~A" (slot-value obj 'file)))

(defmethod composition-artist ((obj composition))
  (with-output-to-string (s)
    (external-program:run "/bin/sh" (list "script/get-id3-artist" (composition-file-name obj)) :output s)
    s))

(defmethod composition-track-title ((obj composition))
  (with-output-to-string (s)
    (external-program:run "/bin/sh" (list "script/get-id3-track-title" (composition-file-name obj)) :output s)
    s))

(in-package :media-library)

(defclass composition ()
  ((id)
   (text :accessor composition-text)
   (file :accessor composition-file)
   (item-created-at :initform (get-universal-time))
   (cached-artist :initform nil :accessor composition-cached-artist)
   (cached-track-title :initform nil :accessor composition-cached-track-title)))

(defmethod composition-file-name ((obj composition))
  (with-slots (file) obj
    (when file 
      (merge-pathnames file (get-upload-directory)))))

(defmethod composition-file-url ((obj composition))
  (format nil "/pub/upload/~A" (slot-value obj 'file)))

(defmethod composition-file-param-by-cmd ((obj composition) cmd)
  (handler-case 
    (with-output-to-string (s)
      (external-program:run "/bin/sh" (list cmd (composition-file-name obj)) :output s)
      s)
    (type-error () nil)))

(defmethod composition-artist ((obj composition))
  (composition-file-param-by-cmd obj "script/get-id3-artist"))

(defmethod composition-track-title ((obj composition))
  (composition-file-param-by-cmd obj "script/get-id3-track-title"))

(defmethod composition-created-at-rfc-822 ((obj composition))
  (with-slots (item-created-at) obj
    (when item-created-at
      (net.telent.date:universal-time-to-rfc-date item-created-at))))

(defun update-all-compositions-cached-data ()
  (loop for i in (weblocks-utils:all-of 'composition) do
        (setf (slot-value i 'cached-artist) (composition-artist i))
        (setf (slot-value i 'cached-track-title) (composition-track-title i))))

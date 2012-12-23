(in-package :media-library)

(defclass composition ()
  ((id)
   (text :accessor composition-text)
   (text-2 :accessor composition-text-2)
   (file :accessor composition-file)
   (item-created-at :initform (get-universal-time))
   (item-updated-at :initform nil)
   (cached-artist :initform nil :accessor composition-cached-artist)
   (cached-track-title :initform nil :accessor composition-cached-track-title)
   (cached-bit-rate :initform nil :accessor composition-cached-bit-rate)
   (cached-sound-type :initform nil :accessor composition-cached-sound-type)))

(defmethod composition-file-name ((obj composition))
  (with-slots (file) obj
    (when file 
      (merge-pathnames file (get-upload-directory)))))

(defmethod composition-file-url ((obj composition))
  (format nil "/pub/upload/~A" (slot-value obj 'file)))

(defmethod composition-file-full-url ((obj composition))
  (concatenate 'string 
               *app-protocol-and-domain*
               (composition-file-url obj)))

(defun composition-file-full-url-by-filename (filename)
  (format nil "~A/pub/upload/~A" *app-protocol-and-domain* filename))

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

(defmethod composition-bit-rate ((obj composition))
  (composition-file-param-by-cmd obj "script/get-mp3-bit-rate"))

(defmethod composition-sound-type ((obj composition))
  (composition-file-param-by-cmd obj "script/get-mp3-mono-or-stereo"))

(defmethod composition-created-at-rfc-822 ((obj composition))
  (with-slots (item-created-at) obj
    (when item-created-at
      (net.telent.date:universal-time-to-rfc-date item-created-at))))

(defmethod delete-persistent-object :around (store (obj composition))
  (handler-case 
    (delete-file (composition-file-name obj))
    (file-error () (eval `(log:info ,(format nil "Unable to delete ~A" (composition-file-name obj))))))
  (call-next-method))

(defun update-all-compositions-cached-data ()
  (loop for i in (weblocks-utils:all-of 'composition) do
        (setf (slot-value i 'cached-artist) (composition-artist i))
        (setf (slot-value i 'cached-track-title) (composition-track-title i))
        (setf (slot-value i 'cached-bit-rate) (composition-bit-rate i))
        (setf (slot-value i 'cached-sound-type) (composition-sound-type i))))

(defmethod (setf composition-text) :around (value (item composition))
  (call-next-method (normalize-newlines value) item))

(defun generate-composition ()
  (let ((composition (make-instance 'composition)))
    (setf (composition-text composition) "test text")
    (setf (composition-file composition) "non-existent-file")
    composition))

(defun generate-and-save-composition ()
  (persist-object *default-store* (generate-composition)))

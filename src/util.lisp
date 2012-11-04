(in-package :media-library)

(defun transliterate-cyr-to-lat-russian (text)
  ; Taken from http://cl-cookbook.sourceforge.net/strings.html
  (defun replace-all (string part replacement &key (test #'char=))
    "Returns a new string in which all the occurences of the part 
     is replaced with replacement."
    (with-output-to-string (out)
      (loop with part-length = (length part)
            for old-pos = 0 then (+ pos part-length)
            for pos = (search part string
                              :start2 old-pos
                              :test test)
            do (write-string string out
                             :start old-pos
                             :end (or pos (length string)))
            when pos do (write-string replacement out)
            while pos)))

  (let ((simple-transliteration-ru "абвгдежзийклмнопрстуфыэАБВГДЕЖЗИЙКЛМНОПРСТУФЫЭ")
        (simple-transliteration-en "abvgdegziyklmnoprstufieABVGDEGZIYKLMNOPRSTUFIE")
        (complex-transliteration-ru-en '(("ё" "yo")
                                         ("х" "h")
                                         ("ц" "ts")
                                         ("ч" "ch")
                                         ("ш" "sh")
                                         ("щ" "shch")
                                         ("ъ" "")
                                         ("ь" "")
                                         ("ю" "yu")
                                         ("я" "ya")
                                         ("Ё" "Yo")
                                         ("Х" "H")
                                         ("Ц" "Ts")
                                         ("Ч" "Ch")
                                         ("Ш" "Sh")
                                         ("Щ" "Shch")
                                         ("Ъ" "")
                                         ("Ь" "")
                                         ("Ю" "Yu")
                                         ("Я" "Ya"))))

    ; Replacing single characters
    (loop for char across simple-transliteration-ru for en-char across simple-transliteration-en do 
          (setf text (substitute en-char char text)))

    ; Replacing "complex" characters
    (loop for (char en-char) in complex-transliteration-ru-en do
          (setf text (replace-all text char en-char)))
    text))

(defun transform-file-name-for-media-library (text file-id)
  (let ((pathname (substitute #\- #\Space (transliterate-cyr-to-lat-russian text))))
    (format nil "~A"
            (make-pathname 
              :name (format nil "~A-~A" (pathname-name pathname) file-id)
              :type (pathname-type pathname)))))

(assert (string= (transform-file-name-for-media-library "Vishnu Ballada No 3 Fa Bimol Синхронная.mp3" 123)
                 "Vishnu-Ballada-No-3-Fa-Bimol-Sinhronnaya-123.mp3"))

(defmacro %current-user ()
  `(webapp-session-value 'current-user))

(defun current-user-name ()
  (getf (%current-user) :name))

(defmacro capture-weblocks-output (&body body)
  `(let ((*weblocks-output-stream* (make-string-output-stream)))
     ,@body 
     (get-output-stream-string *weblocks-output-stream*)))

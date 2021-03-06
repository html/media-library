#+xcvb (module (:depends-on ("/asdf" "/xcvb/driver" "/xcvb/version" "specials")))

(in-package :xcvb-hello)

(asdf:find-system :xcvb)

(defparameter *hello-version-path*
  (asdf:system-relative-pathname :xcvb "examples/hello/version.lisp"))

(with-open-file (s *hello-version-path* :direction :output
                   :if-exists :rename-and-delete :if-does-not-exist :create)
  (format s ";;; automatically generated by /xcvb/hello/generate-version~%~
             (in-package :xcvb-hello)~%(setf *version* ~S)~%"
          (xcvb-driver::get-xcvb-version)))

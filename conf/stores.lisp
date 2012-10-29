(in-package :media-library)

;;; Multiple stores may be defined. The last defined store will be the
;;; default.
(defstore *media-library-store* :prevalence
  (merge-pathnames (make-pathname :directory '(:relative "data"))
       (asdf-system-directory :media-library)))

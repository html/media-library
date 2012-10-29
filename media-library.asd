(defpackage #:media-library-asd
  (:use :cl :asdf))

(in-package :media-library-asd)

(defsystem media-library
    :name "media-library"
    :version "0.0.1"
    :maintainer ""
    :author ""
    :licence ""
    :description "media-library"
    :depends-on (:weblocks)
    :components ((:file "media-library")
     (:module conf
      :components ((:file "stores"))
      :depends-on ("media-library"))
     (:module src 
      :components ((:file "init-session"))
      :depends-on ("media-library" conf))))

(defpackage #:media-library-asd
  (:use :cl :asdf))

(in-package :media-library-asd)

(defsystem media-library
   :name "media-library"
   :version "0.2.0"
   :maintainer ""
   :author ""
   :licence ""
   :description "media-library"
   :depends-on 
   (:weblocks 
    :weblocks-utils 
    :yaclml 
    :external-program 
    :weblocks-filtering-widget 
    :firephp 
    :log4cl 
    :weblocks-twitter-bootstrap-application 
    :weblocks-dataseq-records-count-panel 
    :cl-mustache
    :weblocks-mustache-template-form-view 
    :weblocks-strings-translation-app 
    :weblocks-ajax-file-upload-presentation 
    :cl-cron 
    :metatilities)
   :components ((:file "media-library")
     (:module conf
      :components ((:file "stores"))
      :depends-on ("media-library"))
     (:module src 
      :components 
      ((:file "init-session" :depends-on ("util" "widgets"))
       (:module models :depends-on ("util")
        :components ((:file "composition")))
       (:module widgets 
        :depends-on ("util")
        :components ((:file "library-grid")))
       (:file "util"))
      :depends-on ("media-library" conf))))

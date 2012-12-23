(defpackage #:media-library
  (:use :cl :weblocks
        :f-underscore :anaphora :weblocks-utils :cl-who 
        :weblocks-strings-translation-app 
        :weblocks-ajax-file-upload-presentation)
  (:import-from :hunchentoot #:header-in
    #:set-cookie #:set-cookie* #:cookie-in
    #:user-agent #:referer)
  (:documentation
   "A web application based on Weblocks."))

(in-package :media-library)

(export '(start-media-library stop-media-library))

;; A macro that generates a class or this webapp

(defwebapp media-library
           :prefix "/" 
           :description "Media library"
           :subclasses (weblocks-twitter-bootstrap-application:twitter-bootstrap-webapp)
           :init-user-session 'media-library::init-user-session
           :autostart nil                   ;; have to start the app manually
           ;:ignore-default-dependencies t ;; accept the defaults
           :debug t)

(defvar *app-protocol-and-domain*  "http://contentchaos.com")

;; Top level start & stop scripts

(defun start-media-library (&rest args)
  "Starts the application by calling 'start-weblocks' with appropriate arguments."
  (apply #'start-weblocks args)
  (start-webapp 'media-library)
  (weblocks-strings-translation-app:start-weblocks-strings-translation-app :store *default-store*))

(defun stop-media-library ()
  "Stops the application by calling 'stop-weblocks'."
  (stop-webapp 'media-library)
  (stop-weblocks))

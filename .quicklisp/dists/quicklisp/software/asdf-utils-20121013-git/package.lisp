#+xcvb (module ())

(defpackage :asdf-utils-meta (:use :cl))

(in-package :asdf-utils-meta)

(eval-when (:compile-toplevel :execute)
  (defparameter *asdf-utils-exports*
    '(;; featurep string-suffix-p aif appendf
      orf
      length=n-p
      remove-keys remove-keyword
      first-char last-char
      directory-pathname-p ensure-directory-pathname
      absolute-pathname-p ensure-pathname-absolute pathname-root
      getenv getenv-pathname getenv-pathnames
      getenv-absolute-directory getenv-absolute-directories
      probe-file*
      find-symbol* strcat
      make-pathname-component-logical make-pathname-logical
      merge-pathnames* coerce-pathname subpathname subpathname*
      pathname-directory-pathname pathname-parent-directory-pathname
      pathname-parent-directory-pathname
      os-unix-p os-windows-p
      user-homedir hostname
      read-file-forms
      resolve-symlinks truenamize
      split-string
      split-name-type
      split-pathnames*
      subdirectories directory-files directory*
      hidden-file-p
      while-collecting
      delete-file-if-exists
      *wild* *wild-file* *wild-directory* *wild-inferiors*
      *wild-path* wilden directorize-pathname-host-device
      find-class*
      get-folder-path)))

(defpackage :asdf-utils
  (:use :common-lisp)
  ;; (:import-from :asdf . #.*asdf-utils-exports*)
  (:export . #.(mapcar 'string *asdf-utils-exports*)))

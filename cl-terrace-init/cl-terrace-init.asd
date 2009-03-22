
(defpackage #:cl-terrace-init-system
  (:use :cl :asdf))

(in-package #:cl-terrace-init-system)

(asdf:defsystem :cl-terrace-init
  :depends-on (:djula :logv)
  :description
"
the function !CL-TERRACE-INIT takes the name of a project and a directory and creates
a new terrace project in that directory, creating a new .asd file, \"site/\" folder,
etc.
"
  :components
  ((:file "cl-terrace-init")))
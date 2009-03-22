
(defpackage #:{{name}}-system
  (:use :cl :asdf))

(in-package #:{{name}}-system)

(asdf:defsystem #:{{name}}
  :depends-on (:cl-terrace)
  :components
  ((:module #:src
	    :components ((:file "package")
			 (:file "core"))
	    :serial t)))
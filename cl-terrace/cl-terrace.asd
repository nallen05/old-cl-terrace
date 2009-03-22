;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(defpackage :cl-terrace-system
  (:use :cl))

(in-package :cl-terrace-system)

(asdf:defsystem :cl-terrace
  :depends-on (:djula
	       :djula-util
	       :hunchentoot
	       :cl-fad
	       :trivial-utf-8
	       :logv
	       :split-sequence
	       :f-underscore)
  :description "the VC part of a hypothetical MVC web framework written in Common Lisp"
  :version "0.1"
  :author "Nick Allen <nallen05@gmail.com>"
  :components
  ((:module :cl-terrace
	    :components
	    ((:file "package")
	     (:file "special")
	     (:file "util")
	     (:file "dispatch")
	     (:file "compile")
	     (:file "terrace-file-functions")
	     (:file "core")
	     (:file "server")
	     (:file "view-only-server"))
	    :serial t)))
;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(defpackage #:djula-system
  (:use :cl))

(in-package #:djula-system)

(asdf:defsystem :djula
  :depends-on (:djula-util :trivial-utf-8 :cl-ppcre :cl-fad :logv :split-sequence :f-underscore)
  :description "spork of django templating engine for julia"
  :version "0.1"
  :author "Nick Allen <nallen05@gmail.com"
  :components
  ((:module :djula
	    :components ((:file "package")

			 (:file "core")
			 (:file "dictionary")
			 (:file "variable")
			 (:file "filter-definitions")
			 (:file "tag")
			 (:file "tag-definitions")
			 (:file "tag-documentation")
			 (:file "build-documentation"))
	    :serial t)))
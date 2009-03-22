;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(defpackage :cl-terrace-test-system
  (:use :cl))

(in-package :cl-terrace-test-system)

(asdf:defsystem :cl-terrace-test
  :depends-on (:cl-terrace :djula :portch)
  :description "tests for `cl-terrace'"
  :components
  ((:module :cl-terrace-test
	    :components ((:file "cl-terrace-test")))))
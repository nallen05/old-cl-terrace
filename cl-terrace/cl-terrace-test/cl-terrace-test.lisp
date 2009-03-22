
(defpackage :cl-terrace-test
  (:use :cl)
  (:export :!run-tests
	   :!load-tests))

(in-package :cl-terrace-test)

(defparameter *system-folder* (asdf:component-pathname (asdf:find-component (asdf:find-system "cl-terrace-test")
									    "cl-terrace-test")))

(portch:def-test-group *system-folder* '!run-tests '!load-tests
		       :run-tests t)

(in-package :cl-terrace-user)

(format nil
	"foo has the value ~A and bar has the value ~A. (numberp (v \"bar\")) -> ~A"
	(v "foo")
	(v "bar")
	(numberp (v "bar")))

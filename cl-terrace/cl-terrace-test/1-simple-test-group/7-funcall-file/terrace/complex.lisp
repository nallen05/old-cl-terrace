(in-package :cl-terrace-user)

(format nil
	"[[~A]] This is a more complex path: [[~A]]"
	(funcall-file "/nested.lisp")
	(funcall-file "/folder/nested.lisp"))
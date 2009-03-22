(in-package :cl-terrace-test)

(defun .run-terrace-test (folder)
  (let ((tests ;; read tests from "test.lisp"
 	       (read-from-string (format nil
					 "(~A)"
					 (djula-util:slurp-utf-8-file (merge-pathnames "tests.lisp" folder))))))
    
    ;; setup test project
    (cl-terrace:def-terrace 'test-project folder)

    ;; sync project
    (cl-terrace:sync-terrace 'test-project)

    ;; run tests
    (let ((cl-terrace:*terrace-project* 'test-project))
      (dolist (test tests t)
	(destructuring-bind (url-path expected-return) test
	  (portch:test expected-return
		       (cl-terrace:funcall-path url-path)
		       :test 'equalp))))))
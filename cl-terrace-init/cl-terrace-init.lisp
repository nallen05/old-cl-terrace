;; the function !CL-TERRACE-INIT takes the name of a project and a directory and creates
;; a new terrace project in that directory, creating a new .asd file, \"site/\" folder,
;; etc.
;;
;; try running this:
;;
;;    ;; load cl-terrace-init
;;    (asdf:oos 'asdf:load-op :cl-terrace-init)
;;
;;    ;; create a project named "foo" in *DEFAULT-PATHNAME-DEFAULTS*
;;    (cl-terrace-init:!cl-terrace-init "foo")
;;
;;    ;; load and start the new "foo" project
;;    (asdf:oos 'asdf:load-op :foo)
;;    (cl-terrace:!start-terrace-server 'foo:foo :port 8080)
;;
;; then go to
;;
;;    http://localhost:8080
;;

(defpackage :cl-terrace-init
  (:use :cl :logv)
  (:export :!cl-terrace-init))

(in-package :cl-terrace-init)

(defparameter *system-folder*
  (asdf:component-pathname (asdf:find-system :cl-terrace-init)))

(defparameter *template-folder*
  (merge-pathnames "templates/" *system-folder*))

(defun !cl-terrace-init (name &optional (directory *default-pathname-defaults*))
  (assert (and (cl-fad:directory-pathname-p directory)
	       (cl-fad:directory-exists-p directory))
	  (directory)
	  "<<<`DIRECTORY' should name a director that exists!>>>")
  (flet ((create-directory (fmt-string &rest fmt-args)
	     (let ((string (apply 'format nil fmt-string fmt-args)))
	       (logv:format-log "Creating directory: \"~A\"" string)
	       (ensure-directories-exist (merge-pathnames string directory))))
	 (render-template (template &rest v-plist)
	   (apply (djula:compile-template (merge-pathnames template *template-folder*)
					  :template-folder *template-folder*)
		  v-plist))
	 (create-file (content-string fmt-string &rest fmt-args)
	   (let ((string (apply 'format nil fmt-string fmt-args)))
	     (logv:format-log "Creating file \"~A\"" string)
	     (with-open-file (out (merge-pathnames string directory)
				  :direction :output
				  :if-does-not-exist :create)
	       (write-string content-string out)))))

    ;; system folder
    (let ((new-directory (create-directory "~A/" name)))

      ;; "site/" folder and hello world
      (create-directory "~A/site/" name)
      (create-directory "~A/site/terrace/" name)
      (create-file (render-template "hello-world.lisp" :name name)
		   "~A/site/terrace/i.hello-world.lisp" name)
      (create-directory "~A/site/template/" name)
      (create-file (render-template "hello-world.html" :name name)
		   "~A/site/template/hello-world.html" name)
      (create-directory "~A/site/static/" name)

      ;; "src/" folder
      (create-directory "~A/src/" name)
      (create-file (render-template "defpackage.lisp" :name name)
		   "~A/src/package.lisp" name)
      (create-file (render-template "core.lisp" :name name)
		   "~A/src/core.lisp" name)

      ;; .asd file
      (create-file (render-template "asd-file.lisp" :name name)
		   "~A/~:*~A.asd" name)

      ;; push new directory to .asd
      (push new-directory asdf:*central-registry*))))
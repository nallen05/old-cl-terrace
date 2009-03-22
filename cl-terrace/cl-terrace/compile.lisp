; this file contains the code for compiling terrace files, djula templates, djula
; dictionaries, and djula devel-dictionaries to lisp functions
;
; the functions in this file all expect *TERRACE-PROJECT* to be bound to a symbol and
;
;    (get *terrace-project* 'terrace-folder) -> the path to the "site/" folder
;
;    (get *terrace-project* 'terrace-folder) -> the path to the "template/" folder
;
;    (get *terrace-project* 'template-ffc) -> a DJULA-UTIL:FFC instance ready to be
;                                             loaded with djula template and dictionary
;                                             file functions

(in-package :cl-terrace)

; compile terrace / template files

(defun .log-function-compilation (path description 1st-time-seen)
  "alerts LOGV:*LOG-OUTPUT* that `PATH' is being compiled to a function for
`DESCRIPTION' reason.

`1ST-TIME-SEEN' should be non-NULL if `PATH' has never been compiled to a function for this
reason"
  (logv:format-log  "~A~Afile: \"/~A\""
		    (if 1st-time-seen
			"Compiling"
			"Recompiling")
		    (if description
			(format nil " ~A " description)
			" ")
		    (enough-namestring path (get *terrace-project* 'site-folder))))

(defun .compile-terrace-file (path new-p)
  "compiles the terrace file pointed to by `PATH' into a function and returns that
function. `NEW-P' should be non-NULL if `PATH' has never been compiled as a terrace
file before"

  ;; first log what's happening
  (.log-function-compilation path (.identify-filename path) new-p)

  ;; then compile the function
  (let* ((*terrace-path* path))
    (compile nil
	     (coerce `(lambda ()
			(let ((*terrace-path* ,path))
			  (block terrace-file
			    ,@(.read-from-string-understanding-in-package-forms
			       (djula-util:slurp-utf-8-file path)))))
		     'function))))

(defun .compile-devel-dictionary-file (path new-p)
  "compiles the devel-dictionry pointed to by `PATH' into a function and returns that
function. `NEW-P' should be non-NULL if `PATH' has never been compiled as a devel-
dictionary before"
  (.log-function-compilation path "devel-dictionary" new-p)
  (let ((dd (djula:compile-devel-dictionary path)))
    (constantly dd)))

(defun .compile-dictionary-file (path new-p)
  "compiles the devel-dictionry pointed to by `PATH' into a function and returns that
function. `NEW-P' should be non-NULL if `PATH' has never been compiled as a dictionary
before"
  (.log-function-compilation path "dictionary" new-p)
  (let ((d (djula:compile-dictionary path)))
    (constantly d)))

(defun .compile-template-file (path new-p)
  "compiles the devel-dictionry pointed to by `PATH' into a function and returns that
function. `NEW-P' should be non-NULL if `PATH' has never been compiled as a template
before"
  (.log-function-compilation path "template" new-p)
  (djula:compile-template path
			  :template-folder (get *terrace-project* 'template-folder)
			  :ffc (get *terrace-project* 'template-ffc)))


(in-package :cl-terrace)

; configure these

(defvar *default-reply-external-format* (flexi-streams:make-external-format :utf-8)
"default flexi streams encoding used by hunchentoot when serving cl-terrace files")

(defvar *overrule-mime-type-alist*
  '(("html"  . #1="text/html; charset=utf-8")
    (nil     . #1#) ; "clean urls"
    ("htm"   . #1#)
    ("shtml" . #1#)
    ("txt"   . "text/plain; charset=utf-8")
    ("json"  . "application/json"))
"FUNCALL-FILE looks here before HUNCHENTOOT:MIME-TYPE when setting the MIME type")

(defvar *boring-extentions* (list "lisp" "cl")
"
These file extentions are ignored by `cl-terrace' when mapping between a request
url-path and the filesystem. So the url-path

   /foo/bar

can find the file

   /foo/bar.lisp

if the string \"lisp\" is found in *BORING-EXTENTIONS*
")

; special

(defvar *terrace-project*)

(defvar *terrace-path*)

(defvar *terrace-server* nil)

(defvar *v-plist*)

; templates used by the "View Only" server

(defparameter *source-folder* (asdf:component-pathname (asdf:find-component (asdf:find-system :cl-terrace)
									    "cl-terrace")))

(defparameter *internal-template-folder*
  (merge-pathnames "templates/" *source-folder*))

(defparameter *view-only-index-page-template*
  (merge-pathnames "view-only-index-page.html" *internal-template-folder*))

(defparameter *view-only-show-dictionary-template*
  (merge-pathnames "view-only-show-dictionary.html" *internal-template-folder*))
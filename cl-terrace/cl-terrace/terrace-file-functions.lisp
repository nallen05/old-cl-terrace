; this file contains functions intended to be used within terrace files. see the
; documentation in the comments at the top of the file "core.lisp" for the API

(in-package :cl-terrace)

; terrace/static/template paths

(defun .get-path (local-name folder-key)
  "returns the file or folder pointed to by the pathname designator `P', where the root
path / is seen as

   (GET *TERRACE-PROJECT* FOLDER-KEY)
"
  (merge-pathnames (string-trim '(#\/) (namestring local-name))
		   (get *terrace-project* folder-key)))

(defun .try-to-check-path (p folder-key)
  "warns to LOGV:*LOG-OUTPUT* if it can figure out that `P' points to a file or folder
that doesn't exist in

   (get *TERRACE-PROJECT* FOLDER-KEY)
"
  (if (and (or (stringp p)
	       (pathnamep p))
	   (symbolp folder-key)
	   (boundp '*terrace-project*)
	   (boundp '*terrace-path*)
	   (not (aif (.get-path p folder-key)
		     (if (cl-fad:directory-pathname-p p)
			 (cl-fad:directory-exists-p it)
			 (cl-fad:file-exists-p it)))))
      (logv:format-log "<<<~A ~A doesn't exist: ~S>>>"
		       folder-key
		       (if (cl-fad:directory-pathname-p p)
			   "folder"
			   "file")
		       p))
  (values))

; functions for use inside terrace files

(defun render (template-path &rest plist)
"and renders the Djula template pointed to by `TEMPLATE-PATH' [where / is seen
as the \"template/\" directory of the current Terrace project]

`PLIST' should be a plist of template variables.
"
  (apply 'djula-util:ffc-call
	 (get *terrace-project* 'template-ffc)
	 (.get-path template-path 'template-folder)
	 plist))

(define-compiler-macro render (&whole form template-path &rest plist)
  (declare (ignore plist))
  (.try-to-check-path template-path 'template-folder)
  form)

(defun static (static-path &optional content-type)
"calls HUNCHENTOOT:HANDLE-STATIC-file on the file pointed to by `STATIC-PATH' [where /
is seen as the \"static/\" directory of the current Terrace project"
  (hunchentoot:handle-static-file (.get-path static-path 'static-folder)
				  content-type))

(define-compiler-macro static (&whole form static-path &optional content-type)
  (declare (ignore content-type))
  (.try-to-check-path static-path 'static-folder)
  form)

(defun funcall-file (terrace-path &rest v-plist)
  "calls the Terrace file pointed to by `TERRACE-PATH' [where / is seen as the
\"terrace/\" directory of the current Terrace project] as if it were a function and
returns it's value

 `V-PLIST' should be aplist of key/val pairs for the V function
"
  (let ((*v-plist* (append v-plist *v-plist*)))
    (djula-util:ffc-call (get *terrace-project* 'terrace-ffc)
			 (.get-path terrace-path 'terrace-folder))))

(define-compiler-macro funcall-file (&whole form terrace-path &rest v-plist)
  (declare (ignore v-plist))
  (.try-to-check-path terrace-path 'terrace-folder)
  form)

(defun v (key)
  "finds the value associated with the key `KEY' by looking into the plist *V-PLIST*.
*V-PLIST* might be populated with key/val pairs from

   1. \"special.\" files that are ancestors or siblings to the top-level Terrace file
      responding to the request
   2. \"v.\" files or folders that have an ancestor-or-self relationship to the top-level
      Terrace file responding to the request
   3. the FUNCALL-FILE function [assuming the current Terrace file has been called by
      some parent terrace file]
"
  (.getf-equal *v-plist* key))

(defun d (variable dictionary-path &rest djula::*variable-plist* &key &allow-other-keys)
  "returns two values:

   1. the value of the variable `VARIABLE' in the dictionary `TEMPLATE-PATH' in the
      language DJULA:*LANGUAGE*
   2. whether or not the variable exists in the dictionary
"
  (aif (assoc variable
	      (djula-util:ffc-call (get *terrace-project* 'template-ffc)
				   (.get-path dictionary-path 'template-folder)))
       (multiple-value-bind (fn present-p) (.getf (rest it) djula:*language*)
	 (if present-p
	     (let ((djula::*template-folder* (get *terrace-project* 'template-folder))
		   (djula::*template-path* (.get-path dictionary-path 'template-folder))
		   djula::*dictionaries*
		   djula::*devel-dictionaries*
		   djula::*ffc*
		   (djula:*language* djula:*language*))
	     (funcall fn))))))

(define-compiler-macro d (&whole form variable dictionary-path &rest v-plist)
  (declare (ignore variable v-plist))
  (.try-to-check-path dictionary-path 'template-folder)
  form)

(defun g (key)
  "returns two values:

   1. the HTTP GET parameter named `KEY'. Returns NIL if it has not been supplied or
it has been supplied but has no value.
   2. whether or not the GET parameter was supplied
"
  (aif (hunchentoot:get-parameter key)
       (values (if (not (zerop (length it)))
		   it)
	       t)))

(defun p (key)
  "returns two values:

   1. the HTTP POST parameter named `KEY'. Returns NIL if it has not been supplied or
it has been supplied but has no value.
   2. whether or not the POST parameter was supplied
"
  (aif (hunchentoot:post-parameter key)
       (values (if (not (zerop (length it)))
		   it)
	       t)))
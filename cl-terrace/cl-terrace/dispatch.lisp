; this file contains the code necessary for mapping URLs to terrace files. see the
; documentation in the comments at the top of the page in "core.lisp" for an
; explenation of the matching rules
;
; the function GET-TERRACE-FILE takes a url path string and returns all the necessary
; resources to identify and execute the appropriate terrace file and with *V-PLIST*
; bound correctly
;
; FUNCALL-PATH takes a url path and executes the appropriate terrace file with
; *V-PLIST* bound correctly. note that code inside the terrace file might be expecting
;  a proper hunchentoot environment [*REQUEST* and *REPLY* being bound, etc]. this
;  function is mostly useful for debugging
;
; the functions in this file all expect *TERRACE-PROJECT* to be bound to a symbol and
;
;    (get *terrace-project* 'terrace-folder) -> the path to the "terrace/" folder
;
;    (get *terrace-project* 'terrace-ffc) -> a DJULA-UTIL:FFC instance ready to be
;                                            loaded with terrace file functions
;
; todo: -GET-TERRACE-FILE needs to be sped up w/ some sort of caching
;

(in-package :cl-terrace)

(defun .trim-boring-extentions (filename)
  "removes the extension of `FILENAME' if is found in *BORING-EXTENTIONS*."
  (if (find (pathname-type filename) *boring-extentions* :test 'equal)
      (pathname-name filename)
      (namestring filename)))
  
; identifying files and folders

(defun .identify-filename (name)
  "returns two values:

   1. a keyword identifying a special Terrace prefix or NIL if `NAME' does not have one
   2. the token created by removing the special prefix after the match or `NAME' if
      there is no special prefix

   (.identify-filename \"v.foo.cl\")

   -> :v, \"foo.cl\"

   (.identify-filename \"foo.cl\")

   -> NIL, \"foo.cl\"
"
  (flet ((% (<id> <token>)
	   (if <token>
	       (return-from .identify-filename (values <id> <token>)))))
    (let ((name (namestring name)))

      ; v.foo.cl -> :v, "foo.cl"
      (% :v (.matches-prefix "v." name))

      ; special.foo.cl -> :special, "foo.cl"
      (% :special (.matches-prefix "special." name))

      ; i.foo.cl -> :i, "foo.cl"
      (% :i (.matches-prefix "i." name))

      ; foo.cl -> NIL, "foo.cl"
      (values nil name))))

(defun .identify-foldername (name)
  "returns two values:

   1. a keyword identifying a special Terrace prefix/suffix or NIL if `NAME' does not
      have any special prefix/suffix
   2. the token created by the match after removing the special prefix/suffix or `NAME'
      if there is no special prefix/suffix

   (.identify-foldername \"v.foo\")

   -> :v, \"foo\"

   (.identify-foldername \"foo.i\")

   -> :i, \"foo\"

   (.identify-filename \"foo\")

   -> NIL, \"foo\"
"
  (flet ((% (<id> <token>)
	   (if <token>
	       (return-from .identify-foldername (values <id> <token>)))))
    (let ((name (namestring name)))

      ; v.foo -> :v, "foo"
      (% :v (.matches-prefix "v." name))

      ; foo.i -> :i, "foo"
      (% :i (if (equal (pathname-type name) "i")
		(pathname-name name)))

      ; foo -> NIL, "foo"
      (values nil name))))

(defmacro .with-identified-file ((path identify &optional token) &body body)
  "if `PATH' can be identified as an `IDENTIFY' type of file then executes `BODY'.
Otherwise returns NIL.

If `TOKEN' is supplied, then it is bound to the token resulting from the identification
within the scope of `BODY'

   (.with-identified-file (\"/foo/v.bar.cl\" :v <token>)
     (values t <token>))

   -> t, \"bar.cl\"

   (.with-identified-file (\"/foo/v.bar.cl\" :i <token>)
     (values t <token>))

   -> NIL
"
  (let ((<type> (gensym "type")))
    `(multiple-value-bind (,<type> ,@(if token
					 `(,token)))
	 (.identify-filename (djula-util:filename ,path))
       (case ,<type>
	 ((,identify) ,@body)
	 (otherwise nil)))))

(defmacro .with-identified-folder ((path identify &optional token) &body body)
  "if `PATH' can be identified as an `IDENTIFY' type of folder then executes `BODY'.
Otherwise returns NIL.

If `TOKEN' is supplied, then it is bound to the token resulting from the identification
within the scope of `BODY'

   (.with-identified-folder (\"/foo/v.bar/\" :v <token>)
     (values t <token>))

   -> t, \"bar\"

   (.with-identified-folder (\"foo/v.bar/\" :i <token>)
     (values t <token>))

   -> NIL
"
  (let ((<type> (gensym "type")))
    `(multiple-value-bind (,<type> ,@(if token
					 `(,token)))
	 (.identify-foldername (djula-util:foldername ,path))
       (case ,<type>
	 ((,identify) ,@body)
	 (otherwise nil)))))

; matching terrace files from urls

(defun .get-normal-match/file (folder name)
  "returns any files in `FOLDER' that match the name `NAME' [does not return any \"v.\"
file matches]"
  (some (f_ (.with-identified-file (_ nil)
	      (if (equal (.trim-boring-extentions (djula-util:filename _)) name)
		  _)))
	(djula-util:non-boring-files folder)))

(defun .get-v-match/file (folder)
  "returns the first \"v.\" file found in `FOLDER'"
  (mapc (f_ (.with-identified-file (_ :v <token>)
	      (return-from .get-v-match/file (values _ <token>))))
	(djula-util:non-boring-files folder))
  nil)

(defun .get-normal-match/folder (folder name)
    "returns any folders in `FOLDER' that match the name `NAME' [does not return any
\"v.\" folder matches]"
  (some (f_ (block <f_>
	      (.with-identified-folder (_ nil)
		(if (equal (djula-util:foldername _) name)
		    (return-from <f_> _)))
	      (.with-identified-folder (_ :i <token>)
		(if (equal <token> name)
		    (return-from <f_> _)))))
	(djula-util:non-boring-folders folder)))

(defun .get-v-match/folder (folder)
  "returns the first \"v.\" folder found in `FOLDER'"
  (mapc (f_ (.with-identified-folder (_ :v <token>)
	      (return-from .get-v-match/folder (values _ <token>))))
	(djula-util:non-boring-folders folder))
  nil)

(defun .get-index-file (folder)
  "if `FOLDER' is has the suffix \".i\" or `ROOT-FOLDER-P' is T, then `FOLDER' is an
\".i\" folder. return the first file with the prefix \"i.\""
  (some (f_ (.with-identified-file (_ :i)
	      _))
	(djula-util:non-boring-files folder)))

(defun .get-index-match (folder name)
  (some (f_ (.with-identified-folder (_ :i <token>)
	      (when (equal <token> name)
		(.get-index-file _))))
	(djula-util:non-boring-folders folder)))

(defun .get-special-path-alist (folder)
  "
returns an alist of

    (token . path)

key/vals made from any \"special.\" files seen in folder"
  (mapcan (f_ (.with-identified-file (_ :special <token>)
		(list (cons (.trim-boring-extentions <token>) _))))
	  (djula-util:non-boring-files folder)))

(defun .get-terrace-file (url-path-list path)
  "Tries to match `URL-PATH' with `PATH' [which may be a file or a folder]. If there is
no match returns NIL. if there is a match it returns 3 values:

   1. the pathname of the matching file
   2. the v-plist derived by combining `URL-PATH' with `PATH'
   3. the \"special.\" path alist seen while making the match
      [see SPECIAL-PATH-ALIST->PLIST]

"
  (if (null url-path-list)

      ;; `URL-PATH-LIST' is empty. This means that `PATH' is probably the root folder
      ;; and GET-TERRACE-FILE was called with a url "/". Look for an "i." file in `PATH'
      (if (cl-fad:directory-pathname-p path)
	  (aif (.get-index-file path)
	       (values it nil (.get-special-path-alist path))))

      ;; `URL-PATH-LIST' is not empty
      (destructuring-bind (1st . rest) url-path-list
	(if (null rest)

	    ;; `URL-PATH-LIST' points to a file

	    (multiple-value-bind (<v-match> <v-token>) (.get-v-match/file path)
	      (if <v-match>

		  ;; maybe a "v." match
		  (values <v-match>
			  (list (.trim-boring-extentions <v-token>) 1st)
			  (.get-special-path-alist path))
		  
		  (let ((normal-match (.get-normal-match/file path 1st)))
		    (if normal-match

			;; is a normal match
			(values normal-match
				nil
				(.get-special-path-alist path))

			(let ((index-match (.get-index-match path 1st)))
			  (if index-match

			      ;; is a ".i" directory with an "i." file
			      (values index-match
				      nil
				      (nconc (.get-special-path-alist path)
					     (.get-special-path-alist (directory-namestring index-match))))))))))

	    ;; `URL-PATH-LIST' points to a directory
	    (progn

	      ;; maybe a "v." match
	      (multiple-value-bind (match <token>) (.get-v-match/folder path)
		(if match
		    (multiple-value-bind (resource v-url-plist special-v-path-alist)
			(.get-terrace-file rest match)
		      (if resource
			  (return-from .get-terrace-file
			               (values resource
					       (list* (.trim-boring-extentions <token>) 1st v-url-plist)
					       (append (.get-special-path-alist path)
						       special-v-path-alist)))))))

	      ;; maybe a normal match
	      (let ((match (.get-normal-match/folder path 1st)))
		(if match
		    (multiple-value-bind (resource v-url-plist special-v-path-alist)
			(.get-terrace-file rest match)
		      (if resource
			  (return-from .get-terrace-file
			               (values resource
					       v-url-plist
					       (append (.get-special-path-alist path)
						       special-v-path-alist))))))))))))

(defun get-terrace-file (url-path)
  "Tries to match `URL-PATH' with the \"terrace/\" folder of *TERRACE-PROJECT*. If there
is no match returns NIL. If there is a match returns 3 values:

   1. the pathname of the matching file
   2. the v-plist derived by combining `URL-PATH' with `PATH'
   3. the \"special.\" path alist seen while making the match
      [see SPECIAL-PATH-ALIST->PLIST]
"
  (.get-terrace-file (split-sequence:split-sequence #\/
						    url-path
						    :remove-empty-subseqs t)
		     (get *terrace-project* 'terrace-folder)))

(defun special-path-alist->plist (alist)
  "takes a special-path-alist [like the one returned as the 3rd return value of
GET-TERRACE-FILE], which is filled with

   (token . path)

key/vals and returns a plist where the keys are the tokens from the alist and the vals
are the results of calling FUNCALL-FILE on the paths of the alist"
  (when alist
    (destructuring-bind ((<k> . path) . rest) alist
      (list* <k>
	     (djula-util:ffc-call (get *terrace-project* 'terrace-ffc)  path)
	     (special-path-alist->plist rest)))))

(defun .dispatch-url-path (url-path)
  "
if there is a terrace file matching `URL-PATH' then returns a handler that, when called,
generates the reply to that request. otherwise returns NIL

note: *TERRACE-PROJECT* must be bound to a terrace project so it knows where the
\"terrace/\" folder is

note: neither .DISPATCH-URL-PATH nor the function returned by it do anything that
involves hunchentoot
"
  (multiple-value-bind (path url-v-plist special-path-alist) 
      (get-terrace-file url-path)
    (if path
	(let ((terrace-project *terrace-project*))
	  (f0 (let* ((*terrace-project* terrace-project)
		     (*v-plist* (nconc url-v-plist (special-path-alist->plist special-path-alist))))
		(aif (djula-util:ffc-call (get *terrace-project* 'terrace-ffc)
					  (namestring path))

		     it

		     ;; if the terrace file returns NIL throw a 404
		     (progn
		       (setf (hunchentoot:return-code) hunchentoot:+http-not-found+)
		       (throw 'hunchentoot:handler-done nil)))))))))

; useful for debugging

(defun funcall-path (url-path)
  (aif (.dispatch-url-path url-path)
       (funcall it)))
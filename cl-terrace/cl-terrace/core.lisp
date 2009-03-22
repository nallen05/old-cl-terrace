;
; OVERVIEW
;
; Simply put: `Cl-Terrace' is the VC part of a hypothetical MVC framework written in
; Common Lisp. It uses `Hunchentoot' for a webserver, `Djula' for HTML templating, and
; a few abstractions over the url-as-file-in-the-filesystem metaphor for deciding how
; to reply to HTTP requests
;
; TERRACE PROJECTS
;
; a terrace project is represented within a lisp image as a symbol. the symbol becomes
; a terrace project once it has been linked to a "site/" folder using the DEF-TERRACE
; function
;
; a "sight/" folder must contain at least 1 of the following 3 subfolders:
;
;    1. "static/" -- the "static/" folder holds static content used by the project
;    2. "template/" -- the "template/" folder holds Djula templates [and dictionaries]
;                      used by the project
;    3. "terrace/" -- the "terrace/" folder holds the project's ``terrace files''. the
;                     contents of the "terrace/" directory control how the project
;                     dispatches and handles individual HTTP requests. see
;                     TERRACE FILES
;
; [note: running DEF-TERRACE hijacks the SYMBOL-FUNCTION and SYMBOL-VALUE slots of the
; symbol. the symbol value is a Hunchentoot dispatcher]
;
; once you have turned a symbol into a terrace project you can to do any of the
; following 3 things with it:
;
;    1. start a view server using the !START-TERRACE-SERVER/VIEW-ONLY function
;      
;       you can use a view server to preview interactive mock-ups of your website
;
;       [see Djula's {% devel-dictionary %}, {% devel-value %}, and {% include %} tags
;       for ideas].
;
;       After starting the server go to the root url (/) in your browser and it will
;       guide you from there.
;
;    2. publish the project with the function !START-TERRACE-SERVER
;
;       once this is done your site is up and running and viewable in the browser!
;
;    3. push the project to HUNCHENTOOT:*DISPATCH-TABLE* and start Hunchentoot yourself
;
;       publishing the project manually makes it easier for multiple terrace projects
;       to coexist behind the same Hunchentoot instance or for terrace projects to
;       coexist with other lisp code replying to HTTP requests behind the same
;       Hunchentoot image
;
;       note: remember that you need to sync the project with the SYNC-TERRACE function
;       before the dispatcher will do anything
;
; SYNCING PROJECTS
;
; calling the function SYNC-TERRACE on a project makes its Hunchentoot dispatcher aware
; of any new changes or additions to the "terrace/" or "template/" directories. this
; has to be done at least once before the project's Hunchentoot dispatcher will do
; anything. this is done automatically by !START-TERRACE-SERVER
;
; TERRACE FILES
;
; the "terrace/" folder contains the project's terrace files. terrace files control
; how the project handles a particular set of possible HTTP requests. the positions of
; terrace files within the "terrace/" folder [and how they relate to the url path of
; the HTTP request] dictates which terrace  file will be responsible for replying to a
; request
;
; terrace files are like normal lisp source files except their last form should return
; the body of the reply as a string or array of bytes [like a Hunchentoot dispatcher],
; or NIL, in which case the server will return a 404. 
;
; in simple cases this works something like PHP:
;
; this url:
;
;    http://localhost:8282/hello-world
;
; matches this file:
;
;    /hello-world.lisp
;
; [note: the root path in these examples is assumed to start at the "terrace/" folder]
;
; [".lisp" and ".cl" extensions are trimmed from the filename during this process]
;
; there is, however, much more you can do with terrace files. for instance if you have
; the following file
;
;    /user/v.username.lisp
;
; containing the following lisp code:
;
;       (in-package :cl-terrace-user)
;       (format nil "the username is ~A" (v "username"))
;
; then the following url
;
;    http://localhost:8282/user/bob
;
; with show up in the browser as
;
;    "the username is bob"
;
; see TERRACE FILE MATCHING RULES for more info on terrace files
;
; FUNCTIONS FOR USE INSIDE TERRACE FILES
;
; the following functions are exported from the CL-TERRACE package specifically for use
; within terrace files
;
; RENDER (template-path &rest kwd-args &key &allow-other-keys)
;
;    -- renders and returns the Djula template pointed by `TEMPLATE-PATH'. the root
;       path (/) is the "template/" folder
;
;       `KWD-ARGS' is a plist of keyword arguments that maps to variables used within
;       the template. RENDER accepts :. "splices" like the function returned by
;       DJULA:COMPILE-TEMPLATE
;
; STATIC (static-path &optional content-type)
;
;    -- handles the file pointed to by `STATIC-PATH'. the root path (/) is the
;       "static/" folder
;
; FUNCALL-FILE (terrace-path &rest v-plist &key &allow-other-keys)
;
;    -- calls the terrace file pointed to by `TERRACE-PATH' [returning its value as if
;       it were a normal lisp function]. the root path (/) is the "terrace/" folder
;
;       `V-PLIST' should be a plist of keys/values that will be visible to the V
;        function within the body of the terrace file pointed to by `TERRACE-PATH'
;
; D (variable dictionary-path &rest variable-plist &key &allow-other-keys)
;
;    -- returns the value of the variable `VARIABLE' in the dictionary file pointed to
;       by `TEMPLATE-PATH' in the language DJULA:*LANGUAGE*. `VARIABLE-PLIST' should be
;       a plist of template variable name/value pairs like the ones given to RENDER
;       [it takes :. "splices"]
;
;       also returns a second value that is non-NULL if the value of `VARIABLE'
;       is actually found in the language `LANGUAGE' in the dictionary [like GETHASH's
;       second return value]
;
; V (key)
;
;    -- returns the value associated with the string `KEY'. V will look for the key in
;       the following places:
;
;        1. the request url and how it relates to any "v." files or folders
;           encountered while dispatching the request [see TERRACE FILE MATCHING RULES]
;        2. any "special." files that are ancestors or siblings of the top-level
;           terrace file responding to the request [see TERRACE FILE MATCHING RULES]
;        3. the `v-plist' argument to FUNCALL-FILE [assuming there has been a call to
;           FUNCALL-FILE somewhere up the stack]
;
; G (key)
;
;     -- returns the HTTP GET parameter named `key'.
;
;        G behaves differently from HUNCHENTOOT:GET-PARAMETER in that a GET parameter
;        that has been supplied but has no value is represented as NIL [instead of an
;        empty string]. there is a second return value that indicates whether the HTTP
;        GET parameter was supplied or not
;
; P (key)
;
;     -- like G, but for POST parameters instead of GET parameters
;
; RETURNING FROM TERRACE FILES
;
; You can escape from a terrace file at any time by calling RETURN-FROM with the block
; TERRACE-FILE. eg:
;
;    (return-from terrace-file "my webpage")
;
; note: there is no way if a terrace file can know if it's being called directly by 
; Hunchentoot to handle a request or from within another terrace file by FUNCALL-FILE.
; if you want to simulate returning from the toplevel terrace file [the one handling
; the request] throw a HUNCHENTOOT:HANDLER-DONE. eg:
;
;    (throw 'hunchentoot:handler-done "my webpage")
;
; TERRACE FILES AND COMMON LISP PACKAGES
;
; the default package of all terrace files is the :CL-USER package. use top-level
; IN-PACKAGE forms to change *PACKAGE* while the file is being read [like you would do
; with a normal lisp source file]
;
; `cl-terrace' comes with a :CL-TERRACE-USER package that uses the following packages:
;    :COMMON-LISP
;    :CL-TERRACE
;    :DJULA
;    :HUNCHENTOOT
;    :LOGV
;
; MIME-TYPE / ENCODING
;
; cl-terrace is utf-8 "out of the box". terrace files and Djula templates are always
; assumed to be encoded in UTF-8.
;
; HUNCHENTOOT:CONTENT-TYPE is set from the content type derived from the toplevel
; terrace file replying to the request after shaving off the boring ".lisp" or ".cl"
; extention. so the terrace file
;
;    /foo.html.lisp
;
; will send out MIME type
;
;    text/html; charset=utf-8
;
; in the headers before computing the response.
; 
; note that the contenty type is derived by first looking in
; CL-TERRACE:*OVERRULE-MIME-TYPE-ALIST* then calling HUNCHENTOOT:MIME-TYPE. this means
; that things like .txt, .html, .htm, .shtml and "clean urls", etc., will be explicitly
; given UTF-8 charset
;
; <<<DANGER!!! SIDE EFFECTS!!!>>>
;
; when a cl-terrace project considers replying to a request, it sets
; HUNCHENTOOT:REPLY-EXTERNAL-FORMAT to CL-TERRACE::*DEFAULT-REPLY-EXTERNAL-FORMAT*
; [which by default is a :UTF-8 flexi-streams external format]. it also recomputes the
; request parameters using this external format. HUNCHENTOOT:REPLY-EXTERNAL-FORMAT can
; be set from within the terrace file generating the reply to the request if the
; multibyte UTF-8 encoding screws up generating binary requests...
;
; TERRACE FILE MATCHING RULES
;
; URL: /foo/bar/baz
; FILE: /foo/bar/baz.lisp
;
; -> serves result of executing the lisp code in "/foo/bar/baz.lisp"
; [note .cl files work just the same as .lisp]
;
; URL: /foo/bar
; FILE: /foo/bar.i/i.baz.lisp
;
; -> serves result of running the lisp code in "/foo/bar.i/i.baz.lisp"
;
; "i.baz.lisp" is the ``index-file'' of the folder "bar.i". Any folder that has an
; index-file should have the suffix ".i". the obvious exception is the root
; "terrace/" folder
;
; URL: /foo/bar/anything
; FILE: /foo/bar/v.baz.lisp
;
; -> serves result of running the lisp code in "/foo/bar/v.baz.lisp"
;
; Within this code:
;
;       (v "baz") -> "anything"
;
; URL: /more/other/stuff
; FILE: /v.foo/v.bar/v.baz.lisp
;
; -> serves result of running the lisp code in "/v.foo/v.bar/v.vaz.lisp"
;
; Within this code:
;
;    (v "foo") -> "more"
;    (v "bar") -> "other"
;    (v "baz") -> "stuff"
;
; URL: /foo/bar
; FILE: /foo/bar.lisp
; ALSO SEEN: /special.a.lisp -- contains the code (+ 1 2)
;            /foo/special.b.lisp -- contains the code: (1+ (v "a"))
;
; -> serves result of running the lisp code in "/foo/bar.lisp".
;
; Within this code:
;
;    (v "a") -> 3
;    (v "b") -> 4
;

(in-package :cl-terrace)

(defun .overruled-mime-type (pathspec)
"
like HUNCHENTOOT:MIME-TYPE but first checks *OVERRULE-MIME-TYPE* then
*OVERRULE-MIME-TYPE-LIST*

   (.overruled-mime-type \"/foo.html\")

   -> \"text/html; charset=utf-8\"

   (.overruled-mime-type \"/foo\")

   -> \"text/html; charset=utf-8\"

   (.overruled-mime-type \"/foo.txt\")

   -> \"text/plain; charset=utf-8\"

   (let ((*overrule-mime-type[ \"image/jpeg\"))
     (.overruled-mime-type \"/foo.txt\"))

   -> \"image/jpeg\"
"
  (or (rest (assoc (pathname-type pathspec)
		   *overrule-mime-type-alist*
		   :test 'equalp))
      (hunchentoot:mime-type pathspec)))

; projects


(defun def-terrace (project-name
		    site-folder
		    &key (publish-static-p t))
"Creates a Terrace project named `PROJECT-NAME' with a \"site/\" folder `SITE-FOLDER'
[note: this creates a Hunchentoot dispatcher named `PROJECT-NAME', so you can't ever
define a function named `PROJECT-NAME' afterwords].

`SITE-FOLDER' should contain 3 subfolders:

   1. terrace/ -- holds Terrace files [\".lisp\" or \".cl\" files] that are used to
                  dispatch HTTP requests. Make sure to read about the rules for matching
                  url-paths to Terrace Files at the top of this source file. 

   2. template/ -- holds Djula templates. templates in this folder can be rendered with
                   the function TEMPLATE

   3. static/ -- holds static files used to dispatch an HTTP request

if `PUBLISH-STATIC-P' is non-NULL then the `PROJECT-NAME' Hunchentoot dispatcher
publishes the contents of the \"static/\" folder. Otherwise the \"static/\" folder is
ignored. [It's much faster to have some other webserver such as nginx or Apache sitting
in front of Hunchentoot serving your static files]
"
  (let* ((terrace-folder  (merge-pathnames "terrace/" site-folder))
	 (template-folder (merge-pathnames "template/" site-folder))
	 (static-folder   (merge-pathnames "static/" site-folder))
	 (static-dispatcher (if publish-static-p
				(hunchentoot:create-folder-dispatcher-and-handler "/" static-folder))))

    ;; create a "terrace/" folder dispatcher named "project-name
    (setf (symbol-function project-name)
	  (lambda (hunchentoot:*request*)
	    (let ((*terrace-project* project-name))

	      ;; expect everything to come in encoded in utf-8
	      (hunchentoot:recompute-request-parameters :request hunchentoot:*request*
							:external-format *default-reply-external-format*)

	      (or ;; maybe dispatch terrace file
	          (aif (.dispatch-url-path (hunchentoot:script-name hunchentoot:*request*))
		       (f0  ;; set hunchentoot MIME and encoding stuff
			    (setf (hunchentoot:reply-external-format) *default-reply-external-format*
				  (hunchentoot:content-type)          (let ((% (.trim-boring-extentions (hunchentoot:script-name hunchentoot:*request*))))
									;; this bit of uglyness is because the "i."/ "v."
									;; suffix of index files and variable files 
									;; confuses poor .OVERRULED-MIME-TYPE
									(.overruled-mime-type (cond ((eql (mismatch % "i.") 2)
												     (subseq % 2))
												    ((eql (mismatch % "v.") 2)
												     (subseq % 2))
												    (t %)))))
			    (funcall it)))

	          ;; maybe dispatch static file
		  (if static-dispatcher
		      (funcall static-dispatcher hunchentoot:*request*))))))

    ;; set symbol-plist
    (setf ;; save `SITE-FOLDER' and subfolders
          (get project-name 'site-folder)       site-folder
          (get project-name 'terrace-folder)    terrace-folder
          (get project-name 'template-folder)   template-folder
          (get project-name 'dictionary-folder) template-folder
          (get project-name 'static-folder)     static-folder

	  ;; save keyword options
	  (get project-name 'publish-static-p) publish-static-p

	  ;; create project caches
	  (get project-name 'terrace-ffc)    (djula-util:ffc-init '((identity . .compile-terrace-file)))
	  (get project-name 'template-ffc)   (djula-util:ffc-init '((djula:dictionary-p . .compile-dictionary-file)
								    (djula:devel-dictionary-p . .compile-devel-dictionary-file)
								    (identity . .compile-template-file)))

	  ;; finalize
	  (get project-name 'terrace-project-p) t)

    ;; make `PROJECT-NAME' evaluate to itself
    (setf (symbol-value project-name)
	  project-name)

    project-name))

(defun sync-terrace (project &key (sync-template-folder t) (sync-terrace-folder t))
  "makes sure all the functions compiled from files in `PROJECT's \"terrace/\" and
\"template/\" folders are up to date, recompiling them if the're not"
  (if (not (get project 'terrace-project-p))
      (error "<<<~S does not name a terrace project [see DEF-TERRACE]>>>" project)
      (flet ((syncit (folder-name cache-name)
		 (aif (get project folder-name)
		      (if (cl-fad:directory-exists-p it)
			  (let ((*terrace-project* project))
			    (logv:format-log "Syncing ~A" folder-name)
			    (djula-util:ffc-sync (get project cache-name) it)
			    (djula-util:ffc-uncache-deleted (get project cache-name)
							    :hook (f_ (logv:format-log "Deleting ~A from ~A"
										       _
										       cache-name))))
			  (logv:format-log "<<<There is no ~A folder in \"site/\">>>"
					   folder-name))
		      (logv:format-log "<<<~A does not have a ~A folder>>>"
				       project
				       folder-name))))
	(logv:format-log "\"site/\": ~A" (get project 'site-folder))
	(if sync-template-folder
	    (syncit 'template-folder 'template-ffc))
	(if sync-terrace-folder
	    (syncit 'terrace-folder 'terrace-ffc))))
  project)
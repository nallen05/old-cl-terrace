; EXTERNAL LISP API
;
; OVERVIEW
;
; the Lisp API for Djula can be summed up as:
;
;              1. a single function COMPILE-TEMPLATE that takes a template file and
;                 returns a function. this function takes keyword arguments that map to
;                 template variables seen in the template and returns the rendered
;                 template as a string [or UTF-8 bytes]. note that this function can
;                 take "splices" mixed in with the keyword plist [denoted with the
;                 special key :.]
;
;              2. some special variables that can be configured at runtime to
;                 change the behavior of the functions created by COMPILE-TEMPLATE
;
; it also exports:
;
;              1. the function BUILD-HTML-DOCUMENTATION-FROM-SOURCE [which builds the
;                 Djula template API as an HTML document
;
;              2. some functions used by `cl-terrace'
;
; REFERENCE
;
;    function:
;    COMPILE-TEMPLATE (PATH &KEY (RETURN-FORMAT :STRING)
;                                (TEMPLATE-FOLDER *TEMPLATE-FOLDER*)
;                                FFC)
;
;      -- returns a function that takes keyword arguments mapping to variables seen in
;         the template pointed to by `PATH' and returns the template rendered as a
;         string
;
;         note that using the plist key :. [aka the keyword :|.|] "splices" a plist
;         into the keyword arguments. the spliced-in plist is searched before searching
;         the rest of the plist [see .GETF-V-PLIST]
;
;         if `RETURN-FORMAT' is :STRING then the function returns a string. if
;         `RETURN-FORMAT' is :OCTET then an array of utf-8 encoded bytes is returned.
;
;         either the argument `TEMPLATE-FOLDER' has to be supplied or *TEMPLATE-FOLDER*
;         needs to be bound when calling COMPILE-TEMPLATE so it knows where to find
;         other templates, dictionaries, rulebooks, etc, when compiling FUNCTION
;
;         if supplied, the `FFC' should be a DJULA-UTIL:FFC object. this is used when
;         you are managing your templates with a file-function cache. see the
;         `cl-terrace' source, I will document this when I get a chance --nick
;
;    variable:
;    *LANGUAGE*
;    default value: :ENGLISH
;
;      -- represents the language that the function returned by COMPILE-TEMPLATE
;         should be using
;
;    variable:
;    *DEVEL-ONLY-P*
;    default value: T
;
;      -- set this to NIL if you want to turn off "devel-" tags [such as
;         {% devel-dictionanary %} or {% devel-language %}] so they don't do anthing
;
;         note: if *DEVEL-ONLY-P* is NULL then {% devel-dictionary %} and
;               {% devel-value %} tags make sure all their values have been given to the
;               template, complaining if they haven't
;
;    variable:
;    *CATCH-TEMPLATE-ERRORS-P*
;    default value: T
;
;      -- set this to NIL to if you want template errors to invoke the debugger instead
;         of being rendered as error messages in the browser [note: dont forget about
;         HUNCHENTOOT:*CATCH-ERRORS-P* if you are using djula behind hunchentoot]
;
;    variable:
;    *RUN-RULEBOOK-TESTS-P*
;    default value: T
;
;      -- set to NIL to not run the tests inside rulebooks when {% rulebook %} tags are
;         seen
;
;    variable:
;    *TEMPLATE-FOLDER*
;    default value: None! needs to be set by the programmer
;
;      -- the root folder containing your templates. used when finding other templates,
;         dictionaries, rulebooks, etc
;
;    variable
;    *ALLOW-INCLUDE-ROOTS*
;    default value: ()
;
;      -- a list of strings or pathname objects  representing allowed prefixes for the
;         {% ssi %} template tag. This is a security measure, so that template authors
;         can’t access files that they shouldn’t be accessing.
;
;         For example, if *ALLOWED-INCLUDE-ROOTS* is ("/home/html" "/var/www"), then
;         {% ssi "/home/html/foo.txt" %} would work, but {% ssi "/etc/passwd" %}
;         wouldn’t.
;
;    variable
;    *TEMPLATE-STRING-IF-INVALID*
;    default value: NIL
;
;      -- A string or NIL. If is a string, then that string will print when a variable
;         is looked up that doesn't exist. If is NIL, then a template error will be
;         thrown, which will throw and error if *CATCH-TEMPLATE-ERRORS-P* is NIL
;         [possibly entering the debugger]. if *CATCH-TEMPLATE-ERRORS-P* is non-NULL
;         then an error message will be printed in the user's browser when the template
;         is rendered [this is the default behavior]
;
;    variable
;    *TEMPLATE-EVAL*
;    default value: T
;
;      -- set to NIL if you want to turn off "lisp" filters and {% lisp %} tags. they
;         throw a template error when *TEMPLATE-EVAL* is NIL
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; everything below this point is internal documentation for core.lisp
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; INTERNAL DOCUMENTATION
;
; to sum it up:
;
;    (compile-template path)
;
; is something a bit more than:
;
;    (mapcar 'compile-token (process-tokens (parse-template-string (slurp-utf-8-file path))))
;
; where the string
;
;   "foo {{var}} bar {# comment #} baz {% tag %}"
;
; would be parsed by PARSE-TEMPLATE-STRING to the list of tokens
;
;   ((:string "foo ")
;    (:unparsed-variable "var")
;    (:string " bar ")
;    (:comment " comment ")
;    (:string " baz ")
;    (:unparsed-tag " tag "))
;
; and this list would be transformed by PROCESS-TOKENS to something like:
;
;   ((:string "foo ")
;    (:variable (:var))
;    (:string " bar ")
;    (:string " baz ")
;    (:tag :tag))
;
; [use DEF-TOKEN-PROCESSOR to extend PROCESS-TOKENS]
;
; mapping COMPILE-TOKEN on this list would turn it into a list of thunks
;
;   (#<FUNCTION (LAMBDA ())> 
;    #<FUNCTION (LAMBDA ())> 
;    #<FUNCTION (LAMBDA ())> 
;    #<FUNCTION (LAMBDA ())> 
;    #<FUNCTION (LAMBDA ())>)
;
; [use DEF-TOKEN-COMPILER to extend COMPILE-TOKEN]
;
; the function returned by COMPILE-TEMPLATE would FUNCALL these thunks, PRINC-TO-STRING
; and the results and CONCATENATE them together
;
;   style note: things like tag, filter, processor, compilers, etc. are generally
;               internally represented as keywords and their definitions saved
;               in their symbol plist

(in-package :djula)

; configure these at runtime

(defvar *language* :english)

(defvar *devel-only-p* nil)

(defvar *catch-template-errors-p* t)

(defvar *run-rulebook-tests-p* t)

(defvar *allow-include-roots* ())

(defvar *template-string-if-invalid* nil) ;or string

(defvar *template-eval* t)

; special

(defvar *dictionary-regexps* (list "\\.lisp$" "\\.cl$" "\\.sexp$")
"
if a file's DJULA-UTIL:FILENAME matches one of these sexps and not any of
*DEVEL-DICTIONARY-REGEXPS* then it is recognized as a dictionary file.

note that this is a _compile_time_ variable and you must recompile \"dictionary.lisp\"
for changes to its value to be noticed
")

(defvar *devel-dictionary-regexps* (list "devel.*\\.lisp" "devel.*\\.cl" "devel.*\\.sexp")
"
if a file's DJULA-UTIL:FILENAME matches one of these sexps then it is recognized as
being a devel dictionary file.

note that this is a _compile_time_ variable and you must recompile \"dictionary.lisp\"
for changes to its value to be noticed
")

(defvar *template-folder*)

(defvar *template-path*)

(defvar *variable-plist*)

(defvar *dictionaries*)

(defvar *devel-dictionaries*)

(defvar *block-alist*)

(defvar *link-files*)

(defvar *js*)

(defvar *ffc*)

; templates used internally

(defparameter *source-folder* (asdf:component-pathname (asdf:find-component (asdf:find-system :djula)
									    "djula")))

(defparameter *internal-template-folder*
  (merge-pathnames "templates/" *source-folder*))

(defparameter *template-api-documentation-template*
  (merge-pathnames "template-api-documentation.html" *internal-template-folder*))

; util

(defmacro aif (test &optional (then 'it) else)
  `(let ((it ,test))
     (if it
	 ,then
	 ,else)))

(defun .keyword (thing)
  (values (intern (string-upcase (string thing)) :keyword)))

(defun .funcall-and-concatenate (thunks)
  (apply 'concatenate 'string (mapcar 'princ-to-string (mapcar 'funcall thunks))))

(defun .getf (place indicator)
  "like GETF but returns a second value indicating the lookup's success or failure (like GETHASH's PRESENT-P)"
  (when place
    (destructuring-bind (k v . rest) place
      (if (eql k indicator)
	  (values v place)
	  (.getf rest indicator)))))

; template paths

(defun .template-path (path)
  (let ((string (namestring path)))
    (ecase (mismatch "/" string :test 'string=)
      ((nil) *template-folder*)
      ((0) (merge-pathnames string (directory-namestring *template-path*)))
      ((1) (merge-pathnames (subseq string 1) *template-folder*)))))

(defun template-path (path &key dont-check)
  "returns the file or folder pointed to by the template path `PATH', where the root
template path / is seen as *TEMPLATE-DIRECTORY* and relative template paths take off
at the base of *TEMPLATE-PATH* (the current template). returns NIL if the file doesn't
exist unless `DONT-CHECK' is T"
  (if dont-check
      #1=(.template-path path)
      (cl-fad:file-exists-p #1#)))

; error

(defmacro with-template-error (recovery-form &body body)
  "executes `BODY', returning it's results. if there is an error in `BODY' and
*CATCH-TEMPLATE-ERRORS-P* is T, then the debugger is not invoked: instead
`RECOVERY-FORM' is evaluated and it's results returned."
  (let ((e (gensym "error")))
    `(handler-case (progn ,@body)
       (error (,e) (if *catch-template-errors-p*
		       ,recovery-form
		       (error ,e))))))

(defun template-error-string (fmt-string &rest fmt-args)
  "creates a fancy error string to display error information in the browser"
  (concatenate 'string "{# Error: " (apply 'format nil fmt-string fmt-args) " #}"))

; parser
  
(defun .get-closing-delimiter (type)
  (ecase type
    (:comment "#}")
    (:unparsed-variable "}}")
    (:unparsed-dictionary-variable "_}")
    (:unparsed-tag "%}")))

(defun .split-template-string (string start)
  (let (({ (position #\{ string :start start :test 'char=)))
    (if (null {)
	(if (< start (length string))
	    (list `(:string ,(subseq string start))))
	(if (> { start)
	    (cons `(:string ,(subseq string start {))
		  (.split-template-string string {))
	    (let* ((next (char string (1+ {)))
		   (type (case next
			   (#\# :comment)
			   (#\{ :unparsed-variable)
			   (#\_ :unparsed-dictionary-variable)
			   (#\% :unparsed-tag)
			   (otherwise :not-special))))
	      (ecase type
		((:comment :unparsed-variable :unparsed-dictionary-variable :unparsed-tag)
		 (let ((end (search (.get-closing-delimiter type)
				    string
				    :start2 (1+ {))))
		   (if (null end)
		       (list `(:string ,(subseq string start)))
		       (cons (list type (subseq string (+ 2 {) end))
			     (.split-template-string string (+ 2 end))))))
		(:not-special (cons `(:string "{") (.split-template-string string (1+ start))))))))))

(defun parse-template-string (string)
  (.split-template-string string 0))

; token processor

(defmacro def-token-processor (name args rest-var &body body)
  `(setf (get ,name 'token-processor)
	 (m (,rest-var ,@args) ,@body)))

(defun .process-token (token rest-token-list)
  (destructuring-bind (name . args) token
    (let ((f (get name 'token-processor)))
      (if (null f)
	  (cons token (process-tokens rest-token-list))
	  (with-template-error (cons `(:string ,(template-error-string "There was an error processing the token ~A" token))
				     (process-tokens rest-token-list))
	    (apply f rest-token-list args))))))

(defun process-tokens (tokens)
  (if tokens
      (.process-token (first tokens) (rest tokens))))

; token compiler

(defmacro def-token-compiler (name args &body body)
  `(setf (get ,name 'token-compiler)
	 (m ,args ,@body)))

(defun compile-token (token)
  (destructuring-bind (name . args) token
    (let ((compiler (get name 'token-compiler)))
      (if (null compiler)
	  (let ((s (template-error-string "Unknown token ~A" name)))
	    (f0 s))
	  (with-template-error (f0 (template-error-string "There was an error compiling the token ~A" name))
	    (let ((f (apply compiler args)))
	      (assert (functionp f)
		      nil
		      "Compiling the token ~A did not return a function"
		      name)
	      (f0 (with-template-error (template-error-string "There was an error rendering the token ~A" name)
		    (funcall f)))))))))

; basic token: comments

(def-token-processor :comment (comment-string) rest
  ":COMMENT tokens are removed by PROCESS-TOKENS"
  (declare (ignore comment-string))
  (process-tokens rest))

; basic token: strings

(def-token-processor :string (string) unprocessed
  "adjacent :STRING tokens are concatenated together by PROCESS-TOKENS as a small optimization"
  (let ((processed (process-tokens unprocessed)))
    (if (or (null processed)
	    (not (eql (caar processed) :string)))
	`((:string ,string) ,@processed)
	`((:string ,(format nil "~A~A" string (second (first processed))))
	  ,@(rest processed)))))

(def-token-compiler :string (string)
  ":STRING tokens compile into a function that simply returns the string"
  (constantly string))

; API

(defun compile-template (path &key return-bytes ; depricated in favor of `RETURN-FORMAT'
			           (return-format :string) ;or :OCTETS
			           (template-folder *template-folder*)
			           ffc)
  (compile-template-string (djula-util:slurp-utf-8-file path)
			   :template-path path
			   :return-format (if (or return-bytes
						  (eql return-format :octets))
					      :octets
					      :string)
			   :template-folder template-folder
			   :ffc ffc))

(defun .compile-template-string (string &key return-bytes)
  (if return-bytes
      (let ((fn (.compile-template-string string)))
	(f0 (trivial-utf-8:string-to-utf-8-bytes (funcall fn))))
      (let* ((fs (mapcar 'compile-token (process-tokens (parse-template-string string)))))
	(f0 (.funcall-and-concatenate fs)))))

(defun compile-template-string (string &key (template-path *template-path*)
				            return-bytes ; depricated in favor of
				                         ; `RETURN-FORMAT'
				            (return-format :string) ; or :OCTETS
				            (template-folder *template-folder*)
				            ffc)
  (if return-bytes
      (warn "<<<the `RETURN-BYTES' argument to COMPILE-TEMPLATE-STRING is depricated. please use `RETURN-FORMAT' instead>>>"))
  (let* ((*template-folder* template-folder)
	 (*template-path* template-path)
	 *block-alist*
	 *link-files*
	 (*ffc* ffc)
	 (fn (.compile-template-string string
				       :return-bytes (or return-bytes
							 (eql return-format :octets)))))
    (values (f (&rest *variable-plist* &key &allow-other-keys)
	      (let ((*template-folder* template-folder)
		    (*template-path* template-path)
		    *dictionaries*
		    *devel-dictionaries*
		    *js*
		    (*ffc* ffc)
		    (*language* *language*)) ;; this is rebound so that
		                             ;; {% devel-language %} can just SETF
		                             ;; *LANGUAGE*
		(funcall fn)))
	    (mapcar 'namestring *link-files*))))
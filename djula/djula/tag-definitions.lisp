; this file contains the source code to the tags documented in "tag-documentation.lisp"
; if you change anything in this file make damned sure to update
; "tag-documentation.lisp" accordingly!
;
; note: see "tag.lisp" for an explenation of how imlementing new tags work

(in-package :djula)

; util

(defmacro .with-file-handler ((string-var template-path) &body body)
  "evaluates `BODY' with `STRING-VAR' bound to a string representing the contents of
the file pointed to be the template-path `TEMPLATE-PATH', returning it's results.
if there is an error while binding `STRING-VAR' and *CATCH-TEMPLATE-ERRORS-P* is T then
it returns a function that is suitable output for the body of a DEF-TOKEN-COMPILER
form that returns some debugging info."
  (let ((path (gensym "template-path"))
	(real-path (gensym "real-path")))
    `(let* ((,path ,template-path)
	    (,real-path (template-path ,path)))
       (if (null ,real-path)
	   (constantly (template-error-string "The file ~S does not exist" ,path))
	   (with-template-error (constantly (template-error-string "There was an error opening the file ~A. Perhaps an encoding error?" ,real-path))
	     (let ((,string-var (djula-util:slurp-utf-8-file ,real-path)))
	       ,@body))))))

; definitions:

;;; NEW

; css / emit-css

(defvar *css*)

(def-unparsed-tag-processor :css (string) rest
  `((:parsed-css ,string) ,@(process-tokens rest)))

(def-token-compiler :parsed-css (string)
  (pushnew string *css* :test 'equal)
  "")

(def-tag-compiler :emit-css ()
  (apply 'concatenate
	 'string
	 (mapcar (f_ (format nil "~%<script type='text/javascript' src=~A></script>" _))
		 *css*)))

; block / endblock / extends

(def-delimited-tag :block :endblock :parsed-block)

(def-token-compiler :parsed-block ((name) . block-tokens)
  (let ((fs (mapcar 'compile-token
		    (if #1=(assoc name *block-alist*)
			(rest #1#)
			block-tokens))))
    (f0 (.funcall-and-concatenate fs))))

(def-tag-processor :extends (template-path) rest-tokens
  (let ((real-path (template-path template-path)))
    (pushnew real-path *link-files* :test 'equal)
    (if (null real-path)
	`((:string ,(template-error-string "Cannot extend the template ~A because the file ~A does not exists"
					   template-path
					   (template-path template-path :dont-check t))))
	(with-template-error `((:string ,(template-error-string "Cannot extend the template ~A because there was an error opening the file ~A. Perhaps an encoding error?"
								template-path
								real-path)))
	  (let ((string (djula-util:slurp-utf-8-file real-path)))
	    (with-template-error `((:string ,(template-error-string "Cannot extend the template ~A because there was an error parsing the template file ~A"
								    template-path
								    real-path)))
	      (let ((processed     (process-tokens (parse-template-string string))))
		(with-template-error `((:string ,(template-error-string "Cannot extend the template ~A because there was an error parsing this template file"
									template-path)))
		  (let ((extend-blocks (mapcar (f_ (destructuring-bind (<parsed-block> (name) . tokens) _
						     (declare (ignore <parsed-block>))
						     `(,name ,@tokens)))
					       (remove :parsed-block
						       (process-tokens rest-tokens)
						       :key 'first
						       :test-not 'eql))))
		    (setf *block-alist* (append extend-blocks *block-alist*))
		    processed )))))))))

; comment / endcomment

(def-delimited-tag :comment :endcomment :comment-tag)
(def-token-processor :comment-tag (&rest %) rest-tokens
  (declare (ignore %))
  (process-tokens rest-tokens))

; cycle

(def-tag-compiler :cycle (&rest list)
  (let ((circle (copy-list list))
	(unique-id (gensym "cycle")))
    (setf (rest (last circle)) circle)

    (f0 (if (nth-value 1 (.getf *variable-plist* unique-id))
	    #1=(pop (getf *variable-plist* unique-id))
	    (progn (setf *variable-plist* (list* unique-id circle *variable-plist*))
		   #1#)))))

(def-tag-compiler :debug ()
  (f0 (with-output-to-string (out)
	(flet ((% (fmt-string &rest fmt-args)
		 (terpri out)
		 (apply 'format out fmt-string fmt-args))
	       (short (thing)
		 (let ((string (princ-to-string thing)))
		   (if (> (length string) 25)
		       (format nil "~A..." (subseq string 0 22))
		       string))))
	  (macrolet ((with-safe (about &body body)
		       `(with-template-error (% "<<<There was an error gathering debug information about ~A>>>" ,about)
			  ,@body)))
	    (% "<<<START DEBUG INFO>>>")

	    (with-safe "the current language"
	      (% "Language: ~S" (or *language*
				    "none")))

	    (with-safe "whether or not rulebooks are being used"
	      (% "~A" (if *run-rulebook-tests-p*
			  "Running rulebook tests"
			  "<<<Not running rulebook tests>>>")))

	    (with-safe "whether or not template errors are printing to the browser"
	      (% "~A" (if *catch-template-errors-p*
			  "Printing template errors in the browser"
			  "<<<Not printing template errors in the browser>>>")))

	    (with-safe "whether or not {% devel-only %} tags to anything"
	      (% "~A" (if *devel-only-p*
			  "{% devel-only %} tags do stuff"
			  "<<<{% devel-only %} tags do nothing>>>")))

	    (with-safe "the template error string if invalid"
	      (if *template-string-if-invalid*
		  (% "<<<Temlate string if invalid: ~S>>>" *template-string-if-invalid*)))

	    (with-safe "*ALLOW-INCLUDE-ROOTS*"
	      (% "Allow include-roots: ~A" *allow-include-roots*))

	    (with-safe "the template folder"
	      (% "Template folder: ~A" *template-folder*))

	    (with-safe "the tempalate path"
	      (% "Template path: ~A" *template-path*))

	    (with-safe "the variable plist"
	      (if (null *variable-plist*)
		  (% "There is nothing in the variable plist")
		  (progn
		    (% "Variable plist:")
		    (let ((n 0))
		      (labels ((rfn (plist)
				 (when plist
				   (destructuring-bind (k v . rest) plist
				     (% "   ~A. ~A = ~A" (incf n) k (short v))
				     (rfn rest)))))
			(rfn *variable-plist*))))))

	    (with-safe "the current dictionaries"
	      (if (null *dictionaries*)
		  (% "No dictionaries loaded")
		  (progn
		    (% "Dictionaries:")
		    (let ((n 0))
		      (dolist (d *dictionaries*)
			(destructuring-bind (path . alist) d
			  (% "   ~S. ~A" (incf n) (or (format nil "~S" path)
						      "{% dictionary-value %}"))
			  (let ((n 0))
			    (dolist (a alist)
			      (destructuring-bind (var . plist) a
				(% "       ~S. ~S" (incf n) var)
				(labels ((rfn (language-plist)
					   (when language-plist
					     (destructuring-bind (k v . rest) language-plist
					       (% "          language: ~S" k)
					       (% "          value: ~S" (short v))
					       (rfn rest)))))
				  (rfn plist)))))))))))

	    (with-safe "The current devel dictionaries"
	      (if (null *devel-dictionaries*)
		  (% "No devel dictionaries loaded")
		  (progn
		    (% "Devel Dictionaries:")
		    (let ((n 0))
		      (dolist (d *devel-dictionaries*)
			(destructuring-bind (path . plist) d
			  (% "   ~S. ~A" (incf n) (or (format nil "~S" path)
						      "{% devel-path %}"))
			  (labels ((rfn (var-plist)
				     (when plist
				       (destructuring-bind (k v . rest) var-plist
					 (% "      ~A. ~A = ~A" (incf n) k (short v))
					 (rfn rest)))))
			    (rfn plist))))))))

	    (% "<<<END DEBUG INFO>>>"))))))

; devel-language /show-language

; todo: test

(def-tag-compiler :devel-language (name)
  ":DEVEL-LANGUAGE tags are compiled into a function that set *LANGUAGE* to the
keyword version of `NAME' [or NIL if `NAME' is not supplied] if *DEVEL-ONLY-P* is
non-NULL"
  (f0 (if *devel-only-p*
	  (setf *language* name))
      ""))

(def-tag-compiler :show-language ()
  ":SHOW-LANGUAGE tags are compiled into a function that just shows the value of
*LANGUAGE*"
  (f0 *language*))

; dictionary / devel-dictionary / dictionary-value / devel-value

(def-tag-compiler :dictionary (template-path)
  ":DICTIONARY tags compile into a function that pushes a thunk that pushes the list

   (`PATH' . ALIST)

to *DICTIONARIES*, where ALIST is composed of elements that look like:

   (VARIABLE . LANGUAGE-PLIST)

and LANGUAGE-PLIST contains LANGUAGE/VALUE key val pairs]

pushing this stuff to *DICTIONARIES* lets GET-DICTIONARY-VARIABLE know about the
variable definitions contained in the dictionary pointed to by `TEMPLATE-PATH'"
  (with-template-error (f0 (template-error-string "There was an error reading or parsing the contents of the dictionary ~S"
						  template-path))
    (if (not (dictionary-p template-path))

	;; it doesn't look like a dictionary, complain
	(progn (logv:format-log  "<<<the path ~S is not DICTIONARY-P>>>"
				 template-path)
	       (f0 (template-error-string "the path ~S does not name a dictionary! the names of dictionaries must match one of the following regular expressions: ~{~S ~}"
					  template-path
					  *dictionary-regexps*)))

	(aif (cl-fad:file-exists-p (template-path template-path :dont-check t))
	     (progn
	       
	       (pushnew it *link-files* :test 'equal)
	       (if (not (every 'listp (.read-dictionary it)))

		   ;; it doesn't smell like a dictionary, complain
		   (progn (logv:format-log "<<<The dictionary ~A doesn't look like a dictionary>>>" template-path)
			  (f0 (template-error-string "The dictionary ~A doesn't look like a dictionary..." template-path)))

		   ;; use the current contents of the dictionary forever
		   (let ((compiled (compile-dictionary it)))
		     (f0 (push (cons it compiled)
			       *dictionaries*)
			 ""))))

	     ;; the file doesn't exist, complain
	     (progn (logv:format-log "<<<The dictionary ~A doesn't exist" it)
		    (f0 (template-error-string "The dictionary ~A doesn't exist" it)))))))


(def-tag-compiler :devel-dictionary (template-path)
  ":DEVEL-DICTIONARY tags compile into a function that pushes a thunk that pushes the
list

   (`PATH' . PLIST)

to *DEVEL-DICTIONARIES* [where PLIST is composed of variable/value pairs], thus letting
GET-VARIABLES know about the variable definitions contained in the devel dictionary
pointed to by `TEMPLATE-PATH'

Note: definitions contained in a devel-dictionary are only visible to the template if
*DEVEL-ONLY-P* is non-NULL. If *DEVEL-ONLY-P* is NULL, then the dictionary checks to
make sure *VARIABLE-PLIST* contains all its variables, complaining if it doesn't"
  (with-template-error (f0 (template-error-string "There was an error reading or parsing the contents of the devel dictionary ~S"
						  template-path))
    (if (not (devel-dictionary-p template-path))

	;; it doesn't look like a devel dictionary, complain
	(progn (logv:format-log "<<<the path ~S is not DEVEL-DICTIONARY-P"
				template-path)
	       (f0 (template-error-string "the path ~S does not name a devel dictionary! the name of a devel dictionary must match one of the following regular expressions: ~{~S ~}"
					  template-path
					  *devel-dictionary-regexps*)))

	(aif (cl-fad:file-exists-p (template-path template-path :dont-check t))

	     (if (not (evenp (length (.read-dictionary it))))

		 ;; it doesn't smell like a dictionary, complain
		 (progn (logv:format-log "<<<~S does not look like a devel dictionary>>>"
					 template-path)
			(f0 (template-error-string "the devel dictionary ~S does not look like a devel dictionary!"
						   template-path)))

		 (if *ffc*
		 
		     ;; if there is a file function cache for devel dictionaries then always
		     ;; get the contents of the devel dictionary from the cache. This way it
		     ;; will be updated if someone calls FFC-SYNC on the cache
		     (f0 (let ((plist (djula-util:ffc-call *ffc* it)))
			   (push (cons it plist)
				 *devel-dictionaries*)
			   (if *devel-only-p*
			       ""
			       (apply 'concatenate
				      'string
				      ""
				      (.check-devel-dictionary-plist plist)))))
			     
		     ;; otherwise use the current contents of the dictionary forever
		     (let ((plist (.read-dictionary it)))
		       (f0 (push (cons it plist)
				 *devel-dictionaries*)
			   (if *devel-only-p*
			       ""
			       (apply 'concatenate
				      'string
				      ""
				      (.check-devel-dictionary-plist plist)))))))

	     ;; it doesn't exist, complain
	     (progn (logv:format-log "<<<the devel dictionary ~S doesn't exist" template-path)
		    (f0 (template-error-string "The devel dictionary ~A doesn't exist" template-path)))))))

(def-unparsed-tag-processor :dictionary-value (unparsed-string) rest
  (process-tokens `((:parsed-dictionary-value ,@(.read-dictionary-string unparsed-string))
		    ,@rest)))

(def-unparsed-tag-processor :devel-value (unparsed-string) rest
  (process-tokens `((:parsed-devel-value ,@(.read-dictionary-string unparsed-string))
		    ,@rest)))

(def-token-compiler :parsed-dictionary-value (variable . language/value-plist)
  ":DICTIONARY-VALUE tags compile into a function that pushes the list
\(`LANGUAGE' NIL `VARIABLE-NAME' `VALUE') to *DICTIONARIES*, thus letting GET-VARIABLE
known about the variable `VARIABLE-NAME'"
  (let ((d (list nil (.compile-dictionary-variable (cons variable language/value-plist)))))
    (f0 (push d *dictionaries*)
	"")))

(def-token-compiler :parsed-devel-value (&rest variable/value-plist &key &allow-other-keys)
  ":PARSED-DEVEL-VALUE compiles into a thunk that pushes

   (NIL . `VARIABLE/VALUE-PLIST')

to *DEVEL-DICTIONARIES. if *DEVEL-ONLY-P* is NULL then it checks to make sure all the
variables in `VARIABLE/VALUE-PLIST' are in `VARIABLE-PLIST'"
  (let ((d (cons nil variable/value-plist)))
    (f0 (push d *devel-dictionaries*)
	(if *devel-only-p*
	    ""
	    (apply 'concatenate
		   'string
		   ""
		   (.check-devel-dictionary-plist variable/value-plist))))))

; filter

(def-unparsed-tag-processor :filter (unparsed-string) rest
  (process-tokens `((:tag :semi-parsed-filter ,@(rest (.parse-variable-clause (format nil "INTERNAL|~A" unparsed-string))))
		    ,@rest)))

(def-delimited-tag :semi-parsed-filter :endfilter :parsed-filter)

(def-token-compiler :parsed-filter (filters . clauses)
  (let ((fs (mapcar 'compile-token clauses)))
    (f0 (.apply-filters (.funcall-and-concatenate fs) filters))))

; for / endfor

(def-delimited-tag :for :endfor :parsed-for)

(def-token-compiler :parsed-for ((var in %listvar% &optional reversed) . clause)
  (if (not (eql in :in))
      `((:string ,(template-error-string "error parsing {% for %}, it doesn't look like {% for X in XS %}...")))
      (let ((fs (mapcar 'compile-token clause))
	    (phrase (.parse-variable-phrase (string %listvar%))))
	(f0 (multiple-value-bind (list error-string) (.resolve-variable-phrase phrase)
	      (if error-string
		  (with-template-error error-string
		    (error error-string))
		  (let* ((length (length list))
			 (loopfor (list (cons :counter 1)
					(cons :counter0 0)
					(cons :revcounter length)
					(cons :revcounter0 (1- length))
					(cons :first t)
					(cons :last (= length 1))
					(cons :parentloop (get-variable :forloop))))
			 (*variable-plist* (list* var nil
						  :forloop loopfor
						  *variable-plist*))
			 (%set-this-cons% (rest *variable-plist*)))
		    (apply 'concatenate
			   'string
			   (mapcar 'princ-to-string
				   (mapcan (f_ ; set :var for use :for clause
					     (setf (first %set-this-cons%) _)

					; execute :for clause
					     (prog1 (mapcar 'funcall fs)
					 
					; update :loopfor
					       (incf (rest (assoc :counter loopfor)))
					       (incf (rest (assoc :counter0 loopfor)))
					       (decf (rest (assoc :revcounter loopfor)))
					       (decf (rest (assoc :revcounter0 loopfor)))
					       (setf (rest (assoc :first loopfor)) nil
						     (rest (assoc :last loopfor)) (zerop (rest (assoc :revcounter0 loopfor))))))
					   (if reversed
					       (reverse list)
					       list)))))))))))

; if / else / endif

(defun .split-if-clause (clause-tokens)
  "returns two values:

   1. all clause tokens that appear _before_ the first :ELSE token
   2. all clause tokens that appear _after_ the first :ELSE token"
  (let ((else (position-if (f_ (and (eql (first _) :tag)
				    (eql (second _) :else)))
			   clause-tokens)))
    (if else
	(values (subseq clause-tokens 0 else)
		(subseq clause-tokens (1+ else)))
	clause-tokens)))

(def-delimited-tag :if :endif :semi-parsed-if)

(def-token-processor :semi-parsed-if (args . clause) unprocessed
  ":SEMI-PARSED-IF tags are parsed into :PARSED-IF tags. a :PARSED-IF tag looks more 
ike a traditional IF statement [a test, an \"if\" branch, and an \"else\" branch], so
:SEMI-PARSED-IF has to look for the :ELSE token to split up `CLAUSE'"
  (multiple-value-bind (before-else after-else)
      (.split-if-clause clause)
    `((:parsed-if ,args ,before-else ,after-else) ,@(process-tokens unprocessed))))

(defun .compile-logical-statement (statement)
  "takes a \"logical statement\" like you would give {% if %} that has been parsed
into a list of keywords [eg: '(:not :foo) or '(:foo :and :baz) or
`(:foo.bar :or :list.1)] and turns them into a thunk predicate for dispatching the
conditional branching of the {% if %} tag. when called, the function returns two values:

   1. the value returned by resolving the phrase
   2. an error message string if something went wrong [ie, an invalid variable].
      [note: if return value 2 is present, then its probably not safe to consider return
       value 1 useful]"
  (labels ((% (%s)
	     "takes a \"logical statement\" [a list of keywords] minus any :OR or :AND tokens and
returns a list of thunks which, when called, return two values:

   1. whether or not the local statement is true or false
   2. an error-string if something went wrong"
	     (when %s
	       (destructuring-bind (1st . rest) %s
		 (if (eql 1st :not)
		     (let* ((2nd (first rest))
			    (phrase (.parse-variable-phrase (string 2nd))))
		       (cons (f0 (multiple-value-bind (ret error-string) 
				     (.resolve-variable-phrase phrase)
				   (values (not ret) error-string)))
			     (% (rest rest))))
		     (let ((phrase (.parse-variable-phrase (string 1st))))
		       (cons (f0 (.resolve-variable-phrase phrase))
			     (% rest))))))))
    (multiple-value-bind (fs error-string) (% (remove :and (remove :or statement)))
      (let ((and-token-seen-p (find :and statement)))
	(values (f0 (block <f0>
		      (flet ((! (_)
			       "takes a thunks and funcalls it, returning the 1st value. if there is a second value
it treats it as a template error string. see #'%"
			       (multiple-value-bind (ret error-string) (funcall _)
				 (if error-string
				     (return-from <f0> (values ret error-string))
				     ret))))
			(if and-token-seen-p
			    (every #'! fs)
			    (some #'! fs)))))
		error-string)))))

(def-token-compiler :parsed-if (statement then &optional else)
  ":PARSED-IF tags are compiled into a function that executes the {% if %} clause"
  (multiple-value-bind (test error-string)
      (.compile-logical-statement statement)

    (if error-string

	;; there was an error parsing the {% if %} tag [problably an invalid variable]
	;; return a thunk that signals or prints the template error
	(f0 (with-template-error error-string
	      (error error-string)))
	
	  ;; return the function that does the {% if %}
	(let ((then (mapcar 'compile-token then))
	      (else (mapcar 'compile-token else)))
	  (f0 (multiple-value-bind (ret error-string) (funcall test)
		(if error-string
		    (with-template-error error-string
		      (error error-string))
		    (.funcall-and-concatenate (if ret
						  then
						  else)))))))))

; ifchanged

; todo: add else tag, test

(def-delimited-tag :ifchanged :endifchanged :parsed-ifchanged)

(def-token-compiler :parsed-ifchanged (%keywords% . clause)
  (let ((memory (make-list (length %keywords%) :initial-element (gensym "virgin-ifchanged")))
	(fs (mapcar 'compile-token clause))
	(phrases (mapcar '.parse-variable-phrase (mapcar 'string %keywords%))))
    (f0 (block <f0>
	  (let ((new (mapcar (f_ (multiple-value-bind (ret error-string)
				     (.resolve-variable-phrase _)
				   (if error-string
				       (with-template-error (return-from <f0> error-string)
					 (error error-string))
				       ret)))
			     phrases)))
	    (if (every 'equalp memory new)
		""
		(prog1 (.funcall-and-concatenate fs)
		  (replace memory new))))))))

; ifequal / ifnotequal

(defun .process-ifequal-args (unparsed-string)
  (flet ((% (start)
	   (let ((s (string-trim '(#\space #\newline #\tab #\return)
				 (subseq unparsed-string start))))
	     (if (char= (char s 0) #\")
      
		 ;; is a hard-coded string
		 (read-from-string s)

		 ;; is a variable
		 (let ((end (or (position-if (f_ (or (char= _ #\space)
						     (char= _ #\tab)
						     (char= _ #\return)
						     (char= _ #\newline)
						     (char= _ #\")))
					     s)
				(length s))))
		   (values (.parse-variable-phrase (subseq s 0 end))
			   (1+ end)))))))
    (multiple-value-bind (a end-a) (% 0)
      (values a (% end-a)))))

(def-unparsed-tag-processor :ifequal (unparsed-string) rest
  (with-template-error `((:string ,(template-error-string "There was an error parsing the tag {% ifequal %}")))
    (multiple-value-bind (a b) (.process-ifequal-args unparsed-string)
      (process-tokens `((:tag :semi-parsed-ifequal ,a ,b) ,@rest)))))

(def-unparsed-tag-processor :ifnotequal (unparsed-string) rest
  (with-template-error `((:string ,(template-error-string "There was an error parsing the tag {% ifnotequal %}")))
    (multiple-value-bind (a b) (.process-ifequal-args unparsed-string)
      (process-tokens `((:tag :semi-parsed-ifnotequal ,a ,b) ,@rest)))))

(def-delimited-tag :semi-parsed-ifequal :endifequal :almost-parsed-ifequal)
(def-delimited-tag :semi-parsed-ifnotequal :endifnotequal :almost-parsed-ifnotequal)

(def-token-processor :almost-parsed-ifequal ((a b) . clauses) unprocessed
  (multiple-value-bind (before-else after-else) (.split-if-clause clauses)
    (process-tokens `((:parsed-ifequal ,a ,b ,before-else ,after-else) ,@unprocessed))))

(def-token-processor :almost-parsed-ifnotequal ((a b) . clauses) unprocessed
  (multiple-value-bind (before-else after-else) (.split-if-clause clauses)

    ;; from this point on {% ifnotequal %} is just like {% ifequal %},
    ;; the THEN and ELSE clauses have just been switched
    (process-tokens `((:parsed-ifequal ,a ,b ,after-else ,before-else) ,@unprocessed))))

(def-token-compiler :parsed-ifequal (a b then &optional else)
  (flet ((% (x)
	   (etypecase x
	     (string x)
	     (list (.resolve-variable-phrase x)))))
    

    ;; return a thunk that executes the {% ifequal %} clause
    (let ((then (mapcar 'compile-token then))
	  (else (mapcar 'compile-token else)))
      (f0 (multiple-value-bind (a-value a-error-string) (% a)
	    (multiple-value-bind (b-value b-error-string) (% b)

	      (or ;; if there is an error just return the error string
	          a-error-string
		  b-error-string

		  (.funcall-and-concatenate (if (equalp a-value b-value)
						then
						else)))))))))

; include

; todo: test

(def-tag-compiler :include (path)
  "when compiled, :INCLUDE tags first compile the template pointed to by `PATH' then
they compile into a function that simply calls this function with *VARIABLE-PLIST*"
  (aif (template-path path)
       (progn
	 (pushnew (namestring it) *link-files* :test 'equal)
	 (with-template-error (constantly (template-error-string "TThere was an error including the template ~A" it))
	   (if *ffc*
	       (f0 (apply 'djula-util:ffc-call *ffc* it *variable-plist*))
	       (.compile-template-string (djula-util:slurp-utf-8-file it)))))
       (constantly (template-error-string "Cannot include the template ~A because it does not exist." path))))

; js / js-script / emit-js

(def-unparsed-tag-processor :js (string) rest
  `((:parsed-js ,string) ,@(process-tokens rest)))

(def-token-compiler :parsed-js (string)
  (f0 (push (format nil "~%<script type='text/javascript' src=~A></script>" string)
	    *js*)
      ""))

(def-delimited-tag :js-script :endjs-script :semi-parsed-js-script)
(def-token-processor :semi-parsed-js-script  (% . processed-clause) rest
  (declare (ignore %))
  `((:parsed-js-script ,@processed-clause) ,@(process-tokens rest)))

(def-token-compiler :parsed-js-script (&rest clauses)
  (let ((compiled (mapcar 'compile-token clauses)))
    (f0 (push (format nil
		      "~%<script type='text/javascript'>~A</script>"
		      (.funcall-and-concatenate compiled))
	      *js*)
	"")))

(def-tag-compiler :emit-js ()
  (f0 (apply 'concatenate 'string (reverse *js*))))

; lisp

;(reverse (read-from-string (reverse "(+ 4 5) \"foo\"")))

(def-unparsed-tag-processor :lisp (unparsed-string) rest
  (with-template-error (process-tokens `((:string ,(template-error-string "There was an error parsing the lisp statement ~S"
									  unparsed-string))
					 ,@rest))
    (let ((*package* (find-package :cl-user)))
      (process-tokens `((:parsed-lisp ,(read-from-string unparsed-string)) ,@rest)))))

(def-token-compiler :parsed-lisp (sexp)
  (with-template-error (constantly (template-error-string "There was an error compiling the lisp form ~S"
							  sexp))
    (let ((fn (compile nil (coerce `(lambda () ,sexp) 'function))))
      (f0 (if (not *template-eval*)
	      (with-template-error #1=(template-error-string "I can't evaulate the {% lisp %} tag ~A because *TEMPLATE-EVAL* is NIL"
							     sexp)
				   (error #1#))
	      (with-template-error (template-error-string "There was an error executing the lisp form ~A"
							  sexp)
		(funcall fn)))))))

; rulebook

; todo: test

(defun .run-rulebook-tests (rule-plist rulebook-path)
  (with-template-error (template-error-string "There was an error running the tests in the rulebook ~A"
					      rulebook-path)
    (if *run-rulebook-tests-p*
	(labels ((rfn (rules)
		   (if (null rules)
		       ""
		       (destructuring-bind (var test . rest) rules
			 (with-template-error (template-error-string "There was an error validating the variable ~A in the rulebook"
								     var
								     rulebook-path)
			   (if (funcall test (get-variable var))
			       (rfn rest)
			       (return-from .run-rulebook-tests
				 (with-template-error #1=(template-error-string "The variable ~A did not pass the test ~A contained in the rulebook ~A"
										var
										test
										rulebook-path)
						      (error #1#)))))))))
	  (rfn rule-plist)))
    ""))

(def-tag-compiler :rulebook (template-path)
  ":RULEBOOK tags compile into functions that, if *RUN-RULEBOOK-TESTS-P* is T, run all
the tests inside the rulebook pointed to by `PATH'. the rules are a key/val pairs where
the key is the name of a variable (keyword) and the val is a function indicator of a
test that must return NIL if the value of the variable is invalid"
  (logv:format-log "<<<I think rulebooks are broken at the moment, email me if you really need to use them right now... --nick>>>")
  (with-template-error (f0 (template-error-string "There was an error reading or parsing the contents of the rulebook ~S"
						  template-path))
    (aif (cl-fad:file-exists-p (template-path template-path :dont-check t))
	 (if *ffc*

	     (f0 (.run-rulebook-tests (djula-util:ffc-call *ffc* it) it))
	     
	     (let ((plist (.read-dictionary template-path)))
	       (f0 (.run-rulebook-tests plist template-path))))
	 (f0 (template-error-string "The dictionary ~A doesn't exist" template-path)))))

; show dictionary

(def-tag-compiler :show-dictionary (path)
  ":SHOW-FILE tags compile into a function that return the html-escaped contents of
the file pointed to by the template-path `PATH'"
  (.with-file-handler (string path)
    (let ((escaped (djula-util:html-escape string)))
      (constantly escaped))))

; ssi

; todo: test

(def-tag-compiler :ssi (path &optional parse)
  "if `PATH' lives in a folder reckognized by *ALLOW-INCLUDE-ROOTS*, then :SSI tags
compile into a function that return the contents of the file pointed to
by the template-path `PATH'. If `PARSE' is T then the function renders `PATH' as a
template."
  (let ((path-string (namestring path)))
    (if (not #1=(find-if (f_ (eql (mismatch _ path-string :test 'char=)
				  (length _)))
			 *allow-include-roots*))
	(constantly (template-error-string "Cannot SSI to path ~A because ~A is not in a folder recognized by *ALLOW-INCLUDE-ROOTS*. Allowed folders are: ~A"
					   path-string
					   path-string
					   *allow-include-roots*)))
    (format-log "Danger! {% ssi ~A~A~A}"
		path
		(if parse
		    " "
		    "")
		(if parse
		    parse
		    ""))
    (if parse
	(compile-template path :recursivep t)      ; danger: will have screwy *TEMPLATE-FOLDER*!!!!!
	(.with-file-handler (string path)
	  (constantly string)))))

; templatetag

(def-tag-compiler :templatetag (argument)
  ":SHOW-FILE tags compile into a function that return the html-escaped contents of
the file pointed to by the template-path `PATH'"
  (let ((string (case argument
		  (:openblock "{%")
		  (:closeblock "%}")
		  (:openvariable "{{")
		  (:closevariable "}}")
		  (:openbrace "{")
		  (:closebrace "}")
		  (:opencomment "{#")
		  (:closecomment "#}")
		  (:opendictionaryvariable "{_")
		  (:closedictionaryvariable "_}")
		  (otherwise (template-error-string "Unknown templatetag ~A. known template tags are: openblock, closeblock, openvariable, closevariable, openbrace, closebrace, opencomment, closecomment"
						    argument)))))
    (constantly string)))

; url

;; (def-unparsed-tag-processor :url (unparsed-string) rest
;;     (multiple-value-bind (method-name start-args)
;; 	(let ((*read-eval* nil)
;; 	      (*package* (find-package :keyword)))
;; 	  (read-from-string unparsed-string))

;;       (let* ((rest-string (subseq unparsed-string start-args))
;; 	     (method-args (if (find-if 'alphanumericp rest-string)
;; 			      (mapcar '.parse-variable-phrase
;; 				      (split-sequence:split-sequence #\space
;; 								     rest-string
;; 								     :remove-empty-subseqs t)))))
;; 	(process-tokens `((:parsed-url ,method-name ,@method-args) ,@rest)))))

;; (defun .plist-keys (plist)
;;   (when plist
;;     (cons (first plist) (.plist-keys (rest (rest plist))))))

;; (def-token-compiler :parsed-url (method-name . method-args)
;;   (f0 (let ((method (getf *url-method-plist* method-name)))
;; 	(if (null method)
;; 	    (f0 (template-error-string "No known {% url %} method for ~A. Known {% url %} methods are ~A"
;; 				       method-name
;; 				       (.plist-keys *url-method-plist*)))
;; 	    (block <url>
;; 	      (apply method
;; 		     (mapcar (f_ (multiple-value-bind (val error-string)
;; 				     (.resolve-variable-phrase _)
;; 				   (if error-string
;; 				       (return-from <url> error-string)
;; 				       val)))
;; 			     method-args)))))))

; with

;; (def-delimited-tag :parsed-with :endwith :parsed-with)
; this file extends defines the utilities and functions necessary to deal with
; dictionary files and the dictionary cache, as well as extend PROCESS-TOKENS and
; COMPILE-TOKEN to understand :UNPARSED-DICTIONARY-VARIABLE and :DICTIONARY-VARIABLE
; tokens
;
; see "tag-definitions.lisp" for the source of {% dictionary %} and
; {% dictionary-value %}
;
; DICTIONARIES
;
; *DICTIONARIES* is bound to a list of dictionary objects, which represent groups of
; 0 or more variables made available by a {% dictionary %} or {% dictionary-value %}
; tag. dictionary objects are really just lists and not objects at all. the syntax of
; a dictionary object is:
;
;    (PATH . VARIABLE-ALIST)
;
; where variable-alist is filled with elements that look like:
;
;    (VARIABLE-NAME . LANGUAGE/VALUE-PLIST)
;

(in-package :djula)

; reckognizing dictionaries

(let ((scanners (mapcar 'cl-ppcre:create-scanner *devel-dictionary-regexps*)))
  (defun devel-dictionary-p (path)
    "returns non-NULL it `PATH''s filename is matched by a regexp in *DEVEL-DICTIONARY-REGEXPS*"
    (unless (cl-fad:directory-pathname-p path)
      (let* ((filename        (djula-util:filename path))
	     (filename-length (length filename)))
	(some (f_ (funcall _ filename 0 filename-length))
	      scanners)))))

(let ((scanners (mapcar 'cl-ppcre:create-scanner *dictionary-regexps*)))
  (defun dictionary-p (path)
    "returns non-NULL it `PATH''s filename is matched by a regexp in *DICTIONARY-REGEXPS*
and doesn't match the regexps in *DEVEL-DICTIONARY-REGEXPS*"
    (unless (cl-fad:directory-pathname-p path)
      (let* ((filename        (djula-util:filename path))
	     (filename-length (length filename)))
	(and (some (f_ (funcall _ filename 0 filename-length))
		   scanners)
	     (not (devel-dictionary-p path)))))))

; reading/compiling dictionaries

(defun .read-dictionary-string (unparsed-string)
  (let (*read-eval*
	(*package* (find-package :keyword)))
    (values (read-from-string (format nil "(~A)" unparsed-string)))))


(defun .read-dictionary (path)
  (.read-dictionary-string (djula-util:slurp-utf-8-file path)))

(defun .compile-dictionary-variable (list)
  (destructuring-bind (variable &rest language-plist &key &allow-other-keys)
      list
    (cons variable
	  (labels ((rfn (plist)
		     (when plist
		       (destructuring-bind (k v . rest) plist
			 (list* k
				(with-template-error (f0 (template-error-string "There was an error compiling the dictionary variable ~A"
										k))
				  (.compile-template-string v))
				(rfn rest))))))
	    (rfn language-plist)))))

(defun .compile-dictionary-alist (alist)
  (mapcar '.compile-dictionary-variable alist))

(defun compile-dictionary (path)
  (.compile-dictionary-alist (.read-dictionary path)))

(defun compile-devel-dictionary (path)
  (.read-dictionary path))

; getting variables from within dictionaries								 

(defun .get-dictionary-value-from-dictionary-alist (variable alist language)
  (let ((a (assoc variable alist)))
    (if a
	(let ((language-plist (rest a)))
	  (multiple-value-bind (fn present-p) (.getf language-plist language)
	    (if present-p
		(values (funcall fn) present-p)))))))

(defun .get-dictionary-value (variable)
  (dolist (d *dictionaries*)
    (destructuring-bind (path . alist) d
      (multiple-value-bind (v present-p)
	  (.get-dictionary-value-from-dictionary-alist variable
						       (if *ffc*
							   (djula-util:ffc-call *ffc*
										path
										*template-folder*)
							   alist)
						       *language*)
	(if present-p
	    (return-from .get-dictionary-value (values v present-p)))))))

; reading :UNPARSE-DICTIONARY-VARIABLE TOKENS created by {_ dictionary-variables _}

(def-token-processor :unparsed-dictionary-variable (unparsed-string) rest
  ":PARSED-DICTIONARY-VARIABLE tokens are parsed into :DICTIONARY-VARIABLE tokens by PROCESS-TOKENS"
  `((:dictionary-variable ,(let ((*package* (find-package :keyword))
				 *read-eval*)
				(read-from-string unparsed-string)))
    ,@(process-tokens rest)))

; compiling :DICTIONARY-VARIABLE tokens

(def-token-compiler :dictionary-variable (name)

  ;; return a function that finds the value of the variable
  (f0 (with-template-error (template-error-string "There was an error while looking up the dictionary variable ~A"
						  name)
	(multiple-value-bind (v present-p) (.get-dictionary-value name)
	  (if present-p
	      v
	      (or *template-string-if-invalid*
		  (template-error-string "invalid dictionary variable ~A" name)))))))
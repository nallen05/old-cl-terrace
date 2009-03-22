; this file contains code shared between `djula' and `cl-terrace' for interoperability
; or conveneince

(defpackage :djula-util
  (:use :cl :f-underscore)
  (:export ; utf-8 files
	   :slurp-utf-8-file

	   ; html/url encoding
	   :html-escape
	   :url-encode
	   :url-encode-path
	   :url-decode

	   ; filename / foldername
	   :filename
	   :foldername
	   
           ; boring files/directories
	   :*boring-path-regexps*
	   :boring-file-p
	   :non-boring-files
	   :boring-folder-p
	   :non-boring-folders

	   ; ffc
	   :ffc
	   :ffc-init
	   :ffc-get
	   :ffc-call
	   :ffc-link
	   :ffc-get-linked
	   :ffc-needs-syncing-p
	   :ffc-sync
	   :ffc-uncache-deleted))

(in-package :djula-util)

; configure these

(defvar *boring-path-regexps* (list "^\\." "^\\_" "^#" "~$" ".fasl$")
  "if a file's FILENAME or folder's FOLDERNAME matches one of these then it's
considered \"boring\" and ignored by most things.

note that this is a _compile_time_ variable and you must recompile this whole file
for changes to its value to be noticed
")
; utf-8 files

(defun slurp-utf-8-file (p)
  "return the contents of the file pointed to by the pathname designator `P' as a string assumes the file is encoded in :utf-8"
  (with-open-file (in p :element-type '(unsigned-byte 8)
		        :direction :input)
    (trivial-utf-8:read-utf-8-string in :stop-at-eof t)))

; url/html encoding

(defmacro .do-array ((var array &optional return) &body body)
  "like DOLIST but for arrays"
  (let ((n% (gensym "array-index"))
	(a%  (gensym "array")))
    `(let ((,a% ,array))
       (declare (dynamic-extent ,a%))
       (dotimes (,n% (length ,a%) ,return)
	 (let ((,var (aref ,a% ,n%)))
	   ,@body)))))

(defmacro .do-string ((var string &optional return) &body body)
  "like DOLIST but for strings"
  `(.do-array (,var (the string ,string) ,return)
     (declare (type character ,var))
     ,@body))

(defun html-escape (string)
  (with-output-to-string (out)
  "returns an html-escaped version of `STRING'"
  ;; this function was created by modifying HUNCHENTOOT:ESCAPE-FOR-HTML
  (.do-string (c string)
    (case c
      ((#\<) (write-string "&lt;" out))
      ((#\>) (write-string "&gt;" out))
      ((#\") (write-string "&quot;" out))
      ((#\') (write-string "&#039;" out))
      ((#\&) (write-string "&amp;" out))
      (otherwise (write-char c out))))))

(defun url-encode (string)
  "returns a url-encoded version of `STRING'. assumes UTF-8 so be careful if seen around HUNCHENTOOT:URL-DECODE, which probably assumes LATIN-1 by default. see HUCHENTOOT:*DEFAULT-EXTERNAL-FORAT*"
  ;; this function was created by modifying HUNCHENTOOT:URL-ENCODE
  (with-output-to-string (out)
    (.do-string (c string)
      (if (or (char<= #\0 c #\9)
	      (char<= #\a c #\z)
	      (char<= #\A c #\Z)

	      ;; note that there's no comma in there - because of cookies

	      ;;       I don't know why there's no comma because of cookies, it's
	      ;;       what hunchentoot did so I copied it -nick

	      (find c "$-_.!*'()" :test #'char=))
	  (write-char c out)
	  (.do-array (b (trivial-utf-8:string-to-utf-8-bytes (string c)))
	    (format out "%~2,'0x" b))))))

(defun url-encode-path (path)
  (with-output-to-string (out)
    (destructuring-bind (abs/rel . dpath) (pathname-directory path)

      ; maybe initial "/"
      (case abs/rel
	(:absolute (write-char #\/ out))
	(otherwise nil))

      ; the directory path
      (mapc (f_ (write-string (url-encode _) out)
		(write-char #\/ out))
	    dpath)

      ; the name
      (write-string (url-encode (pathname-name path)) out)

      ; maybe type
      (if (pathname-type path)
	  (format out ".~A" (pathname-type path))))))

(defun url-decode (string)
  "returns the a new url-decoded version of `STRING'. assumes UTF-8 so be careful if seen around HUNCHENTOOT:URL-ENCODE, which probably assumes LATIN-1 by default. see HUCHENTOOT:*DEFAULT-EXTERNAL-FORAT*"
  ;; this function was created by modifying HUNCHENTOOT:URL-DECODE
  (let ((vector (make-array (length string)
                            :element-type '(unsigned-byte 8)
                            :fill-pointer 0))
	percent-p
	buff)
    (declare (dynamic-extent vector))
    (dotimes (n (length string) (trivial-utf-8:utf-8-bytes-to-string (make-array (length vector)
										 :element-type '(unsigned-byte 8)
										 :initial-contents vector)))
      (let ((c (char string n)))
	(cond (buff          (vector-push (parse-integer string
							 :start (1- n)
							 :end (1+ n)
							 :radix 16)
					  vector)
			     (setq buff nil))
	      (percent-p     (setq buff t
				   percent-p nil))
	      ((char= c #\%) (setq percent-p t))
	      (t             (vector-push (char-code (case c
						       ((#\+)     #\Space)
						       (otherwise c)))
					  vector)))))))

; filename / foldernames

(defun filename (file)
  "
   (filename \"/foo/bar/baz.txt\")

   -> \"baz.txt\"
"
  (if (pathname-type file)
      (format nil "~A.~A" (pathname-name file) (pathname-type file))
      (pathname-name file)))

(defun foldername (folder)
  "
   (foldername \"/foo/bar/baz/\")

   -> \"baz\"
"
  (first (last (pathname-directory folder))))

; boring files / folders

(let ((scanners (mapcar 'cl-ppcre:create-scanner *boring-path-regexps*)))

  (defun boring-file-p (path)
    "see *BORING-PATH-REGEXPTS*"
    (let* ((filename        (filename path))
	   (filename-length (length filename)))
      (some (f_ (funcall _ filename 0 filename-length))
	    scanners)))

(defun boring-folder-p (path)
  "see *BORING-PATH-REGEXPTS*"
    (let* ((foldername        (foldername path))
	   (foldername-length (length foldername)))
      (some (f_ (funcall _ foldername 0 foldername-length))
	    scanners)))
)

(defun non-boring-files (folder)
  "returns a list of all the non-boring files in `FOLDER'"
  (remove-if 'boring-file-p
	     (remove-if 'cl-fad:directory-pathname-p
			(cl-fad:list-directory folder))))

(defun non-boring-folders (folder)
  "returns a list of all the non-boring folders in `FOLDER'"
  (remove-if 'boring-file-p
	     (remove-if-not 'cl-fad:directory-pathname-p
			    (cl-fad:list-directory folder))))

; ffc

(defstruct ffc
  (function-ht (make-hash-table :test 'equal))
  (link-ht (make-hash-table :test 'equal))
  fn-factories
  cache-directories-p)

(defmethod print-object ((ffc ffc) s)
  (format s
	  "#<ffc ~A ~A>"
	  (hash-table-count (ffc-function-ht ffc))
	  (ffc-fn-factories ffc)))

(defun ffc-init (fn-factories &key cache-directories-p)
  (make-ffc :fn-factories fn-factories
	    :cache-directories-p cache-directories-p))

(defun ffc-get (ffc path)
  (gethash (namestring path) (ffc-function-ht ffc)))

(defun ffc-call (ffc path &rest args)
  (let ((% (ffc-get ffc path)))
    (if %
	(apply (first %) args))))

(defun ffc-link (ffc path paths)
  (if paths
      (setf (gethash (namestring path) (ffc-link-ht ffc))
	    (mapcar (f_ (cons _ (rest (ffc-get ffc _))))
		    paths))))

(defun ffc-get-linked (ffc path)
  (gethash (namestring path) (ffc-link-ht ffc)))

(defun ffc-needs-syncing-p (ffc path)
  (let ((% (gethash (namestring path) (ffc-function-ht ffc)))
	(file-write-date (file-write-date path)))
    (if (null %)
	(values file-write-date t)
	(destructuring-bind (fn . known-write-date) %
	  (declare (ignore fn))
	  (if (or (> file-write-date
		     known-write-date)
		  (find-if (f_ (destructuring-bind (p . known-wd) _
				 (or (not known-wd)
				     (< known-wd
					(file-write-date p)))))
			   (ffc-get-linked ffc path)))
	      file-write-date)))))


(defun ffc-sync (ffc path)
  (flet ((syncit (p)
	   (let ((factory (rest (assoc-if (f_ (funcall _ path))
					  (ffc-fn-factories ffc)))))
	     (if factory
		 (multiple-value-bind (needs-syncing 1st-time-seen)
		     (ffc-needs-syncing-p ffc p)
		   (if needs-syncing
		       (multiple-value-bind (fn links)
			   (funcall factory p 1st-time-seen)
			 (prog1
			     (setf (gethash (namestring p) (ffc-function-ht ffc))
				   (cons fn needs-syncing))
			   (mapc (f_ (ffc-sync ffc _)) links)
			   (ffc-link ffc p links)))))))))
    (values ffc
	    (prog1

		;; sync
		(if (cl-fad:directory-pathname-p path)
		    (if (ffc-cache-directories-p ffc)
			(syncit path))
		    (syncit path))

	      ;; sync decendents
	      (when (cl-fad:directory-pathname-p path)

		(dolist (f (non-boring-files path))
		  (ffc-sync ffc f))

		(dolist (f (non-boring-folders path))
		  (ffc-sync ffc f)))))))

(defun ffc-uncache-deleted (ffc &key hook)
  (flet ((doit (ht)
	   (let (delete)
	     (maphash (f (p v)
			(declare (ignore v))
			(if (not (if (cl-fad:directory-pathname-p p)
				     (cl-fad:directory-exists-p p)
				     (cl-fad:file-exists-p p)))
			    (push p delete)))
		      ht)
	     (mapc (f_ (if hook
			   (funcall hook _))
		       (remhash _ ht))
		   delete))))
    (doit (ffc-function-ht ffc))
    (doit (ffc-link-ht ffc))
    (values)))
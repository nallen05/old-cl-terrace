
(in-package :cl-terrace)

; util

(defmacro aif (test &optional (then 'it) else)
  `(let ((it ,test))
     (if it
	 ,then
	 ,else)))

(defun .getf (place indicator)
  "like GETF but returns a second value indicating the lookup's success or failure
[like GETHASH's PRESENT-P return value]"
  (when place
    (destructuring-bind (k v . rest) place
      (if (eql k indicator)
	  (values v place)
	  (.getf rest indicator)))))

(defun .getf-equal (place indicator)
  "like GETF but uses EQUAL instead of EQL and returns a second value indicating the
lookup's success or failure [like GETHASH's PRESENT-P return value]"
  (when place
    (destructuring-bind (k v . rest) place
      (if (equal k indicator)
	  (values v place)
	  (.getf-equal rest indicator)))))

(defun .matches-prefix (prefix string)
  "
   (.matches-prefix \"foo-\" \"foo-bar\")

   -> \"bar\"

   (.matches-prefix \"foo-\" \"baz-bar\")

   -> NIL
"
  (let ((prefix-length (length prefix)))
    (when (>= (length string) prefix-length)
      (let ((m (mismatch prefix string :test 'char=)))
	(if (or (null m)
		(= m prefix-length))
	    (subseq string m))))))

(defun .in-package-form-p (form)
  "if `FORM' is a list that looks like

   (in-package <package-designator>)

then returns <package-designator> otherwise returns NIL
"
  (and (listp form)
       (null (cddr form))
       (eql (first form) 'in-package)
       (second form)))

(defun .read-from-string-understanding-in-package-forms (string)
  "like READ-FROM-STRING but returns a list of all forms in the string, not just the
first form. understands toplevel IN-PACKAGE forms while reading, but discards them after
they are read"
  (let (forms
	(done (gensym "done"))
	(*package* (find-package :cl-user)))
    (with-input-from-string (in string)
      (do ((form #1=(read in nil done) #1#))
	  ((eql form done) (nreverse forms))
	(if (.in-package-form-p form)
	    (setq *package* (find-package (.in-package-form-p form)))
	    (push form forms))))))
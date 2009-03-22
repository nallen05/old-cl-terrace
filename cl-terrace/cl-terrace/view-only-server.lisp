
(in-package :cl-terrace)

; VIEW ONLY MODE

(defun .get-file-links (folder &key test)
  "produces a list of paths to files contained in `FOLDER' and its sub-folders. the
paths  are absolute paths that start at `FOLDER'. Ignores files and folders that are
boring in the eyes of DJULA-UTIL:BORING-FILE-P and
DJULA-UTIL:BORING-FOLDER-P"
  (when (cl-fad:directory-exists-p folder)
    (let (templates)
      (labels ((rfn (p)
		 (if (cl-fad:directory-pathname-p p)
		     (if (not (djula-util:boring-folder-p p))
			 (mapc #'rfn (cl-fad:list-directory p)))
		     (if (not (djula-util:boring-file-p p))
			 (if (or (not test)
				 (funcall test p))
			     (push (format nil
					   "/~A"
					   (namestring (enough-namestring p folder)))
				   templates))))))
	(rfn folder)
	(nreverse templates)))))

(defun .create-view-only-index-page-handler (&key project-name template-folder static-folder)
  "calling this function creates a Hunchentoot handler for the Djula Server Index Page."
  (flet ((file-links (folder &key test)
	   (mapcar (f_ (list (cons :normal _)
			     (cons :url-safe (djula-util:url-encode-path _))))
		   (.get-file-links folder :test test))))
    (let ((template (djula:compile-template *view-only-index-page-template*
					   :template-folder *internal-template-folder*)))
      (f0 ;; set content type stuff
	  (setf (hunchentoot:reply-external-format) (flexi-streams:make-external-format :utf-8)
		(hunchentoot:content-type) "text/html; charset=utf-8")

	  ;; get files
	  (let ((static-directory-files (file-links static-folder))
		(template-directory-files (file-links template-folder))
		 templates
		 dictionaries
		 devel-dictionaries)
	    (dolist (% template-directory-files)
	      (let ((p (rest (assoc :normal %))))
		(cond ((djula:dictionary-p p)       (push % dictionaries))
		      ((djula:devel-dictionary-p p) (push % devel-dictionaries))
		      (t                            (push % templates)))))

	    ;; render template
	    (let ((djula:*devel-only-p* t))
	      (funcall template
		       :project-name project-name
		       :template-directory (namestring template-folder)
		       :templates templates
		       :dictionaries dictionaries
		       :devel-dictionaries devel-dictionaries
		       :static-directory (namestring static-folder)
		       :static-files static-directory-files)))))))

(defun .create-view-only-dispatcher (project-name)
  (let ((static-dispatcher (if #1=(get project-name 'static-folder)
			       (hunchentoot:create-folder-dispatcher-and-handler "/" #1#)))
	(index-handler (.create-view-only-index-page-handler :template-folder (get project-name 'template-folder)
							     :static-folder   (get project-name 'static-folder)
							     :project-name    project-name)))
    (f (hunchentoot:*request*)
      (let ((*terrace-project* project-name)
	    (path (hunchentoot:script-name hunchentoot:*request*)))

	;; maybe update DJULA:*LANGUAGE* from "language" HTTP GET parameter
	(aif (g "language")
	     (setf djula:*language* (find-symbol (string-upcase it) :keyword)))

	(case (mismatch "/" path)
	  ((nil 0)  index-handler)
	  (otherwise (if (not (mismatch "/template-api.html" path))
			 (f0 ;; setup utf-8 stuff
			     (setf (hunchentoot:reply-external-format) *default-reply-external-format*
				   (hunchentoot:content-type) (.overruled-mime-type "/foo.html"))

			   ;; template api documentstion
			   (djula:build-html-documentation-from-source))

			 (or ;; maybe dispatch template/dictionary handler
			     (aif (cl-fad:file-exists-p (merge-pathnames (subseq path 1) (get project-name 'template-folder)))
				  (f0
				    
				    (if (or (djula:dictionary-p path)
					    (djula:devel-dictionary-p path))

					(progn 
					  
					  ;; setup utf-8 stuff
					  (setf (hunchentoot:reply-external-format) *default-reply-external-format*
						(hunchentoot:content-type) (.overruled-mime-type "foo.txt"))
					  (djula-util:slurp-utf-8-file it))

					;; render template
					(let ((djula:*devel-only-p* t))
					  (setf (hunchentoot:content-type)         (.overruled-mime-type it)
						(hunchentoot:reply-external-format) *default-reply-external-format*)
					  (funcall (djula:compile-template it :template-folder (get project-name 'template-folder)))))))

			     ;; maybe dispatch static-file-handler
			     (if static-dispatcher
				 (funcall static-dispatcher hunchentoot:*request*))))))))))

(defun !start-terrace-server/view-only (project-name &rest hunchentoot-kwds)
  " like !START-TERRACE-SERVER, but goes into a special \"view only\" mode. Go to the
the root url:

   /

to see the index page.

Go to

   /template-api.html

to see the Djula template API documentation"
  (apply '.start-terrace-server
	 project-name
	 (f0
;	   ;; sync project
;	     (sync-terrace project-name
;			   :sync-terrace-folder nil)

	   ;; create handler
	   (logv:format-log "compiling View Only Index Page handler")
	   (let ((handler (.create-view-only-dispatcher project-name)))

	     ;; update dispatch-table
	     (logv:format-log "Updating Hunchentoot Dispatch Table")
	     (setf hunchentoot:*dispatch-table*
		   (cons handler hunchentoot:*dispatch-table*))))
	 hunchentoot-kwds))
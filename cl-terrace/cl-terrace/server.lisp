
(in-package :cl-terrace)


(defun .start-terrace-server (project-name action-thunk &rest hunchentoot-kwds)
  "
   1. logs a bunch of stuff about the Terrace project PROJECT-NAME
   2. performs `ACTION-THUNK'
   3. starts a Hunchentoot server using `HUNCHENTOOT-KWDS' as keyword arguments, setting
      *TERRACE-SERVER* to the resultant hunchentoot server [so it can be stopped by
      !STOP-TERRACE-SERVER]
"

  ;; run all sorts of checks to make sure some things aren't seriously wrong
  (if *terrace-server*
      (error "A Terrace server is already running [see *TERRACE-SERVER*]. run !STOP-TERRACE-SERVER to stop it before trying to start it again")
      (if (not (get project-name 'terrace-project-p))
	  (error "The symbol ~S does not name Terrace project. You need to run DEF-TERRACE to define a Terrace project before you can start it]"
		 project-name)
	  (let ((site-folder (get project-name 'site-folder)))
	    (if (not (cl-fad:directory-exists-p site-folder))
		(error "Cannot start Terrace project ~S. There is no \"site/\" folder at ~A"
		       project-name
		       site-folder)

		;; nothing seems to be seriously wrong
		(progn

		  ;; log DEF-TERRACE keyword args
		  (let ((publish-static-p (get project-name 'devel-only-p)))
		    (logv publish-static-p))
		    
		  ;; log "template/", "terrace/", and "static/" folders
		  (flet ((log-folder (name)
			   (let ((folder (get project-name name)))
			     (if (and folder
				      (cl-fad:directory-exists-p folder))
				 (logv:format-log "There are ~A non-boring file~:*~p and ~A non-boring directories in the ~A"
						  (length (djula-util:non-boring-files folder))
						  (length (djula-util:non-boring-folders folder))
						  name)
				 (logv:format-log "<<<There is no ~A!!>>>"
						  name)))))
		    (log-folder 'static-folder)
		    (log-folder 'template-folder)
		    (log-folder 'terrace-folder))

		  ;; perform action
		  (funcall action-thunk)

		  ;; start-server
		  (logv:format-log "Starting Hunchentoot")
		  (setf *terrace-server* (apply 'hunchentoot:start-server hunchentoot-kwds))

		  ;; warn if LOGV:*LOG-OUTPUT* is t
		  (when (eql logv:*log-output* t)
		    (logv:format-log "-------------------------------------------------------------------------")
		    (logv:format-log "LOGV:*LOG-OUTPUT* is T, which means you won't be able to see stuff logged")
		    (logv:format-log "while replying to HTTP requests in the repl because Hunchentoot rebinds")
		    (logv:format-log "*STANDARD-OUTPUT*. Run (SETF LOGV:*LOG-OUTPUT* *STANDARD-OUTPUT*) to see")
		    (logv:format-log "logs in the current repl when replying to HTTP requests, or better yet,")
		    (logv:format-log "set LOGV:*LOG-OUTPUT* to the path of a log file")
		    (logv:format-log "-------------------------------------------------------------------------"))

		  *terrace-server*))))))

(defun !start-terrace-server (project-name &rest hunchentoot-kwds)
  "
   1. logs a bunch of stuff
   2. Syncs the \"site/\" folder of the project `PROJECT-NAME'
   2. starts a Hunchentoot server using `HUNCHENTOOT-KWDS' as keyword arguments
   3. publishes the Terrace project `PROJECT-NAME'.
"
  (apply '.start-terrace-server
	 project-name
	 (f0 ;; sync project
	     (sync-terrace project-name)

	     ;; update dispatch-table
	     (logv:format-log "Updating Hunchentoot Dispatch Table")
	     (setf hunchentoot:*dispatch-table*
		   (list* project-name (remove project-name hunchentoot:*dispatch-table*))))
	 hunchentoot-kwds))
	
(defun !stop-terrace-server ()
  "stops the Hunchentoot server created by !START-TERRACE-SERVER"
  (if *terrace-server*
      (progn
	(hunchentoot:stop-server *terrace-server*)
	(setf *terrace-server*             nil
	      hunchentoot:*dispatch-table* (list 'hunchentoot:default-dispatcher)))
      (error "There is no terrace server running [see *TERRACE-SERVER*]")))
; `Logv' is a simple logging utility for Common Lisp
;
; The most useful form is LOGV, a macro that behaves like PROGN except that it writes
; each form and its return value to the log.
;
; the following code:
;
;    (logv (+ 1 (logv (+ 1 1)))
;          (+ 10 10)
;          (* 64 64))
;
; will return 4096 and might write something like the following lines to the log:
;
;    ;[repl-thread] [01:06:2008] [14:07:53] (+ 1 1) -> 2
;    ;[repl-thread] [01:06:2008] [14:07:53] (+ 1 (LOGV (+ 1 1))) -> 3
;    ;[repl-thread] [01:06:2008] [14:07:53] (+ 10 10) -> 20
;    ;[repl-thread] [01:06:2008] [14:07:53] (* 64 64) -> 4096
;
; LOGVS is like LOGV but understands multiple return values. FORMAT-LOG lets you
; write to the log with a format string and arguments [like FORMAT].
; WRITE-STRING-TO-LOG simply writes a string to the log.
;
; where these functions/macros write to is controled by the variabe *LOG-OUTPUT*
;
; if *LOG-OUTPUT* is
;
;   -T: then things that write to the log write to *STANDARD-OUTPUT*
;   -NIL: then things that normally write to the log don't do anything
;   -a stream: then things that write to the log write to the stream
;   -a pathname or a string [a pathname designator]: then things that write to the
;    log write to the file pointed to by the string/pathname. the file is created if
;    it doesn't already exist. if there are special characters they are encoded in
;    UTF-8
;
; the config variables *LOG-PREFIX-STING*, *LOG-THREAD-NAME-P*, *LOG-DATE-P*, and
; *LOG-TIME-P* let you configure the informative garbage that gets prepended to the
; stuff written to the log by LOGV, LOGVS, and FORMAT-LOG
;
; license: BSD. See "license.txt"
; 
; todo: -log rotation script
;       -different log levels

(in-package :logv)

; configure these

(defvar *log-output* t
  "if *LOG-OUTPUT* is

   -T: then things that write to the log write to *STANDARD-OUTPUT*
   -NIL: then things that normally write to the log don't do anything
   -a stream: then things that write to the log write to the stream
   -a pathname or a string [a pathname designator]: then things that write to the log
    write to the file pointed to by the string/pathname. the file is created if it
    doesn't already exist. if there are special characters then they are encoded in
    UTF-8")

(defvar *log-prefix-string* (format nil "~%;")
  "the leftmost string prepended to the string sent to WRITE-STRING-TO-LOG-FILE by
FORMAT-LOG")

(defvar *log-thread-name-p* t
  "set to NIL if you don't want FORMAT-LOG to log the name of the current")

(defvar *log-date-p* t
  "set to NIL if you don't want FORMAT-LOG to log the current date")

(defvar *log-time-p* t
  "set to NIL if you don't want FORMAT-LOG to log the current time")

; special

(defvar *log-mutex%* (make-lock "log stream")
  "prevents multiple threads from writing to *LOG-OUTPUT* at the same time")

; writing to the log file

(defun write-string-to-log (string)
  "writes `STRING' to the log"
  (when *log-output*
    (with-lock-held (*log-mutex%*)
      (if (or (stringp *log-output*)
	      (pathnamep *log-output*))
	  (with-open-file (out *log-output*
			       :element-type '(unsigned-byte 8)
			       :direction :output
			       :if-exists :append
			       :if-does-not-exist :create)
	    (trivial-utf-8:write-utf-8-bytes string out))
	  (write-string string (if (eql *log-output* t)
				   *standard-output*
				   *log-output*))))))

(defun .format-thread-name ()
  "writes the name of the thread like \"[thread-name] \" for FORMAT-LOG"
  (format nil "[~A] " (thread-name (current-thread))))

 (defun .format-log-date (ut)
  "writes the date like \"[DD:MM:YYY] \" for FORMAT-LOG"
   (multiple-value-bind (sec min hour day month year) (decode-universal-time ut)
     (declare (ignore sec min hour))
     (format nil "[~2,'0d:~2,'0d:~2,'0d] " day month year)))

 (defun .format-log-time (time)
   "writes the time like \"[HH:MM:SS] \" for FORMAT-LOG"
   (multiple-value-bind (sec min hour) (decode-universal-time time)
     (format nil "[~2,'0d:~2,'0d:~2,'0d] " hour min sec)))

(defun format-log (fmt-string &rest fmt-args)
  "like FORMAT, except it prepends the result string of combining `FMT-STRING' and
`FMT-ARGS' with some informative garbage and writes it to the log

running the following code:

    (format-log \"foo bar ~A\" 'baz)

might result in the following line being sent to WRITE-STRING-TO-LOG:

   \";[repl-thread] [01:06:2008] [13:53:27] foo bar BAZ\"

see also: *LOG-PREFIX-STRING*, *LOG-THREAD-NAME-P*, *LOG-DATE-P*, and *LOG-TIME-P*"
  (when *log-output*
    (let ((ut (get-universal-time)))
      (write-string-to-log (concatenate 'string
					*log-prefix-string*
					(if *log-thread-name-p*
					    (.format-thread-name)
					    "")
					(if *log-date-p*
					    (.format-log-date ut)
					    "")
					(if *log-time-p*
					    (.format-log-time ut)
					    "")
					(apply #'format nil fmt-string fmt-args))))))

; logging arbitrary forms and their return values

(defmacro logv (form &rest more-forms)
  "behaves like PROGN except it logs each form and its return value using FORMAT-LOG
[and of course its forms are not reckognized by the compiler as being top-level
forms since it's just a macro, not a special operator like PROGN]

the following code:

    (logv (+ 1 (logv (+ 1 1)))
          (+ 10 10)
          (* 64 64))

will return 4096 and probably write something like the following lines to the log:

   ;[repl-thread] [01:06:2008] [14:07:53] (+ 1 1) -> 2
   ;[repl-thread] [01:06:2008] [14:07:53] (+ 1 (LOGV (+ 1 1))) -> 3
   ;[repl-thread] [01:06:2008] [14:07:53] (+ 10 10) -> 20
   ;[repl-thread] [01:06:2008] [14:07:53] (* 64 64) -> 4096

see also: FORMAT-LOG, LOGVS"
  (if more-forms
      `(progn (logv ,form) (logv ,@more-forms))
      (let ((% (gensym "value")))
	`(let ((,% ,form))
	   (format-log "~S -> ~S" ',form ,%)
	   ,%))))

(defmacro logvs (form &rest more-forms)
  "lke LOGV but works with multiple return values"
  (if more-forms
      `(progn (logvs ,form) (logvs ,@more-forms))
      (let ((%l (gensym "values")))
	`(let ((,%l (multiple-value-list ,form)))
	   (format-log "~S -> values list: ~S" ',form ,%l)
	   (apply 'values ,%l)))))

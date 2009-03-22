(defpackage :logv
  (:use :cl)
  (:export :*log-output*
	   :*log-prefix-string*
	   :*log-thread-name-p*
	   :*log-date-p*
	   :*log-time-p*
           :write-string-to-log
           :format-log
	   :logv
	   :logvs))


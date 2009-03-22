
(defpackage :cl-terrace
  (:use :cl :f-underscore :logv)
  (:export ; defining-projects
           :def-terrace

	   ; functions for use within terrace files
	   :render
	   :static
	   :funcall-file
	   :v
	   :d
	   :g
	   :p

	   ; block you can RETURN-FROM to escape from terrace files
	   :terrace-file

	   ; syncing projects
	   :sync-terrace

	   ; script for easily publishing projects
           :*terrace-server*
	   :!start-terrace-server
	   :!stop-terrace-server

	   ; script for starting "View Only" server
	   :!start-terrace-server/view-only

	   ; probably useful for debugging
	   :*terrace-project*
	   :*terrace-path*
	   :*v-plist*
	   :get-terrace-file
	   :special-path-alist->plist
	   :funcall-path))

(defpackage :cl-terrace-user
  (:use :cl :cl-terrace :hunchentoot :djula :logv))
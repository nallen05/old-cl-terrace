
(defpackage :logv-system
  (:use :cl))

(in-package :logv-system)

(asdf:defsystem :logv
  :depends-on (:trivial-utf-8
               #-:lispworks :bordeaux-threads)
  :description "simple logging for cl-terrace"
  :components
  ((:file "package")
   #-:lispworks (:file "bordeaux-wrappers" :depends-on ("package"))
   #+:lispworks (:file "lispworks" :depends-on ("package"))
   (:file "logv" :depends-on (#-lispworks "bordeaux-wrappers"
			      #+lispworks "lispworks"))))

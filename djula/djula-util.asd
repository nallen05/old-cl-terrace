
(defpackage #:djula-util-system
  (:use :cl :asdf))

(in-package #:djula-util-system)

(defsystem :djula-util
  :depends-on (:trivial-utf-8 :cl-fad :cl-ppcre :f-underscore)
  :description
"code shared between `djula' and `cl-terrace' for interoperability or convenience"
  :author "Nick Allen <nallen05@gmail.com"
  :components
  ((:module :djula-util
            :components ((:file "djula-util")))))

(defpackage :{{name}}
  (:use :cl :cl-terrace :hunchentoot :djula :logv)
  (:export ;; {{name}} is the name of the project. use it like:
           ;;
           ;;    (cl-terrace:!start-terrace-server #:{{name}})
           ;;
           #:{{name}}))
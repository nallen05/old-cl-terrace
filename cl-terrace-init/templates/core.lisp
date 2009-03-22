
(in-package :{{name}})

(defparameter *system-folder*
  (asdf:component-pathname (asdf:find-system :{{name}})))

(defparameter *site-folder* (merge-pathnames "site/" *system-folder*))

(defvar {{name}})
(def-terrace '{{name}} *site-folder*)

;; now run:
;;
;;    (require :{{name}})
;;    (in-package :{{name}})
;;    (!start-terrace-server {{name}} :port 8282)
;;
;; then go to:
;;
;;    http://localhost:8282/
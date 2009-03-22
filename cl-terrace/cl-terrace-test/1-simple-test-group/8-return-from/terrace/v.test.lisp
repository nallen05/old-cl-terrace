
(in-package :cl-terrace-user)

(if (string-equal (v "test") "foo")
    (return-from cl-terrace:terrace-file "the url is /foo"))

"the url is not /foo"
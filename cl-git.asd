;;;; cl-git.asd
(defpackage #:cl-git-config (:export #:*base-directory*))
(defparameter cl-git-config:*base-directory* 
  (make-pathname :name nil :type nil :defaults *load-truename*))

(asdf:defsystem #:cl-git
  :serial t
  :description "Pure Common Lisp Git Implementation."
  :author "Jose Santos <jsmpereira@gmail.com>"
  :license "MIT"
  :depends-on (#:zlib
               #:ironclad
	       #:cl-ppcre
	       #:fiveam)
  :components ((:file "package")
               (:file "cl-git")))

(asdf:defsystem #:cl-git-test
  :serial t
  :description "Tests for cl-git."
  :author "Jose Santos <jsmpereira@gmail.com>"
  :license "MIT"
  :depends-on (#:cl-git)
  :components ((:file "tests")))

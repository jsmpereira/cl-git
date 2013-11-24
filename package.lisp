;;;; package.lisp

(defpackage #:cl-git
  (:use #:cl)
  (:export #:*repository*
	   #:git-hash-object
	   #:git-cat-file))


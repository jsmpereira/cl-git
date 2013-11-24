;;; -*- Mode: Lisp -*-
;;;;
;;;; tests.lisp

(defpackage #:cl-git-test
  (:use #:cl
	#:cl-git
	#:fiveam))

(in-package #:cl-git-test)

(def-suite object-suite :description "Test Git objects behaviour.")

(in-suite object-suite)

;;; Project after initial commit
;;;
;;; cl-git$  find .git/objects -type f
;;; .git/objects/7f/c261b7816fd6f4efd458e8ece5ee11ab5f7a18 <- tree
;;; .git/objects/c7/61a3517437e5f141b12ac8d622ae6511f67391 <- blob
;;; .git/objects/d4/f2c463feb66b7b8ccb18ba7971e793087c27c5 <- commit

(defparameter *content* "Pure Common Lisp Git (naive) Implementation")
(defparameter *decompressed-blob*
  (format nil "blob ~A~A~A" (length *content*) (code-char 0) *content*))
(defparameter *sha1* "c761a3517437e5f141b12ac8d622ae6511f67391")

(test sha1->path
  (is (string= (cl-git::sha1->path *sha1*)
	       ".git/objects/c7/61a3517437e5f141b12ac8d622ae6511f67391")))

(test header+content
  (is (string= (cl-git::header+content :blob *content*)
	       *decompressed-blob*)))

(test git-hash-object
  (let ((obj (second (multiple-value-list (cl-git:git-hash-object *content*)))))
    (is (string= (cl-git::inflate (cl-git::content obj)) *decompressed-blob*))))

(test git-cat-file-handle-t
  (is (string= (git-cat-file :t *sha1*)
	       "blob")))

(test git-cat-file-handle-p
  (is (string= (git-cat-file :p *sha1*)
	       (format nil "Pure Common Lisp Git (naive) Implementation~%"))))

;;;; cl-git.lisp

(in-package #:cl-git)

(defparameter *repository* cl-git-config:*base-directory*
  "Path to git repository. Defaults to cl-git project directory.")

(defclass blob ()
  ((content :initarg :content :accessor content)
   (sha1 :accessor sha1)
   (path :accessor path)))

(defmethod initialize-instance :after ((self blob) &key)
  (with-slots (content sha1 path) self
    (let ((header+content (header+content :blob content)))
      (setf sha1 (sha1-digest header+content)
	    content (deflate header+content)
	    path (merge-pathnames (sha1->path sha1) *repository*)))))

(defun deflate (content)
  (zlib:compress (ironclad:ascii-string-to-byte-array content) :fixed))

(defun inflate (content)
  (with-output-to-string (s)
    (loop for el across (zlib:uncompress content)
	  do (write-char (code-char el) s))))

(defun sha1-digest (content)
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence :sha1 (ironclad:ascii-string-to-byte-array content))))

(defun sha1->path (sha1)
  (concatenate 'string ".git/objects/" (subseq sha1 0 2) "/" (subseq sha1 2)))

(defun header+content (obj-type content)
  (format nil "~A ~A~A~A" (string-downcase obj-type) (length content) (code-char 0) content))

(defun git-hash-object (content)
  "Create git object. Returns the sha1 and the object.
TODO: implement -w option for writing object to disk."
  (let ((obj (make-instance 'blob :content content)))
    (values (sha1 obj) obj)))

(defun git-cat-file (handle sha1)
  "Inspect Git objects.
Pass HANDLE as :t for object type or :p for content pretty printing."
  (let ((path (merge-pathnames (sha1->path sha1) *repository*)))
    (with-open-file (f path :element-type '(unsigned-byte 8))
      (let ((buffer (make-array (file-length f) :element-type '(unsigned-byte 8) :fill-pointer t)))
	(read-sequence buffer f)
	(let ((content (inflate buffer)))
	  (ecase handle
	    (:t (subseq content 0 (cl-ppcre:scan #\Space content)))
	    (:p (subseq content (1+ (cl-ppcre:scan (code-char 0) content))))))))))

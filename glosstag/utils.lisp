
(in-package :glosstag)

(defparameter *db* (make-hash-table :test #'equal))

(defun index (&key (*db* *db*))
  (dolist (file (directory "/Users/ar/work/glosstag/*.plist"))
    (with-open-file (in file)
      (loop for plist = (read in nil nil)
	    while plist
	    do (let ((pairs (getf (cdr plist) :KEYS)))
		 (dolist (p pairs)
		   (push (car p) (gethash (cdr p) *db*))))))))

(defun get-lemma (lemma &key (*db* *db*))
  (gethash lemma *db*))



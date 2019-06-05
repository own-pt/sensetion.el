
(in-package :glosstag)

(defparameter *db* (make-hash-table :test #'equal))
(defparameter *db-freq* (make-hash-table :test #'equal))

(defun index (&key (*db* *db*))
  (dolist (file (directory "/Users/ar/Temp/glosstag/*.plist"))
    (with-open-file (in file)
      (loop for plist = (read in nil nil)
	    while plist
	    do (let ((pairs (getf plist :keys)))
		 (dolist (p pairs)
		   (push (car p) (gethash (cdr p) *db*))))))))


(defun index-freq (&key (*db-freq* *db-freq*))
  (dolist (file (directory "/Users/ar/Temp/glosstag/*.plist"))
    (with-open-file (in file)
      (loop for plist = (read in nil nil)
	    while plist
	    do (dolist (tk (getf plist :tokens))
		 (if (getf tk :lemma)
		     (let ((lms (cl-ppcre:split "\\|" (getf tk :lemma))))
		       (mapcar (lambda (l)
				 (push (getf plist :id)
				       (gethash l *db-freq*)))
			       lms))))))))
  

(defun get-lemma (lemma &key (db *db*))
  (gethash lemma db))



(defun stat-gloss (db gloss)
  (let ((tks (getf gloss :TOKENS)))
    ))


(defun stat-corpus (corpus-directory)
  (let ((db (make-hash-table :test '#equal)))
    (mapcar (lambda (file) 
	      (with-open-file (in file) 
	       (loop for line = (read in)
		     while line
		     do (stat-gloss db line))))
	    (directory corpus-directory))))



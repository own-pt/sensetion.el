
(in-package :glosstag)

(defstruct line id type form lemma pos tag senses globid globtag sep)

(defun token-to-line (tk)
  (labels ((getc (key)
	     (if (getf tk key) (getf tk key) "_")))
    (let ((senses (if (getf tk :senses)
		      (format nil "~{~a~^|~}" (mapcar #'car (getf tk :senses)))
		      "_")))
     (cond
       ((equal :wf (getf tk :kind))
	(make-line :id id :type (getc :kind)
		   :form (getc :form) :lemma (getc :lemma) :pos (getc :pos) :tag (getc :tag) :senses senses))

       ((equal :cf (car (getf tk :kind)))
	(make-line :id id :type (car (getc :kind))
		   :form (getc :form) :lemma (getc :lemma) :pos (getc :pos) :tag (getc :tag) :senses senses
		   :sep (getc :sep) :globid (cdr (getf tk :kind))))

       ((equal :glob (car (getf tk :kind)))
	(make-line :id id :type (car (getc :kind))
		   :form (getc :form) :lemma (getc :lemma) :tag (getc :tag) :senses senses
		   :globid (cdr (getc :kind)) :globtag (getc :glob)))
       
       ((member (getf tk :kind) (list :def :qf :ex)) nil)
       (t (error "invalid object ~a" tk))))))


(defun process-token (id tk stream)
  (let ((sep #\Tab)
	(line (token-to-line tk)))
    (format stream "~a~C~a~C~a~C~a~C~a~C~a~C~a~%"
	    (reduce (lambda (a b) (append a (list (line-?? ) sep)))
		    '(id type form lemma pos tag senses globid globtag sep) :initial-value nil))))


(defun process-entry (data stream)
  (format stream "~%# txt = ~a~%# id = ~a-~a~%" (getf data :gloss) (getf data :ofs) (getf data :pos))
  (loop for token in (getf data :tokens)
	for id = 1 then (incf id)
	do (process-token id token stream)))

(defun process-corpus (corpus-directory output)
  (with-open-file (out output :direction :output :if-exists :supersede)
    (mapc (lambda (file) 
	    (with-open-file (in file) 
	      (loop for line = (read in nil nil)
		    while line
		    do (process-entry line out))))
	  (directory corpus-directory))))



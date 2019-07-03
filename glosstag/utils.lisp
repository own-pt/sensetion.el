
(in-package :glosstag)

(defclass line ()
  ((id     :initarg :id     :initform "_" :accessor line-id)
   (kind   :initarg :kind   :initform "_" :accessor line-kind)
   (type   :initarg :type   :initform "_" :accessor line-type)
   (form   :initarg :form   :initform "_" :accessor line-form)
   (lemma  :initarg :lemma  :initform "_" :accessor line-lemma)
   (pos    :initarg :pos    :initform "_" :accessor line-pos)
   (sense  :initarg :sense  :initform "_" :accessor line-sense)
   (tag    :initarg :tag    :initform "_" :accessor line-tag)
   (glob-i :initarg :glob-i :initform "_" :accessor line-glob-i)
   (glob-t :initarg :glob-t :initform "_" :accessor line-glob-t)
   (sep    :initarg :sep    :initform "_" :accessor line-sep)))


(defun token-to-line (id tk)
  (labels ((getc (key)
	     (if (getf tk key) (getf tk key) "_")))
    (let ((senses (if (getf tk :senses)
		      (format nil "~{~a~^|~}" (mapcar #'car (getf tk :senses)))
		      "_")))
      (cond
	((member (getf tk :kind) (list :classif :def :mwf :qf :aux :ex))
	 (make-instance 'line :id id :kind (getc :kind) :type (getc :type)
			      :form (getc :action) :lemma (getc :rend) :tag (getc :tag))
	 (let ((db (remove-from-plist tk :kind :type :action :rend :tag)))
	   (assert (null db))))
	
	((equal :wf (getf tk :kind))
	 (make-instance 'line :id id :kind (getc :kind) :type (getc :type)
			      :form (getc :form) :lemma (getc :lemma) :pos (getc :pos)
			      :tag (getc :tag) :sense senses)
	 (let ((db (remove-from-plist tk :kind :type :form :lemma :pos :tag :senses)))
	   (assert (null db))))

	((equal :cf   (car (getf tk :kind)))
	 (make-instance 'line :id id :kind (car (getc :kind)) :type (getc :type)
			      :form (getc :form) :lemma (getc :lemma) :pos (getc :pos) :tag (getc :tag)
			      :sense senses
			      :sep (getc :sep) :glob-i (cdr (getf tk :kind))))

	((equal :glob (car (getf tk :kind)))
	 (make-instance 'line :id id :kind (car (getc :kind)) :type (getc :type)
			      :form (getc :form) :lemma (getc :lemma) :tag (getc :tag)
			      :sense senses
			      :glob-i (cdr (getc :kind)) :glob-t (getc :glob)))
       
	(t (error "invalid object ~a" tk))))))


(defun process-token (id tk stream)
  (let ((line (token-to-line id tk))
	(sep #\Tab))
    (if line
	(format stream "~{~a~^~a~}~%"
		(reverse (cdr (reverse (loop for x in '(id kind type form lemma pos tag sense glob-i glob-t sep)
					     append (list (slot-value line x) sep)))))))))


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


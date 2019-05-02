(ql:quickload :yason)
(ql:quickload :alexandria)

(defun main (in-fp synset-fp)
  (let ((files (directory (make-pathname :defaults in-fp :name :wild :type "plist"))))
    (with-open-file (synset-out synset-fp :direction :output :if-does-not-exist :create
					  :if-exists :supersede)
      (loop
	for fp in files
	do (file->json fp synset-out)))))

(defun file->json (fp synset-out)
  (with-open-file (in fp)
    (with-open-file (out (make-pathname :defaults in :type "json")
			 :direction :output :if-does-not-exist :create :if-exists :supersede)
      (loop
	for line = (read-line in nil 'eof)
	until (eq line 'eof)
	do (let ((plist (read-from-string line)))
	     (synset->json plist synset-out)
	     (sentence->json plist out))))))

(defun synset->json (plist out)
  (index-json out)
  (let ((ofs (read-from-string (getf plist :ofs)))
	(pos (getf plist :pos))
	(keys-lemmas (getf plist :keys))
	(gloss (getf plist :gloss)))
    (yason:encode
     (alexandria:alist-hash-table
      `(("ofs" . ,ofs)
	("pos" . ,pos)
	("keys" . ,(mapcar #'car keys-lemmas))
	("terms" . ,(mapcar #'cdr keys-lemmas))
	("gloss" . ,gloss))
      :test 'equal)
     out)
    (terpri out)))

(defun index-json (out)
  (yason:encode
   (alexandria:alist-hash-table
    `(("index" . ,(make-hash-table :test 'equal))) :test 'equal)
   out)
  (terpri out))

(defun sentence->json (plist out)
  (index-json out)
  (let ((ofs (read-from-string (getf plist :ofs)))
	(pos (getf plist :pos))
	(tokens (getf plist :tokens))
	(gloss (getf plist :gloss)))
    (yason:encode
     (alexandria:alist-hash-table
      `(("meta" . ,(alexandria:alist-hash-table
		    `(("ofs" . ,ofs)
		      ("pos" . ,pos)) :test 'equal))
	("token" . ,(mapcar #'token->json tokens))
	("raw_text" . ,gloss))
      :test 'equal)
     out)
    (terpri out)))

(defun symbol->string (sym)
  (string-downcase (string sym)))

(defun alist-remove-empty (alist)
  (mapcan (lambda (elem) (if (null (cdr elem)) nil (list elem))) alist))

(defun token->json (plist)
  (let ((h (alexandria:plist-hash-table plist))
	(kind (getf plist :kind)))
    (alexandria:alist-hash-table
     (alist-remove-empty
      `(("form" . ,(gethash :form h))
	("lemmas" . ,(uiop:split-string (gethash :lemma h) :separator '(#\|)))
	("kind" . ,(if (consp kind)
		       (format nil "~{~A~^:~}" (cons (symbol->string (car kind))
						     (if (listp (cdr kind)) (cdr kind) (list (cdr kind)))))
		       (symbol->string kind)))
	("senses" . ,(mapcar #'car (gethash :senses h)))
	("unsure" . ,(gethash :unsure h))
	("tag" . ,(symbol->string (gethash :tag h "ignore")))
	("glob" . ,(gethash :glob h))
	("meta" . ,(alexandria:alist-hash-table
		    (loop
		      for key being the hash-keys of h
		      for val being the hash-values of h
		      unless (member key '(:form :lemma :glob :kind :senses :unsure :tag))
			collect (cons (symbol->string key)
				      (if (symbolp val)
					  (symbol->string val)
					  val)))))))
     :test 'equal)))

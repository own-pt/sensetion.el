(ql:quickload '(:cl-json :split-sequence :alexandria))

(defparameter *lexnames*
  (alexandria:alist-hash-table
   '((0 . "adj.all")
     (1 . "adj.pert")
     (2 . "adv.all")
     (3 . "noun.Tops")
     (4 . "noun.act")
     (5 . "noun.animal")
     (6 . "noun.artifact")
     (7 . "noun.attribute")
     (8 . "noun.body")
     (9 . "noun.cognition")
     (10 . "noun.communication")
     (11 . "noun.event")
     (12 . "noun.feeling")
     (13 . "noun.food")
     (14 . "noun.group")
     (15 . "noun.location")
     (16 . "noun.motive")
     (17 . "noun.object")
     (18 . "noun.person")
     (19 . "noun.phenomenon")
     (20 . "noun.plant")
     (21 . "noun.possession")
     (22 . "noun.process")
     (23 . "noun.quantity")
     (24 . "noun.relation")
     (25 . "noun.shape")
     (26 . "noun.state")
     (27 . "noun.substance")
     (28 . "noun.time")
     (29 . "verb.body")
     (30 . "verb.change")
     (31 . "verb.cognition")
     (32 . "verb.communication")
     (33 . "verb.competition")
     (34 . "verb.consumption")
     (35 . "verb.contact")
     (36 . "verb.creation")
     (37 . "verb.emotion")
     (38 . "verb.motion")
     (39 . "verb.perception")
     (40 . "verb.possession")
     (41 . "verb.social")
     (42 . "verb.stative")
     (43 . "verb.weather")
     (44 . "adj.ppl")
     (45 . "adjs.all"))
   :size 46 :test #'eql))


(defun plist-tk->json-tk (plist-tk)
  (let ((hash (make-hash-table :test #'equal)))
    (alexandria:doplist (key val plist-tk hash)
	(case key
	  (:kind
	   (cond
	     ((symbolp val)
	      (setf (gethash key hash) (list val)))
	     ((not (listp (cdr val)))
	      (setf (gethash key hash) (list (car val) (cdr val))))
	     (t
	      (setf (gethash key hash) val))))
	  ((:tag :form :glob)
	   (setf (gethash key hash)
		 val))
	  (:senses
	   (setf (gethash key hash)
		 (mapcar #'car val)))
	  (:lemma
	   (setf (gethash :lemmas hash)
		 (split-sequence:split-sequence #\| val)))
	  (otherwise
	   (push  (cons key val)
		  (gethash :meta hash)))))))


(defun plist->json (plist stream)
  (let* ((ofs (parse-integer (getf plist :ofs)))
	 (hash (alexandria:alist-hash-table `((:sent_id . ,ofs))
					    :test #'equal)))
    
    (alexandria:doplist (key val plist)
			(case key
			  (:gloss   (setf (gethash :text hash)
					  val))
			  (:tokens  (setf (gethash :tokens hash)
					  (mapcar #'plist-tk->json-tk val)))
			  (:keys    (push (cons "keys"
						(mapcar #'(lambda (pair) (list (car pair) (cdr pair)))
							val))
					  (gethash :meta hash)))
			  (otherwise (push (cons key val)
					   (gethash :meta hash)))))

    (let ((sk (car (first (getf plist :keys)))))
      (destructuring-bind (* lexnumstr) (split-sequence:split-sequence #\: sk :count 2)
	(let* ((lexnum (parse-integer lexnumstr))
	       (lexfile (gethash lexnum *lexnames*)))
	  (setf (gethash :_id hash) (format nil "~a-~a" lexfile ofs))
	  (setf (gethash :doc_id hash) lexfile))))
    
    (cl-json:encode-json hash stream)
    (format stream "~%")
    hash))


(defun main (plists out-file)
  (with-open-file (out out-file :direction :output :if-exists :supersede)
    (loop for file in plists
	  do (with-open-file (in file)
	       (loop for plist = (read in nil nil)
		     while plist do
		       (plist->json plist out))))))

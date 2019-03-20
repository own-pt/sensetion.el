;; (C) 2019 IBM Corporation
;;  Author: Alexandre Rademaker

(in-package :glosstag)

(defvar *state* nil)

(defun state-p (id)
  (member id *state*))

(defun state-on (&rest ids)
  (setf *state* (union ids *state*)))

(defun state-off (&rest ids)
  (setf *state* (if (null ids) nil
		    (set-difference *state* ids))))


(defclass synset ()
  ((id         :initform nil :initarg :id  :accessor ss-id)
   (ofs        :initform nil :initarg :ofs :accessor ss-ofs)
   (pos        :initform nil :initarg :pos :accessor ss-pos)
   (terms      :initform nil :accessor ss-terms)
   (keys       :initform nil :accessor ss-keys)
   (gloss-orig :initform nil :accessor ss-gloss-orig)
   (gloss-tok  :initform nil :accessor ss-gloss-tok)
   (tokens     :initform nil :accessor ss-tokens)))


(defclass token ()
  ((id         :initform nil :initarg :id    :accessor tk-id)
   (attrs      :initform nil :initarg :ofs   :accessor tk-attrs)
   (sform      :initform nil :initarg :sform :accessor tk-sform)))

(defclass wordnet-handler (sax:default-handler)
  ((synsets :initform nil :accessor wh-synsets) 
   (css     :initform nil :accessor wh-css)
   (ctk     :initform nil :accessor wh-ctk)))



(defun synset->plist (ss)
  (list 'synset
	:id (ss-id ss) :ofs (ss-ofs ss) :pos (ss-pos ss)
	:keys (mapcar (lambda (a b) (cons a b))  (ss-keys ss) (ss-terms ss))
	:gloss (ss-gloss-orig ss)
	:tokens (remove-if (lambda (tk) (member :remove tk))
			   (mapcar #'token->plist (ss-tokens ss)))))

(defun token->plist (tk)
  (assert (or (null (tk-sform tk)) (= 1 (length (tk-sform tk)))))
  (cond
    ((member (car (car (tk-attrs tk))) (list "def" "ex") :test #'equal)
     (assert (equal 1 (length (tk-attrs tk))))
     (let ((att (car (tk-attrs tk))))
       (list 'tk :kind (car att) :action (cadr att))))

    ((equal "qf" (car (car (tk-attrs tk))))
     (assert (equal 1 (length (tk-attrs tk))))
     (let ((att (car (tk-attrs tk))))
       (if (equal "open" (cadr (car (tk-attrs tk))))
	   (list 'tk :kind (car att) :action (cadr att) :rend (nth 3 att))
	   (list 'tk :kind (car att) :action (cadr att)))))

    ((member '("wf" "type" "punc") (tk-attrs tk) :test #'equal)
     (if (member '("wf" "pos" ":") (tk-attrs tk) :test #'equal)
	 ;; all cases where punc and @pos are the fake `;`, marking to remove
	 (list 'tk :kind "wf" :pos "punc" :sform (car (tk-sform tk)) :remove t) 
	 (list 'tk :kind "wf" :pos "punc" :sform (car (tk-sform tk)))))
    (t (if (tk-sform tk)
	   (list 'tk :attrs (tk-attrs tk) :sform (car (tk-sform tk)))
	   (list 'tk :attrs (tk-attrs tk))))))


(defmethod sax:start-element ((wh wordnet-handler) (namespace t) (local-name t) (qname t) 
			      (attributes t))
  (with-slots (css ctk) wh
    (cond 
     ((equal local-name "synset")
      (setf css (make-instance 'synset
			       :id  (sax:attribute-value (sax:find-attribute "id"  attributes))
			       :pos (sax:attribute-value (sax:find-attribute "pos" attributes))
			       :ofs (sax:attribute-value (sax:find-attribute "ofs" attributes)))))
     ((equal local-name "term")
      (state-on :reading-term))
     ((equal local-name "sk")
      (state-on :reading-key))
     
     ((equal local-name "gloss")
      (let* ((av (sax:attribute-value (sax:find-attribute "desc" attributes))))
	(switch (av :test #'equal)
		("orig"  (state-on :reading-gloss-orig))
		("text"  (state-on :reading-gloss-tok)))))
     
     ((member local-name '("cf" "wf") :test #'equal)
      (state-on :reading-token)
      (let ((tk (make-instance 'token)))
	(mapcar (lambda (at)
		  (push (list local-name (sax:attribute-local-name at) (sax:attribute-value at))
			(slot-value tk 'attrs)))
		attributes)
	(setf ctk tk)))

     ((member local-name '("mwf" "qf" "aux" "classif" "ex" "def") :test #'equal)
      (let ((tk (make-instance 'token)))
	(mapcar (lambda (at)
		  (push (list local-name "open" (sax:attribute-local-name at) (sax:attribute-value at))
			(slot-value tk 'attrs)))
		attributes)
	(push tk (ss-tokens css))))

     ((member local-name (list "id" "glob") :test #'equal)
      (mapcar (lambda (at)
		(push (list local-name (sax:attribute-local-name at) (sax:attribute-value at))
		      (slot-value ctk 'attrs)))
	      attributes)))))


(defmethod sax:end-element ((wh wordnet-handler) (namespace t) (local-name t) (qname t))
  (with-slots (synsets css ctk) wh
    (cond 
      ((equal local-name "synset")
       (setf (slot-value css 'tokens)
	     (reverse (slot-value css 'tokens)))
       (dolist (slot (list 'gloss-orig 'gloss-tok))
	 (setf (slot-value css slot)
	       (format nil "~{~a~^ ~}" (reverse (slot-value css slot)))))
       (push css synsets)
       (setf css nil))
      ((equal local-name "term")
       (state-off :reading-term))
      ((equal local-name "sk")
       (state-off :reading-key))
      ((equal local-name "gloss")
       (state-off :reading-gloss-tok)
       (state-off :reading-gloss-orig))

      ((member local-name '("mwf" "qf" "aux" "classif" "def" "ex") :test #'equal)
       (let ((tk (make-instance 'token)))
	 (push (list local-name "close")
	       (slot-value tk 'attrs))
	 (push tk (ss-tokens css))))

      ((member local-name '("cf" "wf") :test #'equal)
       (state-off :reading-token)
       (push ctk (ss-tokens css))
       (setf ctk nil)))))


(defmethod sax:characters ((wh wordnet-handler) (data t))
  (with-slots (css ctk) wh
    (cond 
     ((state-p :reading-term)
      (push data (slot-value css 'terms)))
     ((state-p :reading-key)
      (push data (slot-value css 'keys)))
     ((state-p :reading-gloss-orig)
      (if (> (length (string-trim '(#\Space #\Tab #\Newline) data)) 0)
	  (push data (slot-value css 'gloss-orig))))
     ((state-p :reading-gloss-tok)
      (if (> (length (string-trim '(#\Space #\Tab #\Newline) data)) 0)
	  (push data (slot-value css 'gloss-tok))))
     ((state-p :reading-token)
      (if (> (length (string-trim '(#\Space #\Tab #\Newline) data)) 0)
	  (push data (slot-value ctk 'sform)))))))


(defun load-xml (filename) 
  (let ((wh (make-instance 'wordnet-handler))
	(*state* nil))
    (cxml:parse filename wh)
    (slot-value wh 'synsets)))


(defun xml->plist (filein fileout)
  (with-open-file (out fileout :direction :output :if-exists :supersede) 
    (format out "~{~s~%~}" (mapcar #'synset->plist (load-xml (make-pathname :defaults filein))))))



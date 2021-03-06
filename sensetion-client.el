;;; sensetion.el --- -*- lexical-binding: t; -*-

(eval-when-compile 'cl-lib)
(require 'cl-lib)
(require 'json)
(require 'request)
(require 'f)
(require 'map)
(require 'seq)

(require 'sensetion-data)
(require 'sensetion-utils)

;;; define generic methods
(cl-defgeneric sensetion--backend-prepare (backend)
  "Check that backend is available and prepare it.")

(cl-defgeneric sensetion--backend-prefix-lemma (backend prefix)
  "Return a list of lemmas with PREFIX as prefix.")

(cl-defgeneric sensetion--backend-prefix-document-id (backend prefix)
  "Return a list of doc-id with PREFIX as prefix.")

(cl-defgeneric sensetion--backend-get-sorted-doc-sents (backend doc-id)
  "Given the DOC-ID return a list of sensetion--sent objects sorted by sent-id.")

(cl-defgeneric sensetion--backend-get-sorted-sents (backend lemma &optional pos)
  "Return a list of sensetion--sent objects thats cotains tokens with LEMMA and/or POS.")

(cl-defgeneric sensetion--backend-id->sent (backend sent_id)
  "Return the sent given a SENT_ID.")

(cl-defgeneric sensetion--backend-update-modified-sent (backend sent)
  "Update the backend info SENT content.")

(cl-defgeneric sensetion--backend-lemma->sorted-synsets (backend lemma pos)
  "Return a list of sensetion--synset objects given a LEMMA and a POS.")


(defun sensetion--client-prepare ()
  "See `sensetion--backend-prepare'"
  (sensetion--backend-prepare (sensetion--project-backend sensetion-current-project)))

(defun sensetion-client-prefix-lemma (prefix)
  "See `sensetion--backend-prefix-lemma'"
  (sensetion--backend-prefix-lemma (sensetion--project-backend sensetion-current-project) prefix))

(defun sensetion-client-prefix-document-id (prefix)
  "See `sensetion--backend-prefix-document-id'"
  (sensetion--backend-prefix-document-id (sensetion--project-backend sensetion-current-project) prefix))

(defun sensetion--client-get-sorted-doc-sents (doc-id)
  "See `sensetion--backend-get-sorted-doc-sents'"
  (sensetion--backend-get-sorted-doc-sents (sensetion--project-backend sensetion-current-project) doc-id))

(defun sensetion--client-get-sorted-sents (lemma &optional pos)
  "See `sensetion--backend-get-sorted-sents'"
  (sensetion--backend-get-sorted-sents (sensetion--project-backend sensetion-current-project) lemma pos))

(defun sensetion--client-id->sent (sent_id)
  "See `sensetion--backend-id->sent'"
  (sensetion--backend-id->sent (sensetion--project-backend sensetion-current-project) sent_id))

(defun sensetion--client-update-modified-sent (sent)
  "See `sensetion--backend-update-modified-sent'"
  (let ((sent (sensetion--remove-man-now sent)))
    (sensetion--backend-update-modified-sent (sensetion--project-backend sensetion-current-project) sent)))

(defun sensetion--client-lemma->sorted-synsets (lemma pos)
  "See `sensetion--backend-lemma->sorted-synsets'"
  (sensetion--backend-lemma->sorted-synsets (sensetion--project-backend sensetion-current-project) lemma pos))


(defun sensetion--remove-man-now (sent)
  "Substitute annotation tags of 'man-now' by 'man'.

We use a virutal annotation tag of 'man-now' to be able to
differentiate between tokens that were annotated in the current
session and tokens that had already been annotated. We don't save
tokens with 'man-now' tags, only 'man' tags."
  (cl-labels
      ((remove-man-now (tk)
		       (pcase tk
			 ((cl-struct sensetion--tk (tag "man-now"))
			  (setf (sensetion--tk-tag tk) "man")))
		       tk))
    (setf (sensetion--sent-tokens sent) (mapcar #'remove-man-now (sensetion--sent-tokens sent)))
    sent))


;;; elasticsearch backend deprecated


;;; mongodb backend

(cl-defstruct (sensetion--mongo (:constructor nil)
		       (:constructor sensetion--make-mongo))
  db synset-collection document-collection)


(cl-defun sensetion-make-mongo (&key (db "sensetion-database")
			    (synset-collection "synsets")
			    (document-collection "documents"))
  (cl-labels
      ((check-name (name)
		   (let ((forbidden-chars "';.[]()"))
		     (when (seq-intersection forbidden-chars name)
		       (user-error "Mongo name must not contain any of %s"
				   forbidden-chars)))))
    (mapc #'check-name (list db synset-collection document-collection))
    (sensetion--make-mongo :db db :synset-collection synset-collection
		  :document-collection document-collection)))


(defun sensetion--mongo-cmd (args)
  "In DB.COLLECTION run CMD.
ARGS if present will be used to format CMD."
  (with-temp-buffer
    (let ((exit-code (apply #'call-process "mongo" nil (current-buffer) nil args)))
      (pcase exit-code
	(0
	 (goto-char (point-min))
	 (let ((json-array-type 'list))
	   (sensetion--parse-jsonlines)))
	(_
	 (let ((log-buffer (get-buffer-create sensetion-log-buffer-name)))
	   (append-to-buffer log-buffer (point-min) (point-max))
	   (pop-to-buffer log-buffer nil t))
	 (error "Error: see %s" sensetion-log-buffer-name))))))


(defun sensetion--mongo-cmd-file (cmd-str)
  "Create temporary file with contents CMD-STR, and return its filename."
  (let ((inhibit-message t))
    (make-temp-file "sensetion" nil nil cmd-str)))


(cl-defmethod sensetion--backend-prepare ((backend sensetion--mongo))
  (let* ((db (sensetion--mongo-db backend))
	 (synset-coll (sensetion--mongo-synset-collection backend))
	 (document-coll (sensetion--mongo-document-collection backend))
	 (args (list db "--quiet" "--norc"
		     (sensetion--mongo-cmd-file
		      (format "printjsononeline(db.serverCmdLineOpts());
printjsononeline(db['%s'].createIndex({\"terms\": 1, \"pos\": 1, \"_id\": -1}));
printjsononeline(db['%s'].createIndex({\"doc_id\": 1, \"sent_id\": -1}));
printjsononeline(db['%s'].createIndex({\"tokens.lemmas\": 1, \"_id\": -1}));"
			      synset-coll document-coll document-coll)))))
    (message "Creating MongoDB indices")
    (sensetion--mongo-cmd args)))


(cl-defun sensetion--mongo-find (db collection query &key sort projection)
  (let* ((query-json (json-encode query))
         (projection-json
          (and projection (json-encode projection)))
	 (args (list db "--quiet" "--norc"
		     (sensetion--mongo-cmd-file
		      (format "db['%s'].find(%s)%s.forEach(printjsononeline)"
			      ;; FIXME: use `prin1-to-string' so that
			      ;; we don't have to worry about escaping
			      ;; problems and thus having to check
			      ;; collection names?
			      collection
			      (if projection
				  (format "%s, %s" query-json projection-json)
				query-json)
			      (if sort
				  (format ".sort(%s)" (json-encode-alist sort))
				""))))))
    (sensetion--mongo-cmd args)))


(cl-defun sensetion--mongo-distinct (db collection field &optional query)
  (let ((args (list db "--quiet" "--norc"
		    (sensetion--mongo-cmd-file
		     (format "db['%s'].distinct(%s).forEach(printjsononeline)"
			     collection
			     (if query
				 (format "\"%s\", %s" field (json-encode query))
			       (format "\"%s\"" field)))))))
    (sensetion--mongo-cmd args)))


(defun sensetion--mongo-replace-one (db collection query replacement)
  "In DB.COLLECTION update records matching QUERY with the contents of REPLACEMENT."
  (let* ((query-json (json-encode-alist query))
         (replacement-json (json-encode-alist replacement))
         (args (list db "--quiet" "--norc"
		     (sensetion--mongo-cmd-file
		      (format "printjson(db['%s'].replaceOne(%s, %s))"
			      collection query-json replacement-json)))))
    (sensetion--mongo-cmd args)))


(cl-defmethod sensetion--backend-prefix-lemma ((backend sensetion--mongo) prefix)
  (let ((terms (sensetion--mongo-distinct (sensetion--mongo-db backend) (sensetion--mongo-synset-collection backend)
				 "terms" `((terms ($regex . ,(format "^%s" prefix)))))))
    (seq-filter (lambda (term) (string-prefix-p prefix term)) terms)))


(cl-defmethod sensetion--backend-prefix-document-id ((backend sensetion--mongo) prefix)
  (sensetion--mongo-distinct (sensetion--mongo-db backend) (sensetion--mongo-document-collection backend)
		    "doc_id" `((doc_id ($regex . ,(format "^%s" prefix))))))


(cl-defmethod sensetion--backend-get-sorted-doc-sents ((backend sensetion--mongo) doc-id)
  (let ((hits (sensetion--mongo-find (sensetion--mongo-db backend) (sensetion--mongo-document-collection backend)
			    `((doc_id . ,doc-id)) :sort '(("sent_id" . -1)))))
    (mapcar #'sensetion--alist->sent hits)))


(defun sensetion--mongo-lemma-pos->docs (backend lemma pos)
  ;; FIXME: can we now assume that lemmas always have PoS/synset-type?
  (let ((synset-type-regexp (string-join (sensetion--pos->synset-types pos))))
    (sensetion--mongo-find (sensetion--mongo-db backend) (sensetion--mongo-document-collection backend)
		  `((tokens.lemmas ($regex . ,(format "^%s(%%[%s])?$"
						      lemma
						      synset-type-regexp))))
		  :sort '(("_id" . -1)))))


(defun sensetion--mongo-lemma->docs (backend lemma)
  (sensetion--mongo-find (sensetion--mongo-db backend) (sensetion--mongo-document-collection backend)
		`((tokens.lemmas ($regex . ,(format "^%s(%%[1-5])?$" lemma))))
		:sort '(("_id" . -1))))


(cl-defmethod sensetion--backend-get-sorted-sents ((backend sensetion--mongo) lemma &optional pos)
  (let* ((lemma (sensetion--spaces->underlines lemma))
	 (sents (if pos
		   (sensetion--mongo-lemma-pos->docs backend lemma pos)
		 (sensetion--mongo-lemma->docs backend lemma))))
    (mapcar #'sensetion--alist->sent sents)))


(cl-defmethod sensetion--backend-id->sent ((backend sensetion--mongo) sent-id)
  (pcase (sensetion--mongo-find (sensetion--mongo-db backend) (sensetion--mongo-document-collection backend)
		       `((_id . ,sent-id)))
    ('()
     (error "Sentence with id %s not found" sent-id))
    (`(,sent)
     (sensetion--alist->sent sent))
    (_
     (error "More than one sentence with id %s" sent-id))))


(cl-defmethod sensetion--backend-update-modified-sent ((backend sensetion--mongo) sent)
  (let ((result (sensetion--mongo-replace-one (sensetion--mongo-db backend)
				     (sensetion--mongo-document-collection backend)
				     `((_id . ,(sensetion--sent-id sent)))
				     (sensetion--sent->alist sent))))
    (if result
	(pcase (cl-first result)
	  ((map ('acknowledged t) ('matchedCount 1) ('modifiedCount 1))
	   t)
	  (error-doc
	   (error "Updated failed with %s" error-doc)))
      (error "No result"))))


(cl-defmethod sensetion--backend-lemma->sorted-synsets ((backend sensetion--mongo) lemma pos)
  (let* ((lemma (sensetion--spaces->underlines lemma))
	 (poses (sensetion--pos->strings pos))
	 (docs  (sensetion--mongo-find (sensetion--mongo-db backend) (sensetion--mongo-synset-collection backend)
			      `((terms . ,lemma) (pos . (($in . ,poses))))
			      :sort '(("_id" . -1)))))
    (mapcar #'sensetion--alist->synset docs)))

(provide 'sensetion-client)

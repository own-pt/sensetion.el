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

(cl-defgeneric sensetion-backend-prefix-lemma (backend prefix)
  "Return a list of lemmas with PREFIX as prefix")

(cl-defgeneric sensetion-backend-prefix-document-id (backend prefix)
  "Return a list of doc-id with PREFIX as prefix")

(cl-defgeneric sensetion--backend-get-sorted-doc-sents (backend doc-id)
  "Given the DOC-ID return a list of sensetion--sent objects
  sorted by sent-id")

(cl-defgeneric sensetion--backend-get-sents (backend lemma &optional pos)
  "Return a list of sensetion--sent objects thats cotains tokens
  with LEMMA and/or POS")

(cl-defgeneric sensetion--backend-id->sent (backend sent_id)
  "Return the sent given a SENT_ID")

(cl-defgeneric sensetion--backend-update-modified-sent (backend sent)
  "Update the backend info SENT content")

(cl-defgeneric sensetion--backend-lemma->synsets (backend lemma pos)
  "Return a list of sensetion--synset objects given a LEMMA and a
  POS.")

(defun sensetion-client-prefix-lemma (prefix)
  "See `sensetion-backend-prefix-lemma'"
  (sensetion-backend-prefix-lemma (sensetion--project-backend sensetion-current-project) prefix))

(defun sensetion-client-prefix-document-id (prefix)
  "See `sensetion-backend-prefix-document-id'"
  (sensetion-backend-prefix-document-id (sensetion--project-backend sensetion-current-project) prefix))

(defun sensetion--client-get-sorted-doc-sents (doc-id)
  "See `sensetion--backend-get-sorted-doc-sents'"
  (sensetion--backend-get-sorted-doc-sents (sensetion--project-backend sensetion-current-project) doc-id))

(defun sensetion--client-get-sents (lemma &optional pos)
  "See `sensetion--backend-get-sents'"
  (sensetion--backend-get-sents (sensetion--project-backend sensetion-current-project) lemma pos))

(defun sensetion--client-id->sent (sent_id)
  "See `sensetion--backend-id->sent'"
  (sensetion--backend-id->sent (sensetion--project-backend sensetion-current-project) sent_id))

(defun sensetion--client-update-modified-sent (sent)
  "See `sensetion--backend-update-modified-sent'"
  (let ((sent (sensetion--remove-man-now sent)))
    (sensetion--backend-update-modified-sent (sensetion--project-backend sensetion-current-project) sent)))

(defun sensetion--client-lemma->synsets (lemma pos)
  "See `sensetion--backend-lemma->synsets'"
  (sensetion--backend-lemma->synsets (sensetion--project-backend sensetion-current-project) lemma pos))


;;; elasticsearch backend
(defvar sensetion--es-headers '(("Content-Type" . "application/json ; charset=UTF-8")))

(defvar sensetion--es-size-params '(("size" . "10000")))

(defsubst sensetion--json-read ()
  (decode-coding-region (point-min) (point-max) 'utf-8)
  (let ((json-array-type 'list))
    (json-read)))

(cl-defun sensetion--es-request-debug-fn (&key data error-thrown &allow-other-keys)
  (let ((inhibit-message t))
    (message "%s" (json-encode-alist data)))
  (message "Done %s "
	   (cl-case (map-elt data 'errors 'unsure #'eq)
	     (:json-false "with no errors")
	     (t  "with errors"))))


(cl-defun sensetion--es-request (path &key data type params (sync t) debug)
  (let* ((response (request (format "%s:%s/%s" sensetion-backend-url sensetion-backend-port path)
			    :headers sensetion--es-headers :parser #'sensetion--json-read
			    :params params :type type
			    :sync t :data data :complete (when debug #'sensetion--es-request-debug-fn)))
	 (data (request-response-data response)))
    (if-let ((error? (map-elt data 'error)))
	(error "Elasticsearch error %s" error?)
      data)))


(cl-defun sensetion--es-query (path data &key (type "GET") params (sync t) debug)
  (let* ((data (sensetion--es-request path :data data :type type :params params
			     :sync sync :debug debug))
	 (hits (map-elt (map-elt data 'hits nil #'eq) 'hits nil #'eq))
	 (docs (mapcar (lambda (hit) (map-elt hit '_source)) hits)))
    docs))


(cl-defmethod sensetion-backend-prefix-lemma ((backend (eql es)) prefix)
  (let* ((query `((query
		   (prefix
		    (terms . ,prefix)))))
	 (query (json-encode-alist query))
	 (hits  (sensetion--es-query "sensetion-synsets/_search"
			    query
			    :params sensetion--es-size-params))
	 (terms (seq-mapcat (lambda (doc) (map-elt doc 'terms)) hits)))
    (seq-filter (lambda (lemma) (string-prefix-p prefix lemma t)) terms)))


(cl-defmethod sensetion-backend-prefix-document-id ((backend (eql es)) prefix)
  (let* ((query `((query
		   (prefix
		    (doc_id . ,prefix)))))
	 (query (json-encode-alist query))
	 (hits  (sensetion--es-query "sensetion-docs/_search"
			    query
			    :params sensetion--es-size-params))
	 (document-ids (cl-map 'list (lambda (doc) (map-elt doc 'doc_id)) hits)))
    (seq-filter (lambda (document-id) (string-prefix-p prefix document-id)) document-ids)))


(cl-defmethod sensetion--backend-lemma->synsets ((backend (eql es)) lemma pos)
  (let* ((query `((query
		   (bool
		    (filter .
			    [((term
			       (terms . ,lemma)))
			     ((term
			       (pos . ,pos)))])))))
	 (query (json-encode-alist query))
	 (hits (sensetion--es-query "sensetion-synsets/_search"
			   query
			   :params sensetion--es-size-params)))
    (mapcar #'sensetion--alist->synset hits)))


(cl-defmethod sensetion--backend-id->sent ((backend (eql es)) sent-id)
  (map-elt
   (sensetion--es-request (format "sensetion-docs/_doc/%s" sent-id))
   '_source nil #'eq))


(cl-defmethod sensetion--backend-get-sents ((backend (eql es)) lemma &optional pos)
  (let* ((lemma (cl-substitute ?_ (string-to-char " ") lemma :test #'eq))
	 (docs (if pos
		   (sensetion--es-lemma-pos->docs lemma pos)
		 (sensetion--es-lemma->docs lemma))))
    (mapcar #'sensetion--alist->sent docs)))


(defun sensetion--es-lemma-pos->docs (lemma pos)
  (let* ((query `((query
		   (nested
		    (path . "tokens")
		    (query
		     (regexp
		      (tokens\.lemmas . ,(format "%s(%%%s)?" lemma (sensetion--pos->synset-type pos)))))))
		  (sort . ("doc_id" "sent_id"))))
	 (query (json-encode-alist query))
	 (hits  (sensetion--es-query "sensetion-docs/_search"
			    query
			    :params sensetion--es-size-params)))
    hits))


(defun sensetion--es-lemma->docs (lemma)
  (let* ((query `((query
		   (nested
		    (path . "tokens")
		    (query
		     (regexp
		      (tokens\.lemmas . ,(format "%s(%%[1-4])?" lemma))))))
		  (sort . ("doc_id" "sent_id"))))
	 (query (json-encode-alist query))
	 (hits (sensetion--es-query "sensetion-docs/_search"
			   query
			   :params sensetion--es-size-params)))
    hits))


(cl-defmethod sensetion--backend-get-sorted-doc-sents ((backend (eql es)) doc-id)
  (let* ((query `((query
		   (term
		    (doc_id . ,doc-id)))
		  (sort . ("sent_id"))))
	 (query (json-encode-alist query))
	 (hits (sensetion--es-query "sensetion-docs/_search"
			   query
			   :params sensetion--es-size-params)))
    (mapcar #'sensetion--alist->sent hits)))


(defun sensetion--remove-man-now (sent)
  (cl-labels
      ((remove-man-now (tk)
		       (pcase tk
			 ((cl-struct sensetion--tk tag)
			  (when (equal (sensetion--tk-tag tk) "man-now")
			    (setf (sensetion--tk-tag tk) "man"))))
		       tk))
    (pcase sent
      ((cl-struct sensetion--sent tokens)
       (setf (sensetion--sent-tokens sent) (mapcar #'remove-man-now tokens))))
    sent))


(cl-defmethod sensetion--backend-update-modified-sent ((backend (eql es)) sent)
  (let ((data (encode-coding-string (json-encode-alist (sensetion--sent->alist sent)) 'utf-8 t)))
    (sensetion--es-request (format "sensetion-docs/_doc/%s" (sensetion--sent-id sent))
		  ;; DISCUSS: could be made async, but then might have
		  ;; race condition?
		  :data data :type "PUT")))

;;; mongodb backend

(cl-defstruct (sensetion--mongo (:constructor nil)
		       (:constructor sensetion-make-mongo))
  (db "sensetion-database") (synset-collection "synsets") (document-collection "documents"))


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


(cl-defun sensetion--mongo-find (db collection query &key sort projection)
  (let* ((query-json (json-encode query))
         (projection-json
          (and projection (json-encode projection)))
	 (args (list db "--quiet" "--norc"
		     (sensetion--mongo-cmd-file
		      (format "db.%s.find(%s)%s.forEach(function(myDoc) { printjsononeline(myDoc); })"
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
		     (format "db.%s.distinct(%s).forEach(function(myDoc) { printjsononeline(myDoc); })"
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
		      (format "printjson(db.%s.replaceOne(%s, %s))"
			      collection query-json replacement-json)))))
    (sensetion--mongo-cmd args)))


(cl-defmethod sensetion-backend-prefix-lemma ((backend sensetion--mongo) prefix)
  ;; FIXME: use distinct
  (let* ((hits  (sensetion--mongo-find (sensetion--mongo-db backend)
			      (sensetion--mongo-synset-collection backend)
			      `((terms ($regex . ,(format "^%s" prefix))))
			      :projection '((terms . 1) (_id . 0))))
	 (terms (seq-mapcat (lambda (doc) (map-elt doc 'terms nil #'eq)) hits)))
    (seq-filter (lambda (term) (string-prefix-p prefix term)) terms)))


(cl-defmethod sensetion-backend-prefix-document-id ((backend sensetion--mongo) prefix)
  (sensetion--mongo-distinct (sensetion--mongo-db backend) (sensetion--mongo-document-collection backend)
		    "doc_id" `((doc_id ($regex . ,(format "^%s" prefix))))))


(cl-defmethod sensetion--backend-get-sorted-doc-sents ((backend sensetion--mongo) doc-id)
  (let ((hits (sensetion--mongo-find (sensetion--mongo-db backend) (sensetion--mongo-document-collection backend)
			    `((doc_id . ,doc-id)) :sort '(("sent_id" . -1)))))
    (mapcar #'sensetion--alist->sent hits)))


(defun sensetion--mongo-lemma-pos->docs (backend lemma pos)
  ;; FIXME: can we now assume that lemmas always have PoS/synset-type?
  (sensetion--mongo-find (sensetion--mongo-db backend) (sensetion--mongo-document-collection backend)
		`((tokens.lemmas ($regex . ,(format "^%s(%%%s)?"
						    lemma
						    (sensetion--pos->synset-type pos)))))))


(defun sensetion--mongo-lemma->docs (backend lemma)
  (sensetion--mongo-find (sensetion--mongo-db backend) (sensetion--mongo-document-collection backend)
		`((tokens.lemmas ($regex . ,(format "^%s(%%[1-5])?" lemma))))))


(cl-defmethod sensetion--backend-get-sents ((backend sensetion--mongo) lemma &optional pos)
  (let* ((lemma (cl-substitute ?_ (string-to-char " ") lemma :test #'eq))
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


(cl-defmethod sensetion--backend-lemma->synsets ((backend sensetion--mongo) lemma pos)
  (let* ((lemma (cl-substitute ?_ (string-to-char " ") lemma :test #'eq))
	 (docs (sensetion--mongo-find (sensetion--mongo-db backend) (sensetion--mongo-synset-collection backend)
			   `((terms . ,lemma) (pos . ,pos)))))
    (mapcar #'(lambda (doc) (sensetion--alist->synset (cdr doc)))
	    docs)))

(provide 'sensetion-client)

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


(defvar sensetion--es-headers '(("Content-Type" . "application/json")))

(defvar sensetion--es-size-params '(("size" . "10000")))


(defsubst sensetion--json-read ()
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
			    :params params
			    :sync t :data data :complete (when debug #'sensetion--es-request-debug-fn)))
	 (data (request-response-data response)))
    data))


(cl-defun sensetion--es-query (path data &key (type "GET") params (sync t) debug)
  (let* ((data (sensetion--es-request path :data data :type type :params params
			     :sync sync :debug debug))
	 (hits (map-elt (map-elt data 'hits nil #'eq) 'hits nil #'eq))
	 (docs (mapcar (lambda (hit) (map-elt hit '_source)) hits)))
    docs))


(defun sensetion-es-prefix-lemma (prefix)
  (let* ((query `((query
		   (prefix
		    (terms . ,prefix)))))
	 (query (json-encode-alist query))
	 (hits  (sensetion--es-query "sensetion-synsets/_search"
			    query
			    :params sensetion--es-size-params))
	 (terms (seq-mapcat (lambda (doc) (map-elt doc 'terms)) hits)))
    (seq-filter (lambda (lemma) (string-prefix-p prefix lemma t)) terms)))


(defun sensetion--es-lemma->synsets (lemma pos)
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


(defun sensetion--es-id->sent (sent-id)
  (map-elt
   (sensetion--es-request (format "sensetion-docs/_doc/%s" sent-id))
   '_source nil #'eq))


(defun sensetion--es-get-sents (lemma &optional pos)
  (let* ((lemma (cl-substitute ?_ ?  lemma :test #'eq))
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
		  (sort . ("id"))))
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
		  (sort . ("id"))))
	 (query (json-encode-alist query))
	 (hits (sensetion--es-query "sensetion-docs/_search"
			   query
			   :params sensetion--es-size-params)))
    hits))


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


(defun sensetion--es-update-modified-sent (sent)
  (let* ((sent (sensetion--remove-man-now sent))
	 (data (json-encode-alist (sensetion--sent->alist sent))))
    (sensetion--es-request (format "sensetion-docs/_doc/%s" (sensetion--sent-id sent))
		  ;; DISCUSS: could be made async, but then might have
		  ;; race condition?
		  :data data :type "PUT")))


(provide 'sensetion-client)

;;; sensetion.el --- -*- lexical-binding: t; -*-

(require 'json)
(require 'request)
(require 'f)
(require 'map)
(require 'seq)

(require 'sensetion-data)
(require 'sensetion-utils)


(defvar sensetion--es-headers '(("Content-Type" . "application/json")))


(defsubst sensetion--json-read ()
  (let ((json-array-type 'list))
    (json-read)))

(cl-defun sensetion--es-request-debug-fn (&key data error-thrown &allow-other-keys)
  (print error-thrown)
  (print data))


(defun sensetion--es-request (path data &optional debug)
  (let* ((response (request (string-join (list sensetion-backend-url path) "/")
			    :headers sensetion--es-headers :parser #'sensetion--json-read
			    :sync t :data data :error (when debug #'sensetion--es-request-debug-fn)))
	 (data (request-response-data response))
	 (hits (map-elt (map-elt data 'hits nil #'eq) 'hits nil #'eq))
	 (docs (mapcar (lambda (hit) (map-elt hit '_source)) hits)))
    docs))

(defun sensetion-es-prefix-lemma (prefix &optional limit)
  ;; TODO: FIXME
  (let* ((template "{\"query\":{\"prefix\" : { \"terms\" : \"%s\" }}}")
	 (hits (sensetion--es-request "_search" (format  template prefix)))
	 (terms (seq-mapcat (lambda (doc) (map-elt doc 'terms)) hits)))
    (seq-take (seq-filter (lambda (lemma) (string-prefix-p prefix lemma t)) terms) limit)))


(defun sensetion--es-lemma->synsets (lemma pos)
  (let* ((template "{\"query\": {\"bool\": { \"filter\": [{\"term\":
                           {\"terms\": \"%s\"}}, {\"term\":{ \"pos\" : \"%s\"}}]}}}")
	 (hits (sensetion--es-request "_search"
			     (format template lemma pos))))
    (mapcar #'sensetion--alist->synset hits)))


(defun sensetion--es-get-sents (lemma &optional pos)
  (let ((docs (if pos
		  (sensetion--es-lemma-pos->docs lemma pos)
		(sensetion--es-lemma->docs lemma))))
    (mapcar #'sensetion--alist->sent docs)))


(defun sensetion--es-lemma-pos->docs (lemma pos)
  (let* ((template "{\"query\": {\"nested\": {\"query\": {\"regexp\":
                    {\"tokens.lemmas\": \"%s(%%%s)?\"}}, \"path\": \"tokens\" }}}")
	 (query (format template lemma (sensetion--pos->synset-type pos)))
	 (hits (sensetion--es-request "docs/_search" query)))
    hits))


(defun sensetion--es-lemma->docs (lemma)
  (let* ((template "{\"query\": {\"nested\": {\"path\": \"tokens\",
                       \"query\": {\"regexp\": {\"tokens.lemmas\": \"%s(%%[1-4])?\"}}}}}")
	 (query (format template lemma))
	 (hits (sensetion--es-request "docs/_search" query)))
    hits))


(provide 'sensetion-client)

;;; sensetion.el --- -*- lexical-binding: t; -*-

(require 'url)
(require 'json)
(require 'request)
(require 'f)
(require 'map)
(require 'seq)

;; start ES and run (sensetion-es-index-file-lines
;; "http://localhost:9200/synsets/_bulk" "~/synsets.json") to index
;; synsets

;; ## create index
;; curl -X PUT "localhost:9200/synsets" -H 'Content-Type: application/json' -d'
;; {
;;   "mappings": {
;;     "properties": {
;;       "lemmas": {
;;         "type":  "keyword"
;;       }
;;     }
;;   }
;; }'

;; ##delete index
;; curl -XDELETE http://localhost:9200/synsets/

;; ## query for lemma
;; curl -X POST "localhost:9200/_search" -H 'Content-Type: application/json' -d'{  "query": {    "term" : { "lemmas" : "rock" }   }}'

;; ## query for lemma and string
;; curl -X GET "localhost:9200/_search?pretty" -H 'Content-Type: application/json' -d'{"query": {"bool": { "filter": [{"term": {"lemmas": "rock" }}, {"term" : { "pos" : "a" }} ] }}}'


(defvar sensetion--es-url "http://localhost:9200")

(defvar sensetion--es-headers '(("Content-Type" . "application/json")))

(defun sensetion--json-read ()
  (let ((json-array-type 'list))
    (json-read)))

(defun sensetion-es-prefix-lemma (prefix)
  ;; TODO: FIXME
  (let* ((response (request (f-join sensetion--es-url "_search") :headers sensetion--es-headers :parser #'sensetion--json-read :sync t
			    :data (format "{\"query\":{\"prefix\" : { \"lemmas\" : \"%s\" }}}" prefix)))
	 (data (request-response-data response))
	 (hits (map-elt (map-elt data 'hits) 'hits))
	 (docs (mapcar (lambda (hit) (map-elt hit '_source)) hits))
	 (lemmas (seq-mapcat (lambda (doc) (map-elt doc 'lemmas)) docs)))
    (seq-filter (lambda (lemma) (string-prefix-p prefix lemma t)) lemmas)))


(defun sensetion-es-index-file-lines (url fp)
  (with-temp-buffer
    (insert-file-contents fp)
    (sensetion--buffer-intersperse "{ \"index\" : {} }" t)
    (let ((url-request-data (buffer-string))
	  (url-request-method "POST")
	  (url-request-extra-headers '(("Content-Type" . "application/json"))))
      (url-retrieve url (lambda (status &rest bla) (print status)))))
  nil)

(provide 'sensetion-client)

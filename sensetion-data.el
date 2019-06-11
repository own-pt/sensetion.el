;;; sensetion.el --- -*- lexical-binding: t; -*-
(eval-when-compile (require 'cl-lib))
(require 'cl-lib)
(require 'map)
(require 's)

;; TODO: maybe use maps all the way
(cl-defstruct (sensetion--tk (:constructor nil)
                    (:constructor sensetion--make-tk))
  kind form lemmas tag senses glob unsure meta)


(cl-defstruct (sensetion--synset (:constructor nil)
                        (:constructor sensetion--make-synset))
  ofs pos keys terms gloss)


(cl-defstruct (sensetion--sent (:constructor nil)
		      (:constructor sensetion--make-sent)
		      (:copier nil)
		      (:copier sensetion--copy-sent))
  doc-id sent-id meta tokens text)


(defun sensetion--alist->synset (alist)
  (pcase alist
    ((map ofs pos keys terms gloss)
     (sensetion--make-synset :ofs ofs :pos pos :keys keys :terms terms :gloss gloss))))


(defun sensetion--alist->sent (alist)
  (pcase alist
    ((map doc_id sent_id meta tokens text)
     (sensetion--make-sent :doc-id doc_id :sent-id sent_id :meta meta
		  :tokens (mapcar #'sensetion--alist->tk tokens) :text text))))


(defun sensetion--alist->tk (alist)
  (pcase alist
    ((map kind form lemmas tag senses glob unsure meta)
     (sensetion--make-tk :kind kind :form form :lemmas lemmas
		:tag tag :senses senses :glob glob :unsure unsure :meta meta))))


(defun sensetion--sent->alist (sent)
  (pcase sent
    ((cl-struct sensetion--sent doc-id sent-id meta tokens text)
     (cl-mapcan
      (lambda (k v) (when v (list (cons k v))))
      '(doc_id sent_id meta tokens text)
      (list doc-id
	    sent-id
	    meta
	    (mapcar #'sensetion--tk->alist tokens)
	    text)))))


(defun sensetion--tk->alist (tk)
  (pcase tk
    ((cl-struct sensetion--tk kind form lemmas tag senses glob unsure meta)
     (cl-mapcan
      (lambda (k v) (when v (list (cons k v))))
      '(kind form lemmas tag senses glob unsure meta)
      (list kind
	    form
            lemmas
	    tag
            senses
            glob
	    unsure
	    meta)))))


(defun sensetion--tk-confident-in-anno? (tk)
  (not (sensetion--tk-unsure tk)))


(defalias 'sensetion--synset-senses #'sensetion--synset-keys)


(defalias 'sensetion--tk-skeys #'sensetion--tk-senses)


(defun sensetion--sk-st (sk)
  (pcase (s-split ":" sk)
    (`(,lemma* ,_ ,_ ,_ ,_)
     (sensetion--lemma*->st lemma*))
    (_ (error "Malformed sense key %s" sk))))


(defun sensetion--synset-id (synset)
  (concat (number-to-string (sensetion--synset-ofs synset))
          "-"
          (sensetion--synset-pos synset)))


(defun sensetion--sent-id (sent)
  (format "%s-%s"
	  (sensetion--sent-doc-id  sent)
	  (sensetion--sent-sent-id sent)))


(defun sensetion--make-lemma* (lemma &optional synset-type)
  (if synset-type
      (concat lemma "%" synset-type)
    lemma))


(defun sensetion--lemma*->lemma (lemma*)
  (let ((st (sensetion--lemma*->st lemma*)))
    (if st
        (substring lemma* 0 (- (length lemma*) 2))
      lemma*)))

(defun sensetion--lemma*->st (lemma*)
  (let ((len (length lemma*)))
    (when (and (> len 1) (= (elt lemma* (- len 2)) (string-to-char "%")))
      (substring lemma* (1- len)))))


(provide 'sensetion-data)

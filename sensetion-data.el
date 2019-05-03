;;; sensetion.el --- -*- lexical-binding: t; -*-
(eval-when-compile (require 'cl-lib))
(require 'cl-lib)
(require 'map)
(require 's)

;; TODO: maybe use maps all the way
(cl-defstruct (sensetion--tk (:constructor nil)
                    (:constructor sensetion--make-tk))
  kind form lemmas pos tag senses glob unsure meta)


(cl-defstruct (sensetion--synset (:constructor nil)
                        (:constructor sensetion--make-synset))
  ofs pos keys terms gloss)


(cl-defstruct (sensetion--sent (:constructor nil)
		      (:constructor sensetion--make-sent))
  id meta tokens text)


(defun sensetion--alist->synset (alist)
  (pcase alist
    ((map ofs pos keys terms gloss)
     (sensetion--make-synset :ofs ofs :pos pos :keys keys :terms terms :gloss gloss))))


(defun sensetion--alist->sent (alist)
  (pcase alist
    ((map id meta token raw-text)
     (sensetion--make-sent :id id :meta meta :tokens (mapcar #'sensetion--alist->tk token) :text raw-text))))


(defun sensetion--alist->tk (alist)
  (pcase alist
    ((map kind form lemmas pos tag senses glob unsure meta)
     (sensetion--make-tk :kind (s-split ":" kind t) :form form :lemmas lemmas :pos pos
		:tag tag :senses senses :glob glob :unsure unsure :meta meta))))


(defun sensetion--sent->alist (sent)
  (pcase sent
    ((cl-struct sensetion--sent id meta tokens text)
     (cl-mapcan
      (lambda (k v) (when v (cons k v)))
      '(meta tokens text)
      (list id
	    meta
	    (mapcar #'sensetion--tk->alist tokens)
	    text)))))


(defun sensetion--tk->alist (tk)
  (pcase tk
    ((cl-struct sensetion--tk kind form lemmas pos tag senses glob unsure meta)
     (cl-mapcan
      (lambda (k v) (when v (cons k v)))
      '(kind form lemmas pos tag senses glob unsure)
      (list (s-join ":" kind)
	    form
            lemmas
	    pos
	    ;; "man-now" is a virtual token status, shouldn't be
            ;; written to file
            (if (equal tag "man-now")
                "man"
              tag)
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


(defun sensetion--lemma*->st (lemma*)
  (let ((len (length lemma*)))
    (when (and (> len 1) (= (elt lemma* (- len 2)) (string-to-char "%")))
      (substring lemma* (1- len)))))


(provide 'sensetion-data)

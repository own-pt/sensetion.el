;;; sensetion.el --- -*- lexical-binding: t; -*-
(eval-when-compile (require 'cl-lib))
(require 'cl-lib)
(require 'map)

;; TODO: maybe use maps all the way
(cl-defstruct (sensetion--tk (:constructor nil)
                    (:constructor sensetion--make-tk))
  kind form lemmas tag senses glob unsure meta)


(cl-defstruct (sensetion--synset (:constructor nil)
                        (:constructor sensetion--make-synset))
  ofs pos keys terms gloss)


(cl-defstruct (sensetion--sent (:constructor nil)
		      (:constructor sensetion--make-sent))
  id meta tokens text)


(defmacro sensetion--mk-struct-arglist (alist st-fields &optional al-fields)
  `(list
    ,@(cl-mapcan
       (lambda (st-field al-field)
	 (list (intern (concat ":" (symbol-name st-field)))
	       `(map-elt ,alist ',al-field nil #'eq)))
       st-fields (or al-fields st-fields))))


(defun sensetion--alist->synset (alist)
  (apply #'sensetion--make-synset (sensetion--mk-struct-arglist alist (ofs pos keys terms gloss))))


(defun sensetion--alist->sent (alist)
  (apply #'sensetion--make-sent
	 (sensetion--mk-struct-arglist alist (id meta tokens text) (id meta token raw-text))))


(defun sensetion--alist->tk (alist)
  (apply #'sensetion--make-tk
	 (sensetion--mk-struct-arglist
	  alist
	  (kind form lemmas tag senses glob unsure meta))))


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
    ((cl-struct sensetion--tk kind form lemmas tag senses glob unsure meta)
     (cl-mapcan
      (lambda (k v) (when v (cons k v)))
      '(kind form lemma tag senses glob unsure)
      (list kind
	    form
            lemmas
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

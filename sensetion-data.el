;;; sensetion.el --- -*- lexical-binding: t; -*-
(eval-when-compile (require 'cl-lib))
(require 'cl-lib)


(cl-defstruct (sensetion--tk (:constructor nil)
                    (:constructor sensetion--make-tk))
  kind form lemma pos tag senses glob unsure sep type rdf action rend)


(cl-defstruct (sensetion--synset (:constructor nil)
                        (:constructor sensetion--make-synset))
  ofs pos keys lemmas gloss)


(defun sensetion--alist->synset (plist)
  (sensetion--make-synset :ofs (map-elt plist 'ofs nil #'eq)
                 :pos (map-elt plist 'pos nil #'eq)
                 :keys (map-elt plist 'keys nil #'eq)
		 :lemmas (map-elt plist 'lemmas nil #'eq)
                 :gloss (map-elt plist 'gloss nil #'eq)))


(defun sensetion--plist->tk (plist)
  (apply #'sensetion--make-tk plist))


(defun sensetion--tk->plist (tk)
  (pcase tk
    ((cl-struct sensetion--tk kind form lemma pos tag senses glob sep type rdf unsure action rend)
     (cl-mapcan
      (lambda (k v) (when v (list k v)))
      '(:kind :action :form :lemma :pos :tag :senses :glob :sep :type :rdf :unsure :rend)
      (list kind
            action
            form
            lemma
            pos
            ;; "man-now" is a virtual token status, shouldn't be
            ;; written to file
            (if (equal tag "man-now")
                "man"
              tag)
            senses
            glob
            sep
            type
            rdf
            unsure
            rend)))))


(defun sensetion--tk-confident-in-anno? (tk)
  (not (sensetion--tk-unsure tk)))


(defalias 'sensetion--synset-terms #'sensetion--synset-lemmas)


(defalias 'sensetion--synset-senses #'sensetion--synset-keys)


(defun sensetion--tk-skeys (tk)
  (mapcar #'car (sensetion--tk-senses tk)))


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

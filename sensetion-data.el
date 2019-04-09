;;; sensetion.el --- -*- lexical-binding: t; -*-
(eval-when-compile (require 'cl-lib))
(require 'cl-lib)


(cl-defstruct (sensetion--tk (:constructor nil)
                    (:constructor sensetion--make-tk))
  kind form lemma pos tag senses glob unsure sep type rdf action rend)


(cl-defstruct (sensetion--synset (:constructor nil)
                        (:constructor sensetion--make-synset))
  ofs pos keys gloss tokens)


(defun sensetion--plist->synset (plist)
  (sensetion--make-synset :ofs (plist-get plist :ofs)
                 :pos (plist-get plist :pos)
                 :keys (plist-get plist :keys)
                 :gloss (plist-get plist :gloss)
                 :tokens (mapcar #'sensetion--plist->tk
                                 (plist-get plist :tokens))))


(defun sensetion--plist->tk (plist)
  (apply #'sensetion--make-tk plist))


(defun sensetion--synset->plist (synset)
  (pcase synset
    ((cl-struct sensetion--synset ofs pos keys gloss tokens)
     (list :ofs ofs
           :pos pos
           :keys keys
           :gloss gloss
           :tokens (mapcar #'sensetion--tk->plist tokens)))))


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


(defun sensetion--synset-terms (synset)
  (mapcar #'cdr (sensetion--synset-keys synset)))


(defun sensetion--synset-senses (synset)
  (mapcar #'car (sensetion--synset-keys synset)))


(defun sensetion--tk-skeys (tk)
  (mapcar #'car (sensetion--tk-senses tk)))


(defun sensetion--sk-st (sk)
  (pcase (s-split ":" sk)
    (`(,lemma* ,_ ,_ ,_ ,_)
     (sensetion--lemma*->st lemma*))
    (_ (error "Malformed sense key %s" sk))))


(defun sensetion--synset-id (synset)
  (concat (sensetion--synset-ofs synset)
          "-"
          (sensetion--synset-pos synset)))


(defun sensetion--lemma*->st (lemma*)
  (let ((len (length lemma*)))
    (when (and (> len 1) (= (elt lemma* (- len 2)) (string-to-char "%")))
      (substring lemma* (1- len)))))


(provide 'sensetion-data)

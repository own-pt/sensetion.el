;;; sensetion.el --- -*- lexical-binding: t; -*-
(eval-when-compile (require 'cl-lib))
(require 'cl-lib)
(require 'map)
(require 's)

(cl-defstruct (sensetion--project (:constructor nil)
			 (:constructor sensetion-make-project))
    "Structure representing an annotation project.

Slots:

`name'
     The name of the project.

`backend'
     The database backend used by this project.

`output-buffer-name' (optional, default: sensetion)
     The buffer name where sensetion results are displayed.

`display-meta-data-fn' (optional)
     Function used to display meta data during target mode. When
     not defined nothing is displayed.

`restrict-lemmas' (optional, default: t)
     When non-nil restrict the user to add only lemmas that is part of
     wordnet to a token or a glob.
"

    (name (error "You must provide a name"))
    (backend (error "You must provide a backend"))
    (output-buffer-name "sensetion")
    display-meta-data-fn
    (restrict-lemmas t))

;; TODO: maybe use maps all the way
(cl-defstruct (sensetion--tk (:constructor nil)
                    (:constructor sensetion--make-tk))
  kind form lemmas tag senses glob unsure meta)


(cl-defstruct (sensetion--synset (:constructor nil)
                        (:constructor sensetion--make-synset))
  lexname pos keys terms def exs)


(cl-defstruct (sensetion--sent (:constructor nil)
		      (:constructor sensetion--make-sent)
		      (:copier nil)
		      (:copier sensetion--copy-sent))
  doc-id sent-id meta tokens text)


(defun sensetion--alist->synset (alist)
  (pcase alist
    ((map lexnames pos keys terms definition examples)
     (sensetion--make-synset :pos pos :keys keys :terms terms :def definition :exs examples))))


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


(defun sensetion--sensekey-lexical-id (sk)
  (pcase (s-split ":" sk)
    (`(,_ ,_ ,lex-id ,_ ,_)
     lex-id)
    (_ (error "Malformed sense key %s" sk))))


(defun sensetion--sensekey-pos (sk)
  (pcase (s-split ":" sk)
    (`(,lemma* ,_ ,_ ,_ ,_)
     (sensetion--lemma*->pos lemma*))
    (_ (error "Malformed sense key %s" sk))))


(defun sensetion--synset-id (synset)
  (pcase synset
    ((cl-struct sensetion--synset pos lexname keys terms)
     (let ((lexical-id (sensetion--sensekey-lemma-lexical-id (cl-first keys))))
       (format "%s-%s-%s-%s"
	       pos lexname (cl-first terms) lexical-id)))))


(defun sensetion--sent-id (sent)
  (format "%s-%s"
	  (sensetion--sent-doc-id  sent)
	  (sensetion--sent-sent-id sent)))


(defun sensetion--make-lemma* (lemma &optional synset-type)
  (if synset-type
      (concat lemma "%" synset-type)
    lemma))


(defun sensetion--lemma*->lemma (lemma*)
  (cl-destructuring-bind (lemma . _)
      (sensetion--split-lemma+synset-type lemma*)
    lemma))

(defun sensetion--lemma*->pos (lemma+synset-type)
  (cl-destructuring-bind (_ . pos)
      (sensetion--split-lemma+synset-type lemma+synset-type)
    pos))


(defun sensetion--split-lemma+synset-type (lemma+synset-type)
  "Return (cons lemma (synset-type->pos synset-type))."
  (let ((len (length lemma+synset-type)))
    (if (and (> len 1) (= (elt lemma+synset-type (- len 2)) ?%))
	(cons (substring lemma+synset-type 0 (- len 2))
	      (sensetion--synset-type->pos (substring lemma+synset-type (1- len))))
      (list lemma+synset-type))))


(provide 'sensetion-data)

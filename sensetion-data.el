;;; sensetion.el --- -*- lexical-binding: t; -*-
(eval-when-compile (require 'cl-lib))
(require 'cl-lib)


(cl-defstruct (sensetion--tk (:constructor nil)
                             (:constructor sensetion--make-tk))
  form lemma pos status kind anno meta)


(cl-defstruct (sensetion--sent (:constructor nil)
                               (:constructor sensetion--make-sent))
  id terms text tokens)


(defun sensetion--plist->sent (plist)
  (sensetion--make-sent :id (plist-get plist :id)
                        :terms (plist-get plist :terms)
                        :text (plist-get plist :text)
                        :tokens (mapcar #'sensetion--plist->tk
                                        (plist-get plist :tokens))))


(defun sensetion--plist->tk (plist)
  (apply #'sensetion--make-tk plist))


(defun sensetion--sent->plist (sent)
  (pcase sent
    ((cl-struct sensetion--sent id terms text tokens)
     (list :id id
           :terms terms
           :text text
           :tokens (mapcar #'sensetion--tk->plist tokens)))))


(defun sensetion--tk->plist (tk)
  (cl-mapcan #'list '(:form :lemma :pos :status :kind :anno :meta)
             (list (sensetion--tk-form tk)
                   (sensetion--tk-lemma tk)
                   (sensetion--tk-pos tk)
                   (let ((st (sensetion--tk-status tk)))
                     ;; "now" is a virtual token status, shouldn't be
                     ;; written to file
                     (if (equal st "now")
                         "man"
                       st))
                   (sensetion--tk-kind tk)
                   (sensetion--tk-anno tk)
                   (sensetion--tk-meta tk))))


(provide 'sensetion-data)

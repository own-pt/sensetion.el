;;; -*- lexical-binding: t; -*-
(eval-when-compile (require 'cl-lib))
(require 'cl-lib)


(cl-defstruct (sensetion--tk (:constructor nil)
                             (:constructor sensetion--make-tk))
  form lemma status kind anno meta)


(cl-defstruct (sensetion--sent (:constructor nil)
                               (:constructor sensetion--make-sent))
  id tokens)


(defun sensetion--plist->sent (plist)
  (sensetion--make-sent :id (plist-get plist :id)
                        :tokens (mapcar #'sensetion--plist->tk
                                        (plist-get plist :tokens))))


(defun sensetion--plist->tk (plist)
  (let ((form (plist-get plist :form))
        (lemma (plist-get plist :lemma))
        (status (plist-get plist :status))
        (kind (plist-get plist :kind))
        (anno (plist-get plist :anno))
        (meta (plist-get plist :meta)))
    (sensetion--make-tk :form form :lemma lemma
                        :status status :kind kind
                        :anno anno :meta meta)))


(defun sensetion--sent->plist (sent)
  (list :id (sensetion--sent-id sent)
        :tokens (mapcar #'sensetion--tk->plist (sensetion--sent-tokens sent))))


(defun sensetion--tk->plist (tk)
  (cl-mapcan #'list '(:form :lemma :status :kind :anno :meta)
             (list (sensetion--tk-form tk)
                   (sensetion--tk-lemma tk)
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

;;; sensetion.el --- -*- lexical-binding: t; -*-
(eval-when-compile (require 'cl-lib))
(require 'cl-lib)


(cl-defstruct (sensetion--tk (:constructor nil)
                             (:constructor sensetion--make-tk))
  form lemma pos status kind anno meta conf)


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
  (pcase tk
    ((cl-struct sensetion--tk form lemma pos status kind anno meta conf)
     (cl-mapcan #'list '(:form :lemma :pos :status :kind :anno :meta :conf)
                (list form
                      lemma
                      pos
                      ;; "man-now" is a virtual token status,
                      ;; shouldn't be written to file
                      (if (equal status "man-now")
                          "man"
                        status)
                      kind
                      anno
                      meta
                      conf)))))

(defun sensetion--tk-confident-in-anno? (tk)
  (unless (zerop (sensetion--tk-conf tk))
    t))


(provide 'sensetion-data)

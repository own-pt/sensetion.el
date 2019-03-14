;;; sensetion.el --- -*- lexical-binding: t; -*-
(require 'sensetion-data)


(defun sensetion-edit (lemma ix pos sent)
  (interactive (list (buffer-local-value 'sensetion--lemma (current-buffer))
                     (or (get-char-property (point) 'sensetion--glob-ix)
                         (sensetion--tk-ix-prop-at-point))
                     (ido-completing-read "Token PoS tag: " '("n" "v" "a" "r")
                                          nil t nil nil)
                     (sensetion--get-sent-at-point)))
  (unless (sensetion--selected? (point))
    (user-error "Token at point not selected for annotation"))
  (unless lemma
    (error "No local sensetion--lemma; please report bug"))
  (sensetion--edit lemma pos ix sent))


(defun sensetion--edit (lemma pos1 ix sent)
  (let ((senses (cl-loop for k being the hash-keys of sensetion--synset-cache
                         using (hash-values v)
                         when (equal (substring k 0 1) pos1)
                         collect (cons k v))))
    (unless senses
      (user-error "No senses for lemma %s with pos %s" lemma pos1))
    (sensetion--call-hydra lemma (sensetion--pos->synset-type pos1)
                           ix sent senses)))


(defun sensetion--call-hydra (lemma st tk-ix sent options)
  (call-interactively
   (eval (sensetion--edit-hydra-maker lemma st tk-ix sent options))))


(defun sensetion--edit-hydra-maker (lemma st tk-ix sent options)
  "Creates interactive editing hydra on-the-fly."
  (sensetion-is
   `(defhydra hydra-senses (:color blue)
      ""
      ("q" nil nil)
      ("RET" nil nil)
      ("0" ,no-sense-function "No sense in Wordnet" :column "Pick sense:")
      ,@(mapcar
         (lambda (s)
           (cl-destructuring-bind (sid key sense-text) s
             (list key
                   `(lambda () (interactive)
                      (sensetion--add-sense ,lemma
                                            ,st
                                            ,tk
                                            ,sid
                                            ,tk-ix
                                            ,sent)
                      (sensetion--edit-reinsert-state-call
                       ,tk-ix ,sent ,lemma ,st ',options))
                   (sense-help-text sid sense-text)
                   :column "Pick sense:")))
         options)
      ("?" ,not-sure-function "Not sure" :column "Pick sense:"))
   where
   (sense-help-text (sid sense-text)
                    (concat (sense-chosen-ind sid)
                            (s-word-wrap (- (frame-width) 5) sense-text)))
   (sense-chosen-ind (sid) (if (member sid pres-senses) "+ " ""))
   (not-sure-function
    (lambda ()
      (interactive)
      (setf (sensetion--tk-status (elt (sensetion--sent-tokens sent) tk-ix))
            "unsure")
      (sensetion--edit-reinsert-state-call tk-ix sent)))
   (no-sense-function
    (lambda ()
      (interactive)
      (setf (sensetion--tk-anno (elt (sensetion--sent-tokens sent) tk-ix))
            nil)
      (setf (sensetion--tk-status (elt (sensetion--sent-tokens sent) tk-ix))
            "now")
      (sensetion--edit-reinsert-state-call tk-ix sent)))
   (pres-senses (sensetion--tk-anno tk))
   (tk (elt (sensetion--sent-tokens sent) tk-ix))))


(defun sensetion--add-sense (lemma st tk sense ix sent)
  "Called by `sensetion--edit-hydra-maker'. Only used for side-effects."
  (sensetion-is
   (setf (sensetion--tk-lemma (elt (sensetion--sent-tokens sent) ix))
         lemma-str)
   (setf (sensetion--tk-anno (elt (sensetion--sent-tokens sent) ix))
         senses)
   (setf (sensetion--tk-status (elt (sensetion--sent-tokens sent) ix))
         "now")
   where
   (lemma-str (s-join "|" lemmas))
   (lemmas (seq-uniq (cons (sensetion--make-lemma* lemma st) old-lemmas)))
   (old-lemmas (sensetion--tk-lemmas tk))
   (senses (seq-uniq (cons sense (sensetion--tk-anno tk))))))


(defun sensetion--edit-reinsert-state-call (tk-ix sent &optional lemma st options)
  (sensetion--reinsert-sent-at-point sent)
  (sensetion-previous-selected (point))
  (unless (sensetion--tk-annotated? (elt (sensetion--sent-tokens sent) tk-ix))
    (cl-incf (car sensetion--global-status) 1)
    (cl-incf (car sensetion--local-status) 1))
  (when (and lemma st options)
    (sensetion--call-hydra lemma st tk-ix sent options)))

;; ;; ;; ;; edit source sent
(defun sensetion-edit-sent (sent)
  (interactive (list (sensetion--get-sent-at-point)))
  (sensetion--edit-sent
   sent
   (get-buffer (sensetion--make-edit-buffer-name sent))))


(defun sensetion--edit-sent (sent mbuffer)
  (let ((buffer (or mbuffer (generate-new-buffer (sensetion--make-edit-buffer-name sent)))))
    (unless mbuffer
      (with-current-buffer buffer
        (prin1 (sensetion--sent->plist sent) (current-buffer))
        (sensetion--beginning-of-buffer)
        (indent-pp-sexp 1)
        (sensetion--edit-mode)
        (set-buffer-modified-p nil)))
    (pop-to-buffer buffer nil t)))


(defun sensetion--make-edit-buffer-name (sent)
  (format "*%s-sensetion-edit*" (sensetion--sent-id sent)))


(defun sensetion--save-edit (&optional force)
  (when (buffer-modified-p (current-buffer))
    (when (or force (y-or-n-p "Save sentence? "))
      (save-excursion
        (goto-char (point-min))
        (sensetion--save-sent (sensetion--plist->sent (read (thing-at-point 'sexp t)))))
      (set-buffer-modified-p nil)))
  t)


(define-derived-mode sensetion--edit-mode prog-mode "sensetion-edit"
  "sensetion-edit-mode is a major mode for editing sensetion database files."
  (add-hook 'kill-buffer-hook 'sensetion--save-edit nil t)
  (setq-local write-contents-functions (list (lambda () (sensetion--save-edit t)))))


(provide 'sensetion-edit)

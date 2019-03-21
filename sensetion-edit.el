;;; sensetion.el --- -*- lexical-binding: t; -*-
(require 'sensetion-data)


(defalias 'sensetion-edit #'sensetion-annotate)

(defun sensetion-edit-sense (lemma ix pos synset)
  (interactive (list (buffer-local-value 'sensetion--lemma (current-buffer))
                     (or (get-char-property (point) 'sensetion--glob-ix)
                         (sensetion--tk-ix-prop-at-point))
                     (ido-completing-read "Token PoS tag: " '("n" "v" "a" "r")
                                          nil t nil nil)
                     (sensetion--get-synset-at-point)))
  (unless (sensetion--selected? (point))
    (user-error "Token at point not selected for annotation"))
  (unless lemma
    (error "No local sensetion--lemma; please report bug"))
  (sensetion--edit-sense lemma pos ix synset))


(defun sensetion--edit-sense (lemma pos1 ix synset)
  (let* ((st (sensetion--pos->synset-type pos1))
         (senses (cl-loop for k being the hash-keys of sensetion--synset-cache
                          using (hash-values v)
                          when (equal (sensetion--sk-st k) st)
                          collect (cons k v))))
    (unless senses
      (user-error "No senses for lemma %s with pos %s" lemma pos1))
    (sensetion--call-hydra lemma st ix
                  synset senses)))


(defun sensetion--call-hydra (lemma st tk-ix synset options)
  (call-interactively
   (eval (sensetion--edit-hydra-maker lemma st tk-ix synset options))))


(defun sensetion--edit-hydra-maker (lemma st tk-ix synset options)
  "Creates interactive editing hydra on-the-fly."
  (sensetion-is
   `(defhydra hydra-senses (:color blue)
      ""
      ("q" nil nil)
      ("RET" nil nil)
      ("0" ,no-sense-function "No sense in Wordnet" :column "Pick sense:")
      ,@(mapcar
         (lambda (s)
           (cl-destructuring-bind (sk hkey terms gloss) s
             (list hkey
                   `(lambda () (interactive)
                      (atomic-change-group
                        (sensetion--toggle-sense ,lemma
                                        ,tk
                                        ,sk)
                        (sensetion--edit-reinsert-state-call
                         ,tk-ix ,synset ,lemma ,st ',options)))
                   (sense-help-text sk terms gloss)
                   :column "Pick sense:")))
         options))
   :where
   (sense-help-text (sk terms gloss)
                    (concat (sense-chosen-ind sk)
                            (s-word-wrap (- (frame-width) 5)
                                         (concat (s-join "," terms) " | " gloss))))
   (sense-chosen-ind (sk)
                     (if (member sk pres-skeys) "+ " ""))
   (no-sense-function
    (lambda () (interactive)
      (funcall
       (sensetion--edit-function
        (lambda (tk _)
          (setf (sensetion--tk-senses tk) nil)
          (setf (sensetion--tk-tag tk) "man-now")))
       tk-ix synset)))
   (pres-skeys (sensetion--tk-skeys tk))
   (tk (elt (sensetion--synset-tokens synset) tk-ix))))


(defun sensetion--toggle-sense (lemma tk sk)
  "Called by `sensetion--edit-hydra-maker'. Only used for side-effects."
  (let* ((orig (sensetion--tk-senses tk))
         (present? (cl-member sk orig :key #'car :test #'equal))
         (senses (if present?
                     (cl-remove sk orig :key #'car :test #'equal)
                   (cons (cons sk lemma) orig))))
    (if (and present? (null (cdr orig)))
        (warn "Can't remove last sense")
      (setf (sensetion--tk-senses tk)
            senses)
      (setf (sensetion--tk-tag tk)
            "man-now"))))


(defun sensetion--edit-reinsert-state-call (tk-ix synset &optional lemma st options)
  (let ((point (point)))
    (sensetion--reinsert-synset-at-point synset)
    (goto-char point))
  (unless (sensetion--tk-annotated? (elt (sensetion--synset-tokens synset) tk-ix))
    (cl-incf (car sensetion--global-status) 1)
    (cl-incf (car sensetion--local-status) 1))
  (when (and lemma st options)
    (sensetion--call-hydra lemma st tk-ix synset options)))


;; ;; ;; ;; edit source synset
(defun sensetion-edit-synset (synset)
  (interactive (list (sensetion--get-synset-at-point)))
  (sensetion--edit-synset
   synset
   (get-buffer (sensetion--make-edit-buffer-name synset))))


(defun sensetion--edit-synset (synset mbuffer)
  (let ((buffer (or mbuffer (generate-new-buffer (sensetion--make-edit-buffer-name synset)))))
    (unless mbuffer
      (with-current-buffer buffer
        (prin1 (sensetion--synset->plist synset) (current-buffer))
        (sensetion--beginning-of-buffer)
        (indent-pp-sexp 1)
        (sensetion-edit-mode)
        (set-buffer-modified-p nil)))
    (pop-to-buffer buffer nil t)))


(defun sensetion--make-edit-buffer-name (synset)
  (format "*%s-sensetion-edit*" (sensetion--synset-id synset)))


(defun sensetion--save-edit (&optional force)
  (when (buffer-modified-p (current-buffer))
    (when (or force (y-or-n-p "Save synset? "))
      (save-excursion
        (goto-char (point-min))
        (sensetion--save-synset (sensetion--plist->synset (read (thing-at-point 'sexp t)))))
      (set-buffer-modified-p nil)))
  t)


(define-derived-mode sensetion-edit-mode prog-mode "sensetion-edit"
  "sensetion-edit-mode is a major mode for editing sensetion database files."
  (add-hook 'kill-buffer-hook 'sensetion--save-edit nil t)
  (setq-local write-contents-functions (list (lambda () (sensetion--save-edit t)))))


(defun sensetion--edit-function (before-save-fn &optional after-save-fn)
  "Creates function to edit token at point.

Get token and synset at point, call BEFORE-SAVE-FN with them as
arguments, save synset, call AFTER-SAVE-FN with them as
arguments. None of the arguments may move point."
  (lambda (tk-ix synset)
    (interactive
     ;; TODO: ask about which token to annotate?
     (list (or (get-char-property (point) 'sensetion--glob-ix)
               (sensetion--tk-ix-prop-at-point))
           (sensetion--get-synset-at-point)))
    (let* ((point      (point))
           (tk         (elt (sensetion--synset-tokens synset) tk-ix))
           (prev-anno? (sensetion--tk-annotated? tk)))
      (atomic-change-group
        (funcall before-save-fn tk synset)
        (let ((curr-anno? (sensetion--tk-annotated? tk)))
          (when (and (not prev-anno?) curr-anno?)
            (cl-incf (car sensetion--global-status) 1)
            (cl-incf (car sensetion--local-status) 1)))
        (sensetion--reinsert-synset-at-point synset)
        (when after-save-fn
          (funcall after-save-fn tk synset))
        (goto-char point)))))


(defalias 'sensetion-edit-lemma
  (sensetion--edit-function
   (lambda (tk synset)
     (let* ((old-lemma (sensetion--tk-lemma tk))
            (lemma     (read-string "Assign lemma to token: "
                                    (cons old-lemma (1+ (length old-lemma))))))
       (setf (sensetion--tk-lemma tk) lemma)
       (sensetion--remove-lemmas sensetion--index old-lemma synset)
       (sensetion--index-lemmas sensetion--index lemma (sensetion--synset-id synset)))))
  "Edit lemma of token of index TK-IX at point and save modified SYNSET.")


(defalias 'sensetion-edit-unsure
  (sensetion--edit-function
   (lambda (tk _)
     (let ((st (sensetion--tk-tag tk)))
       (when (member st  '("un"))
         (user-error "Can't be unsure about unnanotated token")))
     (setf (sensetion--tk-unsure tk) 0)))
  "Annotate that confidence in the annotation is low.")


(defalias 'sensetion-edit-ignore
  (sensetion--edit-function
   (lambda (tk _)
     (setf (sensetion--tk-tag tk) "ignore")))
  "Annotate that token is to be ignored in annotation.")


(provide 'sensetion-edit)

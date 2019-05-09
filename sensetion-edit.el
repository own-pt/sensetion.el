;;; sensetion.el --- -*- lexical-binding: t; -*-
(require 'ido)
(require 'sensetion-data)


(defalias 'sensetion-edit #'sensetion-annotate)


(defun sensetion--completing-read-pos ()
  (ido-completing-read "Token PoS tag: "
		       '("n" "v" "a" "r")
                       nil t nil nil))


(defun sensetion-edit-sense (lemma ix pos sent)
  (interactive (list (buffer-local-value 'sensetion--lemma (current-buffer))
                     (or (get-char-property (point) 'sensetion--glob-ix)
                         (sensetion--tk-ix-prop-at-point))
                     (sensetion--completing-read-pos)
                     (sensetion--get-sent-at-point)))
  (unless (sensetion--selected? (point))
    (user-error "Token at point not selected for annotation"))
  (unless lemma
    (error "No local sensetion--lemma; please report bug"))
  (sensetion--edit-sense lemma pos ix sent))


(defun sensetion--edit-sense (lemma pos1 ix sent)
  (let* ((st  (sensetion--pos->synset-type pos1))
         (sts (if (member st '("3" "5"))
                  '("3" "5")
                (list st)))
         (senses (alist-get pos1 (gethash lemma sensetion--synset-cache) nil nil #'equal)))
    (unless senses
      (user-error "No senses for lemma %s with pos %s" lemma pos1))
    (sensetion--call-hydra lemma st ix
                  sent senses)))


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
           (cl-destructuring-bind (sk hkey sid terms gloss) s
             (list hkey
                   ;; gets wrapped in (lambda () (interactive)
                   ;; automatically by hydra
                   `(atomic-change-group
                      (sensetion--toggle-sense ,lemma
                                      ,tk
                                      ,sk)
                      (sensetion--edit-reinsert-state-call
                       ,tk-ix ,sent ,lemma ,st ',options))
                   (sense-help-text sk sid terms gloss)
                   :column "Pick sense:")))
         options))
   :where
   (sense-help-text (sk sid terms gloss)
                    (concat (sense-chosen-ind sk)
                            (if sensetion-sense-menu-show-synset-id (concat "(" sid ") ") "")
                            (s-replace "\n" "\n   "
                                       (s-word-wrap (- (frame-width) 5)
                                                    (concat (s-join "," terms) " | " gloss)))))
   (sense-chosen-ind (sk)
                     (if (member sk pres-skeys) "+ " ""))
   (no-sense-function
    `(,(sensetion--edit-function
        (lambda (tk _)
          (setf (sensetion--tk-senses tk) nil)
          (setf (sensetion--tk-tag tk) "man-now")
          t))
      ,tk-ix ,sent))
   (pres-skeys (sensetion--tk-skeys tk))
   (tk (elt (sensetion--sent-tokens sent) tk-ix))))


(defun sensetion--toggle-sense (lemma tk sk)
  "Called by `sensetion--edit-hydra-maker'. Only used for side-effects."
  (let* ((orig (sensetion--tk-senses tk))
         (present? (member sk orig))
         (senses (if present?
                     (cl-remove sk orig)
                   (cons sk orig))))
    (unless (sensetion--tk-annotated? tk)
      (cl-incf (car sensetion--local-status) 1))
    (if (and present? (null (cdr orig)))
        (user-error "Can't remove last sense")
      (setf (sensetion--tk-senses tk)
            senses)
      (setf (sensetion--tk-tag tk)
            "man-now"))))


(defun sensetion--edit-reinsert-state-call (tk-ix sent &optional lemma st options)
  (let ((point (point)))
    (sensetion--reinsert-sent-at-point sent)
    (goto-char point))
  (when (and lemma st options)
    (sensetion--call-hydra lemma st tk-ix sent options)))


;; ;; ;; ;; edit source sent
(defvar-local sensetion--edit-file-annotation-buffer nil "Buffer where file is displayed for annotation")

(defun sensetion-edit-sent (obuffer)
  "Edit data file corresponding to sent at point in the current OBUFFER."
  (interactive (list (buffer-name)))
  (let ((sent (sensetion--get-sent-at-point)))
    (sensetion--edit-sent
     sent
     (get-buffer (sensetion--make-edit-buffer-name sent))
     obuffer)))


(defun sensetion--edit-sent (sent mbuffer obuffer)
  (let ((buffer (or mbuffer (generate-new-buffer (sensetion--make-edit-buffer-name sent)))))
    (unless mbuffer
      (with-current-buffer buffer
        (prin1 (sensetion--sent->alist sent) (current-buffer))
        (sensetion--beginning-of-buffer)
        (indent-pp-sexp 1)
        (sensetion-edit-mode)
        (setq-local sensetion--edit-file-annotation-buffer obuffer)
        (set-buffer-modified-p nil)))
    (pop-to-buffer buffer nil t)))


(defun sensetion--make-edit-buffer-name (sent)
  (format "*%s:sensetion-edit*" (sensetion--sent-id sent)))


(defun sensetion--refresh-sent (sent &optional buffer)
  (catch 'sensetion--exit
    (sensetion--map-buffer-lines
     (lambda (lno line)
       (when-let* ((line-sent (sensetion--get-sent-at-point))
                   (_ (equal (sensetion--sent-id sent) (sensetion--sent-id line-sent))))
         (sensetion--reinsert-sent-at-point sent)
         (throw 'sensetion--exit t)))
     buffer)))


(defun sensetion--save-edit (&optional force)
  (when (buffer-modified-p (current-buffer))
    (when (or force (y-or-n-p "Save sent? "))
      (save-excursion
        (sensetion--beginning-of-buffer)
        (let ((anno-buffer sensetion--edit-file-annotation-buffer)
              (sent (sensetion--alist->sent (read (thing-at-point 'sexp t)))))
	  (atomic-change-group
            (set-buffer-modified-p nil)
            (sensetion--refresh-sent sent anno-buffer))))))
  t)


(define-derived-mode sensetion-edit-mode prog-mode "sensetion-edit"
  "sensetion-edit-mode is a major mode for editing sensetion data files."
  (add-hook 'kill-buffer-hook 'sensetion--save-edit nil t)
  (setq-local write-contents-functions (list (lambda () (sensetion--save-edit t)))))


(defun sensetion--edit-function (before-save-fn &optional after-save-fn)
  "Creates function to edit token at point.

Get token and sent at point, call BEFORE-SAVE-FN with them as
arguments, save sent and call AFTER-SAVE-FN if BEFORE-SAVE-FN
returns non-nil. None of the arguments may move point."
  (lambda (tk-ix sent &optional glob-ix)
    (interactive
     (list (sensetion--tk-ix-prop-at-point)
           (sensetion--get-sent-at-point)
           (get-char-property (point) 'sensetion--glob-ix)))
    (let* ((point      (point))
           (use-glob? (when glob-ix (ido-completing-read "Edit glob or token? "
                                                         '("glob" "token")
                                                         nil t nil nil t)))
           (tk-ix (if (equal use-glob? "token")
                      glob-ix
                    tk-ix))
           (tk         (elt (sensetion--sent-tokens sent) tk-ix))
           (prev-anno? (sensetion--tk-annotated? tk)))
      (atomic-change-group
        (let* ((to-save? (funcall before-save-fn tk sent))
               (curr-anno? (sensetion--tk-annotated? tk)))
          (when (and (not prev-anno?) curr-anno?)
            (cl-incf (car sensetion--local-status) 1))
          (when to-save?
            (sensetion--reinsert-sent-at-point sent)
            (when after-save-fn
              (funcall after-save-fn tk sent))))
        (goto-char point)))))


(defun sensetion--completing-read-lemma (&optional initial-input)
  (completing-read "Lemma to annotate: "
		   sensetion--completion-function
		   nil t initial-input))


(defalias 'sensetion-edit-lemma
  (sensetion--edit-function
   (lambda (tk sent)
     (let* ((lemmas (sensetion--tk-lemmas tk))
	    (lemmas-str (s-join "," lemmas))
	    (new-lemmas
	     (completing-read-multiple
	      "Edit lemma: "
	      sensetion--completion-function nil nil
	      (cons lemmas-str (1+ (length lemmas-str))))))
       (setf (sensetion--tk-lemmas tk) new-lemmas))
     t))
  "Edit lemma of token of index TK-IX at point and save modified SENT.")


(defalias 'sensetion-edit-unsure
  ;; TODO: allow this anywhere?
  (sensetion--edit-function
   (lambda (tk _)
     (let ((st (sensetion--tk-tag tk)))
       (when (member st '("un"))
         (user-error "Can't be unsure about unnanotated token")))
     (cl-callf not (sensetion--tk-unsure tk))
     t))
  "Toggle the confidence in the annotation (t indicates that confidence is low).")


(defalias 'sensetion-edit-ignore
  ;; TODO: allow this anywhere?
  (sensetion--edit-function
   (lambda (tk _)
     (unless (sensetion--tk-annotatable? tk)
       (user-error "Token already ignored"))
     (when (sensetion--tk-annotated? tk)
       (cl-incf (car sensetion--local-status) -1))
     (cl-incf (cdr sensetion--local-status) -1)
     (setf (sensetion--tk-tag tk) "ignore")
     (setf (sensetion--tk-senses tk) nil)
     t))
  "Annotate that token is to be ignored in annotation.")


(provide 'sensetion-edit)

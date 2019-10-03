;;; sensetion.el --- -*- lexical-binding: t; -*-
(require 'ido)
(require 'sensetion-data)


(defun sensetion--completing-read-pos (&optional any)
  (cl-fourth
   (read-multiple-choice "PoS tag: "
			 (append (when any
				   `((,(string-to-char " ") "any" "Any PoS tag")))
				 '((?n "noun" "noun" :n)
				   (?v "verb" "verb" :v)
				   (?a "adj" "adjective" :as)
				   (?r "adv" "adverb" :r))))))


(defun sensetion-edit-sense (ix sent)
  "Annotate sense of token of index IX in SENT's tokens."
  (interactive (list (or (get-char-property (point) 'sensetion--glob-ix)
                         (sensetion--tk-ix-prop-at-point))
                     (sensetion--get-sent-at-point)))
  (unless (sensetion--selected? (point))
    (user-error "Token at point not selected for annotation"))
  (let* ((token (elt (sensetion--sent-tokens sent) ix))
	 (maybe-token-pos (sensetion--tk-senses-pos token)))
    (cl-destructuring-bind (lemma . pos) (sensetion--split-lemma+synset-type
					  (sensetion--pick-lemma-candidate token))
      (when
	  ;; token already has annotation of different pos
	  (and maybe-token-pos pos (not (eq pos maybe-token-pos)))
	(if (y-or-n-p
	     (format "Token has already been annotated with PoS %s. Remove previous annotation?"
		     (sensetion--pos->string maybe-token-pos)))
	    (setf (sensetion--tk-senses token) nil
		  (sensetion--tk-tag token)    "un")
	  (user-error "Giving up on annotation"))
	(message ""))		       ; to clear minibuffer for hydra
      (sensetion--edit-sense lemma (or pos (sensetion--completing-read-pos)) token sent))))


(defun sensetion--pick-lemma-candidate (token)
  "Choose candidate (lemma + synset type) to annotate among the
options in TOKEN."
  (sensetion-is
   (if (cl-rest lemmas)
       (choose-lemma)
     (let ((lemma (cl-first lemmas)))
       (message "Annotating lemma %s" lemma)
       (sleep-for 0.5)
       lemma))
   :where
   (choose-lemma ()
		 (cl-third
		  (read-multiple-choice
		   "Pick lemma+PoS candidate: "
		   (seq-map-indexed #'make-option lemmas))))
   (make-option (lemma+synset-type ix)
		(cl-destructuring-bind (lemma . pos)
		    (sensetion--split-lemma+synset-type lemma+synset-type)
		  (list (+ 49 ix)
			(format "%s%%%s" lemma (sensetion--pos->string pos))
			lemma+synset-type)))
   (lemmas (sensetion--tk-lemmas token))))


(defun sensetion--edit-sense (lemma pos token sent)
  (let ((senses (sensetion--cache-lemma->senses lemma pos sensetion--synset-cache)))
    (unless senses
      (user-error "No senses for lemma %s with pos %s" lemma (sensetion--pos->string pos)))
    (sensetion--call-hydra lemma pos token sent senses)))


(defun sensetion--call-hydra (lemma pos token sent options)
  (call-interactively
   (eval (sensetion--edit-hydra-maker lemma pos token sent options))))


(defun sensetion--format-gloss (definition examples)
  (string-join
   (cons definition
	 (if-let ((n sensetion-sensetion-maximum-examples-to-show))
	     (seq-take examples n)
	   examples))
   " • "))


(defun sensetion--sense-edit-help-text (chosen? lexname terms definition examples)
  (sensetion-is
   (format "%s%s %s"
	   chosen-mark
	   synset?
	   (s-replace "\n" "\n   "
		      (s-word-wrap (- (frame-width) 5)
				   (concat terms-txt
					   " | "
					   (sensetion--format-gloss definition examples)))))
   :where
   (terms-txt (mapconcat #'bold terms ","))
   (synset? (if sensetion-sense-menu-show-lexicographer-filename
		(concat "(" (prop lexname 'italic) ")")
	      ""))
   (chosen-mark (if chosen? "+ " ""))
   (bold (txt)
	 (prop txt 'bold))
   (prop (txt prop)
	 (propertize txt 'face prop))))


(defun sensetion--edit-hydra-maker (lemma pos token sent options)
  "Creates interactive editing hydra on-the-fly."
  (sensetion-is
   `(defhydra hydra-senses (:color blue)
      ""
      ("q" nil nil)
      ("RET" nil nil)
      ("0" ,no-sense-function "No sense in Wordnet" :column ,column)
      ,@(mapcar
         (lambda (s)
           (seq-let (sk hkey lexname terms definition examples) s
             (list hkey
                   ;; gets wrapped in (lambda () (interactive)
                   ;; automatically by hydra
                   `(atomic-change-group
                      (sensetion--toggle-sense ,token ,sk)
                      (sensetion--edit-reinsert-state-call
                       ,token ,sent ,lemma ,pos ',options))
		   (sensetion--sense-edit-help-text (sense-chosen-ind? sk)
					   lexname terms definition examples)
                   :column column)))
         options))
   :where
   (column (format "Pick sense for token %s with PoS %s:" lemma (sensetion--pos->string pos)))
   (sense-chosen-ind? (sk)
		      (member sk pres-skeys))
   (no-sense-function
    `(,(sensetion--edit-function
        (lambda (tk _)
          (setf (sensetion--tk-senses tk) nil)
          (setf (sensetion--tk-tag tk) "man-now")
          t))
      ,token ,sent))
   (pres-skeys (sensetion--tk-skeys token))))


(defun sensetion--toggle-sense (tk sk)
  "Called by `sensetion--edit-hydra-maker'. Only used for side-effects."
  (let* ((orig (sensetion--tk-senses tk))
         (present? (member sk orig))
         (senses (if present?
                     (remove sk orig)
                   (cons sk orig))))
    (unless (sensetion--tk-annotated? tk)
      (cl-incf (car sensetion--local-status) 1))
    (if (and present? (null (cdr orig)))
        (user-error "Can't remove last sense")
      (setf (sensetion--tk-senses tk) senses)
      (setf (sensetion--tk-tag tk) "man-now"))))


(defun sensetion--edit-reinsert-state-call (token sent &optional lemma pos options)
  (let ((point (point)))
    (sensetion--reinsert-sent-at-point sent)
    (goto-char point))
  (when (and lemma pos options)
    (sensetion--call-hydra lemma pos token sent options)))


;; ;; ;; ;; edit source sent
(defvar-local sensetion--edit-file-annotation-buffer
  nil
  "Buffer where file is displayed for annotation")

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
     (lambda (_i _)
       (when-let* ((line-sent (sensetion--get-sent-at-point))
                   (_    (equal (sensetion--sent-id sent) (sensetion--sent-id line-sent))))
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


(defun sensetion--completing-read-lemma (prompt &optional initial-input)
  (let ((input-lemma (completing-read prompt
				  sensetion--lemma-completion-function
				  nil (sensetion--project-restrict-lemmas sensetion-current-project) initial-input)))
    (sensetion--spaces->underlines input-lemma)))


(defun sensetion--hydra-edit-lemma (tk sent &optional lemmas)
  (call-interactively
   (eval
    (let ((lemmas (or lemmas (sensetion--tk-lemmas tk))))
      `(defhydra hydra-lemmas () ""
	 ("q" nil "quit")
	 ("a" ,(lambda (lemma pos)
		 (interactive
		  (list (sensetion--completing-read-lemma "New lemma: ")
			(sensetion--completing-read-pos)))
		 (let ((new-lemmas (cons (format "%s%%%s"
						 lemma
						 (sensetion--pos->synset-type pos))
					 lemmas)))
		   (sensetion--hydra-edit-lemma tk sent new-lemmas)))
	  "add")
	 ("s" ,(lambda ()
		 (interactive)
		 (setf (sensetion--tk-lemmas tk)
		       lemmas)
		 (sensetion--update-sent sent))
	  "save"
	  :exit t)
	 ,@(sensetion--create-hydra-read-lemma lemmas lemmas tk sent 0))))))


(defun sensetion--create-hydra-read-lemma (all-lemmas lemmas tk sent num)
  (when lemmas
    (cons
     (cl-destructuring-bind (lemma . pos) (sensetion--split-lemma+synset-type (cl-first lemmas))
       (list
	(number-to-string num)
	(lambda ()
	  (interactive)
	  (sensetion--hydra-edit-lemma
	   tk sent (sensetion--remove-nth num all-lemmas)))
	(format "%s %s" lemma (sensetion--pos->string pos))
	:column "Remove lemma pos:"))
     (sensetion--create-hydra-read-lemma all-lemmas (cdr lemmas) tk sent (+ 1 num)))))

(defalias 'sensetion-edit-lemma
  (sensetion--edit-function
   (lambda (tk sent)
     (sensetion--hydra-edit-lemma tk sent)))
 "Edit lemma of TK at point.")


(defalias 'sensetion-edit-unsure
  ;; TODO: allow this anywhere?
  (sensetion--edit-function
   (lambda (tk _)
     (let ((tag (sensetion--tk-tag tk)))
       (when (member tag '("un"))
         (user-error "Can't be unsure about unnanotated token")))
     (cl-callf not (sensetion--tk-unsure tk))
     t))
  "Toggle the confidence in the annotation (t indicates that confidence is low).")


(defalias 'sensetion-edit-ignore
  (sensetion--edit-function
   (lambda (tk _)
     (if (sensetion--tk-annotatable? tk)
	 (when (or (not (sensetion--tk-annotated? tk))
		   (y-or-n-p "Delete all annotations and ignore token?"))
	   (when (sensetion--tk-annotated? tk)
	     (cl-incf (car sensetion--local-status) -1))
	   (cl-incf (cdr sensetion--local-status) -1)
	   (setf (sensetion--tk-tag tk) "ignore")
	   (setf (sensetion--tk-senses tk) nil))
       (progn
	 (setf (sensetion--tk-tag tk) "un")
	 (cl-incf (cdr sensetion--local-status))))
       t))
  "Annotate that token is to be ignored in annotation.")


(provide 'sensetion-edit)

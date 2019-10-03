;;; sensetion.el --- -*- lexical-binding: t; -*-
(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'ido)
(require 'f)
(require 'map)
(require 'async)
(require 'hydra)
(require 'sensetion-utils)
(require 'sensetion-data)
(require 'sensetion-colloc)
(require 'sensetion-client)
(require 'sensetion-edit)


;; TODO: benchmark stuff with
;; https://emacs.stackexchange.com/questions/539/how-do-i-measure-performance-of-elisp-code

;;; Customizations
(defgroup sensetion nil
  "Support for annotating word senses."
  :group 'data)


(defcustom sensetion-backend-port
  nil
  "(deprecated) Port used by backend server."
  :group 'sensetion
  :type 'integer)


(defcustom sensetion-sense-menu-show-lexicographer-filename
  nil
  "Show lexicographer file name of synset in sense menu during annotation."
  :group 'sensetion
  :type  'boolean)


(defcustom sensetion-sensetion-maximum-examples-to-show
  nil
  "Maximum number of examples sentences to show in sense annotation menu.

If nil, show all examples."
  :group 'sensetion
  :type '(choice integer (const nil)))


(defcustom sensetion-unnanotated-colour
  "salmon"
  "Colour to display the selected tokens in."
  :group 'sensetion
  :type 'color)


(defcustom sensetion-previously-annotated-colour
  "dark green"
  "Colour to display the tokens which have been previously annotated."
  :group 'sensetion
  :type 'color)


(defcustom sensetion-previously-annotated-unsure-colour
  "light green"
  "Colour to display the tokens which have been previously annotated with low confidence."
  :group 'sensetion
  :type 'color)


(defcustom sensetion-currently-annotated-colour
  "dark blue"
  "Colour to use in displaying tokens annotated in this batch."
  :group 'sensetion
  :type 'color)


(defcustom sensetion-currently-annotated-unsure-colour
  "light blue"
  "Colour to use in displaying tokens annotated in this batch, with low confidence."
  :group 'sensetion
  :type 'color)


(defcustom sensetion-glob-mark-colour
  "brown"
  "Colour to use in mark of tokens to glob."
  :group 'sensetion
  :type 'color)


(defcustom sensetion-mode-line
  '(:eval (sensetion--mode-line-status-text))
  "Modeline customization for `sensetion-mode'."
  :group 'sensetion
  :type 'sexp
  :risky t)


(defcustom sensetion-end-session-hook nil
  "Functions run when annotation buffer is closed."
  :group 'sensetion
  :type 'list
  :risky t)

;;; Vars

(defvar sensetion-project-list nil
  "List of available annotation projects.

Create a project with `sensetion-make-project'.")

(defvar sensetion-current-project nil
  "Current annotation project.

Select an annotation project with `sensetion-select-project'.")

(defvar-local sensetion--lemma
  nil
  "Lemma being annotated in the buffer, if in targeted annotation
mode.")


(defvar-local sensetion--local-status
  nil
  "Local status.

A cons cell where the car is the number of tokens annotated so
far, and the cdr is the number of annotatable tokens.")


(defvar sensetion-log-buffer-name "*sensetion log*"
  "Buffer where sensetion errors are logged.")


(defvar-local sensetion--synset-cache nil)


(defvar sensetion-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "s" #'sensetion-hydra/body)
    (define-key map "<" #'sensetion-previous-selected)
    (define-key map ">" #'sensetion-next-selected)
    (define-key map "/" #'sensetion-edit-sense)
    (define-key map "c" #'sensetion-sequential-annotate-sentence-document)
    (define-key map "r" #'sensetion-refresh)
    (define-key map "u" #'sensetion-unglob)
    (define-key map "l" #'sensetion-edit-lemma)
    (define-key map "m" #'sensetion-toggle-glob-mark)
    (define-key map "g" #'sensetion-glob)
    (define-key map "v" #'sensetion-toggle-scripts)
    (define-key map "." #'sensetion-edit-sent)
    (define-key map "?" #'sensetion-edit-unsure)
    (define-key map "i" #'sensetion-edit-ignore)
    (define-key map [C-down] #'sensetion-move-line-down)
    (define-key map [C-up] #'sensetion-move-line-up)
    map)
  "Keymap for `sensetion-mode'.")


(define-derived-mode sensetion-mode special-mode "sensetion"
  "sensetion-mode is a major mode for annotating senses."
  :group 'sensetion
  (setq-local sentence-end ".$") ;; to be able to use M-a and M-e to jump
  (visual-line-mode 1)
  (setq-local buffer-invisibility-spec nil)
  (setq-local minor-mode-alist nil)
  ;; show one newline as two newlines; having one sentence per line is
  ;; convenient, but having no separation between sentences is
  ;; confusing
  (aset (or buffer-display-table
            (setq buffer-display-table (make-display-table)))
        ?\n [?\n?\n])
  (mapc (lambda (f) (add-hook 'kill-buffer-hook f nil t)) sensetion-end-session-hook)
  (setq-local sensetion--synset-cache (make-hash-table :size 200 :test 'equal))
  ;; customize mode line
  (setq-local mode-name sensetion-mode-line))


(defun sensetion--mode-line-status-text ()
  (concat "sensetion"
          (if sensetion--local-status
              (cl-destructuring-bind (done . total) sensetion--local-status
                (format ":%.0f/%.0f" done total))
            "")))


(defvar sensetion--lemma-completion-function
  (completion-table-dynamic #'sensetion-client-prefix-lemma))

(defvar sensetion--document-id-completion-function
  (completion-table-dynamic #'sensetion-client-prefix-document-id))

(defconst sensetion--pos->synset-type-table
  #s(hash-table size 5 test eq rehash-size 1.5 rehash-threshold 0.8125
                purecopy t data
		(:n ("1") :v ("2") :as ("3" "5") :r ("4"))))

(defconst sensetion--pos->string-table
  #s(hash-table size 5 test eq rehash-size 1.5 rehash-threshold 0.8125
                purecopy t data
                (:n ("n") :v ("v") :as ("a" "s") :r ("r"))))


(defconst sensetion--synset-type->pos-table
  #s(hash-table size 5 test equal rehash-size 1.5 rehash-threshold 0.8125
                purecopy t data
                ("1" :n "2" :v "3" :as "4" :r "5" :as)))


;;;###autoload
(defalias 'sensetion #'sensetion-annotate)


(defun sensetion-annotate (_project annotation-function)
  "Start annotation session of PROJECT using ANNOTATION-FUNCTION."
  (interactive (list (or sensetion-current-project (sensetion-select-project))
		     (sensetion--pick-annotation-function)))
  (call-interactively annotation-function))


(defun sensetion--pick-annotation-function ()
  (cl-fourth
   (read-multiple-choice
    "Annotation mode: "
    `((?t "targeted mode"
	  "Annotate sentences containing a given target lemma/PoS."
	  ,#'sensetion-annotate-target)
      (?s "sequential mode"
	  "Annotate sentences from a given document."
	  ,#'sensetion-sequential-annotate-doc)))))


(defun sensetion-select-project ()
  "Select a project from `sensetion-project-list'."
  (interactive)
  (setf sensetion-current-project
	(pcase sensetion-project-list
	  ('()
	   (user-error "There is no project available.  Please define a project"))
	  (`(,project)
	   project)
	  (_
	   (cl-find
	    (ido-completing-read "Select project: "
				 (mapcar #'sensetion--project-name sensetion-project-list) nil t)
	    sensetion-project-list :key #'sensetion--project-name :test #'equal)))))


(defun sensetion-sequential-annotate-doc (document-id)
  "Annotate document specified by DOCUMENT-ID in sequential mode."
  (interactive (list (completing-read "Document to annotate: "
				      sensetion--document-id-completion-function
				      nil 'yes)))
  (let ((matches (sensetion--client-get-sorted-doc-sents document-id)))
    (sensetion--annotate matches document-id)))


(defun sensetion-annotate-target (lemma &optional pos)
  "Annotate target LEMMA and POS.

Create targeted annotation buffer, where several sentences
containing tokens with LEMMA and optionally POS are displayed for
annotation."
  (interactive
   (list (sensetion--completing-read-lemma "Lemma to annotate: ")
         (sensetion--completing-read-pos t)))
  (unless lemma (user-error "Must provide lemma"))
  (message "Preparing annotation buffer")
  (sensetion-is
   (sensetion--annotate matches id lemma
	       (lambda ()
		 (setq-local sensetion--lemma lemma)
		 (setq-local sensetion--synset-cache
			     (sensetion--wordnet-lookup-lemma lemma sensetion--synset-cache))))
   :where
   (id (if pos (format "%s@%s" lemma (sensetion--pos->string pos)) lemma))
   (matches (sensetion--client-get-sorted-sents lemma pos))))


(defun sensetion--annotate (matches id &optional target buffer-setup-fn)
  (let ((result-buffer (generate-new-buffer
			(sensetion--create-buffer-name id))))
    (unless matches
      (error "No matches for %s" id))
    (with-current-buffer result-buffer
      (sensetion-mode)
      (sensetion--with-inhibiting-read-only
       (when buffer-setup-fn (funcall buffer-setup-fn))
       (setq sensetion--local-status (sensetion--make-collocations matches target sensetion--synset-cache)))
      (sensetion--beginning-of-buffer))
    (pop-to-buffer result-buffer)))


(defun sensetion--create-buffer-name (id)
  (format "*%s@%s*" (sensetion--project-output-buffer-name sensetion-current-project) id))


(defun sensetion--make-collocations (matches &optional target synset-cache)
  "Insert MATCHES at current buffer, return status.

TARGET is the target lemma, and SYNSET-CACHE caches the synset
data available."
  (sensetion-is
   (cl-mapc #'go matches)
   (cons done total)
   :where
   (go (sent)
       (cl-destructuring-bind (tokens-line (sent-done . sent-total))
           (colloc sent)
         (insert
	  tokens-line
	  ;; no need to add it all the time
          (propertize "\n" 'sensetion--sent-id (sensetion--sent-id sent)))
         (cl-incf done sent-done)
         (cl-incf total sent-total)))
   (colloc (sent)
	   (sensetion--sent-colloc sent target senses))
   (senses (when target (sensetion--cache-lemma->senses target nil synset-cache)))
   (total   0)
   (done    0)))


(defun sensetion-sequential-annotate-sentence-document ()
  "In targeted mode, open a buffer of sequential annotation.

Point at the new buffer is at corresponding sentence of the other
buffer.  Can be used to switch to sequential annotation or to see
the context of a sentence."
  (interactive)
  (unless sensetion--lemma
    (user-error "You are already in sequential mode"))
  (let* ((sentence       (sensetion--get-sent-at-point))
	 (document-id    (sensetion--sent-doc-id sentence))
	 (sentence-index (sensetion--sent-sent-id sentence)))
    ;; this implementation feels like an abstraction leak...
    (sensetion-sequential-annotate-doc document-id)
    (forward-line sentence-index)
    (recenter)
    (momentary-string-display (propertize "----> " 'face '(bold (:foreground "black")))
			      (point))))


(defun sensetion--get-sent-at-point ()
  (let ((sent-id (sensetion--get-text-property-eol 'sensetion--sent-id)))
    (unless sent-id (user-error "No sentence at point"))
    (sensetion--client-id->sent sent-id)))


(defun sensetion--get-token-at-point ()
  ;; for debugging
  (when-let ((token-index (sensetion--tk-ix-prop-at-point))
	     (sent        (sensetion--get-sent-at-point))
	     (tokens      (sensetion--sent-tokens sent)))
    (nth token-index tokens)))


(defun sensetion--update-sent (sent)
  (sensetion--client-update-modified-sent sent))


(defun sensetion--tk-glob? (tk)
  (pcase (sensetion--tk-kind tk)
    (`("glob" ,k)
     k)))


;; TODO: unglobbing (or any kind of editing that calls sent-colloc)
;; turns just annotated tokens into previously annotated tokens
(defun sensetion-unglob (ix sent)
  "Unglob token of index IX in SENT.

When called interactively, unglobs the token at point.

The token must be part of a collocation.  The sentence is
automatically reinserted in the buffer.

Unglobbing means making all tokens in the collocation normal
tokens, and removing the glob token corresponding to the
collocation."
  (interactive (list (sensetion--tk-ix-prop-at-point)
                     (sensetion--get-sent-at-point)))
  (sensetion-is
   (unless ck
     (user-error "Token is not part of a collocation"))
   (when (cdr ckeys)
     (user-error "Please select token which is part of only one collocation"))
   (sensetion--reinsert-sent-at-point (sensetion--unglob ck sent))
   :where
   ;; TODO: select which colloc to undo?
   (ck (cl-first ckeys))
   (ckeys (sensetion--tk-coll-keys tk))
   (tk (elt (sensetion--sent-tokens sent) ix))))


(defun sensetion--unglob (coll-key sent)
  (pcase sent
    ((cl-struct sensetion--sent doc-id sent-id meta tokens text)
     (sensetion--make-sent
      :doc-id doc-id
      :sent-id sent-id
      :meta meta
      :text text
      :tokens
      (cl-loop
       for tk in tokens
       ;; don't collect glob to be removed
       unless (when (equal coll-key (sensetion--tk-glob? tk))
                ;; handle status update when glob was tagged
		(when (sensetion--to-annotate? tk)
                  (cl-incf (cdr sensetion--local-status) -1))
                (when (sensetion--tk-annotated? tk)
		  (cl-incf (car sensetion--local-status) -1))
                t)
       collect (let ((tk-keys (sensetion--tk-coll-keys tk)))
                 (cond
                  ((equal (list coll-key) tk-keys)
                   ;; this token only part of one colloc
                   (setf (sensetion--tk-kind tk) (list "wf")))
                  ((member coll-key tk-keys)
                   ;; this token part of more than one colloc
                   (setf (sensetion--tk-kind tk)
                         (cons "cf" (remove coll-key tk-keys)))))
                 tk))))))


(defun sensetion--tk-coll-keys (tk)
  (pcase (sensetion--tk-kind tk)
    (`("cf" . ,ks) ks)))


(defun sensetion--mark-glob (beg end ix marked)
  "Mark token to be globbed with the `sensetion-glob' command.

Token is the one between BEG and END, corresponding to index IX."
  (sensetion--with-inhibiting-read-only
   (add-face-text-property beg end '(:underline t))
   (sensetion--put-text-property-eol 'sensetion--to-glob (cons ix marked))))


(defun sensetion--tks-to-glob-prop ()
  (sensetion--get-text-property-eol 'sensetion--to-glob))


(defun sensetion--unmark-glob (beg end ix marked)
  (sensetion--with-inhibiting-read-only
   (sensetion--put-text-property-eol 'sensetion--to-glob (cl-remove ix marked))
   (add-face-text-property beg end '(:underline nil))))


(defun sensetion-toggle-glob-mark (beg end)
  "Mark or unmark token between BEG and END.

To glob all marked tokens, use the `sensetion-glob' command."
  (interactive (sensetion--tk-points))
  (let* ((ix (sensetion--tk-ix-prop-at-point beg))
         (marked (sensetion--tks-to-glob-prop))
         (marked? (cl-member ix marked)))
    (if marked?
        (sensetion--unmark-glob beg end ix marked)
      (sensetion--mark-glob beg end ix marked))))


(defun sensetion-glob (lemma pos ixs-to-glob sent)
  "Glob all tokens in SENT which are marked.

Marked tokens are those whose indices are in IXS-TO-GLOB,
assigning the resulting glob token LEMMA and POS.

You can mark/unmark tokens with `sensetion-toggle-glob-mark'."
  (interactive (list
                (sensetion--completing-read-lemma "Lemma: ")
		(sensetion--completing-read-pos)
		(reverse (sensetion--get-text-property-eol 'sensetion--to-glob))
		(sensetion--get-sent-at-point)))
  (let ((point (point))
	(globbed-sent (sensetion--glob lemma pos ixs-to-glob sent)))
    (sensetion--reinsert-sent-at-point globbed-sent)
    (sensetion--with-inhibiting-read-only
     (sensetion--put-text-property-eol 'sensetion--to-glob nil))
    (goto-char point)))


(defun sensetion--glob (lemma pos ixs-to-glob sent)
  (sensetion-is
   globbed-sent
   :where
   (globbed-sent
    (pcase sent
      ((cl-struct sensetion--sent doc-id sent-id meta tokens text)
       (sensetion--make-sent :doc-id doc-id :sent-id sent-id :meta meta :text text
		    :tokens (cl-loop
			     for tk in tokens
			     for i from 0
			     append (cond
				     ((equal i (cl-first ixs-to-glob))
				      ;; insert glob before first token in the
				      ;; collocation
				      (list new-glob (glob-tk tk new-k)))
				     ((cl-member i ixs-to-glob)
				      (list (glob-tk tk new-k)))
				     (t
				      (list tk))))))))
   (glob-tk (tk key)
            (let ((cks (sensetion--tk-coll-keys tk)))
	      (pcase tk
		((cl-struct sensetion--tk form lemmas tag senses glob unsure meta)
		 ;; TODO: delete senses and make status "un"
		 (sensetion--make-tk :kind (cl-list* "cf" key cks)
			    :form form :lemmas lemmas :tag tag :senses senses
			    :glob glob :unsure unsure :meta meta)))))
   (new-glob (sensetion--make-tk :lemmas (list (sensetion--make-lemma* lemma st)) :tag "un"
                        :kind `("glob" ,new-k) :glob "man"))
   (st (sensetion--pos->synset-type pos))
   (new-k (char-to-string (1+ max-k)))
   (max-k (max-key sent))
   (max-key (sent)
            (apply #'max
                   (cons (string-to-char "`")
                         ;; in case there are no other globs, use "`"
                         ;; (which results in "a" being the key),
                         ;; else get the maximum one
                         (seq-mapcat (lambda (tk)
                                       (when-let ((keys (sensetion--tk-coll-keys tk)))
                                         (mapcar #'string-to-char keys)))
                                     (sensetion--sent-tokens sent)))))))


(cl-defun sensetion--reinsert-sent-at-point (sent &optional (update t))
  "Save SENT, delete current line (where previous version of sent
was linearized), and reinsert SENT."
  (sensetion--with-inhibiting-read-only
   (atomic-change-group
     (delete-region (line-beginning-position) (line-end-position))
     (seq-let (line _)
	 (sensetion--sent-colloc sent sensetion--lemma
			(when sensetion--lemma (sensetion--cache-lemma->senses sensetion--lemma nil sensetion--synset-cache)))
       (insert line)))
   (when update
     (sensetion--update-sent sent))))

;; TODO: when annotating glob, check if token is part of more than one
;; colloc
(cl-defun sensetion--tk-has-lemma? (tk &optional (lemma sensetion--lemma))
  (sensetion-is
   (cl-member lemma
              lemmas
              :test #'equal
              :key #'sensetion--lemma*->lemma)
   :where
   (lemmas (sensetion--tk-lemmas tk))))


(cl-defun sensetion--to-annotate? (tk &optional (lemma sensetion--lemma))
  (and (sensetion--tk-annotatable? tk)
       (sensetion--tk-has-lemma? tk lemma)
       (null (sensetion--tk-coll-keys tk))))


(defun sensetion--tk-senses-pos (tk)
  "Get PoS of synsets assigned to TK.

If there is more than one synset and they have different pos1,
return nil."
  (when-let* ((sks   (sensetion--tk-skeys tk))
              (pos   (sensetion--sensekey-pos (cl-first sks)))
              (same? (seq-every-p (lambda (sk) (eq pos (sensetion--sensekey-pos sk)))
                                  (cl-rest sks))))
    pos))


(defun sensetion-previous-selected (point)
  (interactive (list (point)))
  (let ((selected? (sensetion--selected? point)))
    (goto-char
     (previous-single-property-change point 'sensetion--selected nil (point-min)))
    (when selected? (sensetion-previous-selected (point)))))


(defun sensetion-next-selected (point)
  (interactive (list (point)))
  (let ((selected? (sensetion--selected? point)))
    (goto-char
     (next-single-property-change point 'sensetion--selected nil (point-max)))
    (when selected? (sensetion-next-selected (point)))))


(defun sensetion--selected? (point)
  (get-text-property point 'sensetion--selected))


(cl-defun sensetion--tk-ix-prop-at-point (&optional (point (point)))
  (sensetion-is
   (or ix
       (user-error "No token at point"))
   :where
   (ix (get-char-property point 'sensetion--tk-ix))))


(defun sensetion--tk-annotatable? (tk)
  ;; TODO: make status keywords
  (let ((status (sensetion--tk-tag tk)))
    (when (member
           status
           '("man" "man-now" "un" "auto"))
      status)))


(defsubst sensetion--pos->synset-type (pos)
  (cl-first (gethash pos sensetion--pos->synset-type-table)))

(defsubst sensetion--pos->synset-types (pos)
  (gethash pos sensetion--pos->synset-type-table))

(defsubst sensetion--pos->string (pos)
  (cl-first (gethash pos sensetion--pos->string-table)))

(defsubst sensetion--pos->strings (pos)
  (gethash pos sensetion--pos->string-table))

(defsubst sensetion--synset-type->pos (st)
  (gethash st sensetion--synset-type->pos-table))


(defun sensetion--wordnet-lookup-lemma (lemma &optional options)
  ;; (hash-table ,lemma ((,pos (,sense-key ,hydra-index ,synset-id ,terms ,gloss) ...) ...)
  (let ((poses '(:n :v :as :r))
	(options (or options (make-hash-table :test 'equal :size 200))))
    (setf (gethash lemma options)
	  (mapcar
	   (lambda (pos)
	     (cons pos (sensetion--wordnet-lookup-lemma-pos lemma pos)))
	   poses))
    options))


(defun sensetion--wordnet-lookup-lemma-pos (lemma pos)
  "Return list of lists where each element list is composed by a
sense key, the sense key/index shown by the edit hydra, the
synset's lexicographer file, the terms defined by that synset,
and the synset's gloss."
  (sensetion-is
   (mapcar #'go synsets)
   :where
   (go (synset)
       (pcase synset
	 ((cl-struct sensetion--synset lexname terms def exs)
	  (prog1
	   (list (lemma-sk lemma synset)
		 (ix->hydra-key counter)
		 lexname
		 terms
		 def
		 exs)
	 (cl-incf counter)))))
   (ix->hydra-key (ix)
                  (format "%s"
                          (if (< ix 9)
                              ;; handling more than 10 senses: will
                              ;; list from 1 to 9, then alphabetic
                              ;; characters, which start at 97 ('a')
                              ;; reserved 0 for no sense
                              (1+ ix)
                            (char-to-string (+ ix 88)))))
   (counter 0)
   (lemma-sk (lemma synset)
             (or
              (cl-first (cl-find lemma (cl-mapcar #'cons (sensetion--synset-keys synset) (sensetion--synset-terms synset))
			    :test #'equal :key #'cl-rest))
              (error "No matching sensekey for lemma %s in synset %s"
                     lemma (sensetion--synset-id synset))))
   (synsets  (sensetion--client-lemma->sorted-synsets lemma pos))))


(defun sensetion--cache-lemma->senses (lemma &optional pos synset-cache)
  (unless synset-cache (sensetion--wordnet-lookup-lemma lemma synset-cache))
  (let ((senses-by-pos (gethash lemma synset-cache)))
    (if senses-by-pos
	(if pos
	    (alist-get pos senses-by-pos)
	  (seq-mapcat #'cl-rest senses-by-pos))
      (progn
	(sensetion--wordnet-lookup-lemma lemma synset-cache)
	(sensetion--cache-lemma->senses lemma pos synset-cache)))))


(defun sensetion--tk-annotated? (tk)
  (member (sensetion--tk-tag tk)
          '("man" "auto" "man-now")))


;; TODO: refactor prop names as variables
(cl-defun sensetion--tk-points (&optional (point (point)))
  (cond
   ((get-text-property point 'sensetion--tk-ix))	; point at tk
   ((get-char-property point 'sensetion--tk-ix)	; point just before tk (note
					; that get-text's and
					; get-char's behaviours differ
    (cl-incf point))
   (t (user-error "No token at point")))
  (let ((end (next-single-property-change point 'sensetion--tk-ix
					  nil (line-end-position))))
    (list (previous-single-property-change end 'sensetion--tk-ix
                                           nil (line-beginning-position))
          end)))


(defun sensetion-move-line-up ()
  "Move up the current line."
  (interactive)
  (sensetion--with-inhibiting-read-only
   (transpose-lines 1)
   (forward-line -2)))


(defun sensetion-move-line-down ()
  "Move down the current line."
  (interactive)
  (sensetion--with-inhibiting-read-only
   (forward-line 1)
   (transpose-lines 1)
   (forward-line -1)))


(defun sensetion-toggle-scripts ()
  "Toggle display of super/subscripts in annotation buffer."
  (interactive)
  (if (memq 'sensetion--scripts buffer-invisibility-spec)
      (remove-from-invisibility-spec 'sensetion--scripts)
    (add-to-invisibility-spec 'sensetion--scripts)))

(defun sensetion-refresh ()
  "Update annotation buffer."
  (interactive)
  (sensetion--map-buffer-lines
   (lambda (_ __)
     (sensetion--reinsert-sent-at-point (sensetion--get-sent-at-point) nil))))

(defhydra sensetion-hydra (:color blue)
  ("q" nil nil)
  ("s" nil nil)
  ("RET" nil nil)
  ("c" sensetion-sequential-annotate-sentence-document "Show context of sentence at point" :column "Navigation")
  ("r" sensetion-refresh "Refresh current buffer" :column "Navigation")
  ("l" sensetion-edit-lemma "Edit token lemma" :column "Edit")
  ("/" sensetion-edit-sense "Edit token senses" :column "Edit")
  ("i" sensetion-edit-ignore "Ignore token" :column "Edit")
  ("?" sensetion-edit-unsure "Mark annotation as unsure" :column "Edit")
  ("." sensetion-edit-sent "Edit data file" :column "Edit")
  ("m" sensetion-toggle-glob-mark "(Un)Mark token for globbing" :column "Globbing")
  ("g" sensetion-glob "Glob marked tokens" :column "Globbing")
  ("u" sensetion-unglob "Unglob token's collocation" :column "Globbing")
  ("<left>" sensetion-previous-selected "Go to previous selected token" :column "Navigation" :color pink)
  ("<right>" sensetion-next-selected "Go to next selected token" :column "Navigation" :color pink)
  ("<up>" sensetion-move-line-up "Move sentence up" :column "Navigation" :color pink)
  ("<down>" sensetion-move-line-down "Move sentence down" :column "Navigation" :color pink))


(provide 'sensetion)

;;; sensetion.el ends here

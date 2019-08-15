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


(defcustom sensetion-output-buffer-name
  "sensetion"
  "Buffer name where sensetion results are displayed."
  :group 'sensetion
  :type 'string)


(defcustom sensetion-elasticsearch-path
  nil
  "Path to elasticsearch executable."
  :group 'sensetion
  :type '(choice file (const nil)))


(defcustom sensetion-backend-url
  "http://localhost"
  "URL to backend server."
  :group 'sensetion
  :type 'url)


(defcustom sensetion-backend-port
  9200
  "Port used by backend server."
  :group 'sensetion
  :type 'integer)


(defcustom sensetion-sense-menu-show-synset-id
  nil
  "Show synset id in sense menu during annotation."
  :group 'sensetion
  :type  'boolean)


(defcustom sensetion-unnanotated-colour
  "salmon"
  "Colour to display the selected tokens in."
  :group 'sensetion
  :type 'color)


(defcustom sensetion-previously-annotated-colour
  "dark green"
  "Colour to display the tokens which have been previously
annotated."
  :group 'sensetion
  :type 'color)


(defcustom sensetion-previously-annotated-unsure-colour
  "light green"
  "Colour to display the tokens which have been previously
annotated with low confidence."
  :group 'sensetion
  :type 'color)


(defcustom sensetion-currently-annotated-colour
  "dark blue"
  "Colour to use in displaying tokens annotated in this batch."
  :group 'sensetion
  :type 'color)


(defcustom sensetion-currently-annotated-unsure-colour
  "light blue"
  "Colour to use in displaying tokens annotated in this batch,
with low confidence."
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


(defcustom sensetion-restrict-lemmas t
  "When `t' restrict the user to add only lemmas that is part of
wordnet to a token or a glob."
  :group 'sensetion
  :type 'boolean)


(defcustom sensetion-identify-sentence t
  "When t shows a tuple before each sentence in target mode with
  the doc-id and sent-id."
  :group 'sensetion
  :type 'boolean)

;;; Vars


(defvar-local sensetion--lemma
  nil
  "Lemma being annotated in the buffer.")


(defvar-local sensetion--local-status
  nil
  "Local status.

A cons cell where the car is the number of tokens annotated so
far, and the cdr is the number of annotatable tokens.")


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
  (completion-table-dynamic #'sensetion-es-prefix-lemma))

(defvar sensetion--document-id-completion-function
  (completion-table-dynamic #'sensetion-es-prefix-document-id))

;;;###autoload
(defalias 'sensetion #'sensetion-annotate)

(defun sensetion-annotate (annotation-function)
  (interactive (list (sensetion--pick-annotation-function)))
  (call-interactively annotation-function))

(defun sensetion--pick-annotation-function ()
  (cl-fourth
   (read-multiple-choice
    "Annotation mode: "
    `((?t "targeted mode" "Annotate sentences containing a given target lemma/PoS." ,#'sensetion-annotate-target)
      (?s "sequential mode" "Annotate sentences from a given document." ,#'sensetion-sequential-annotate-doc)))))


(defun sensetion-sequential-annotate-doc (document-id)
  (interactive (list (completing-read "Document to annotate: "
				      sensetion--document-id-completion-function
				      nil 'yes)))
  (let ((matches (cl-sort (sensetion--es-get-doc-sents document-id)
			  '< :key 'sensetion--sent-sent-id)))
    (sensetion--annotate matches document-id)))


(defun sensetion-sequential-annotate-text (text)
  (interactive "sDocuments matching: ")
  (let ((matches (sensetion--es-text->sents text)))
    (sensetion--annotate matches text)))


(defun sensetion-annotate-target (lemma &optional pos)
  "Create targeted annotation buffer, where several sentences
containing tokens with LEMMA and optionally POS are displayed for
annotation."
  (interactive
   (list (sensetion--completing-read-lemma "Lemma to annotate: ")
         (ido-completing-read "PoS tag? " '("a" "r" "v" "n" "any") nil t nil nil "any")))
  (unless lemma (user-error "Must provide lemma"))
  (sensetion-is
   (sensetion--annotate matches id lemma
	       (lambda ()
		 (setq-local sensetion--lemma lemma)
		 (setq-local sensetion--synset-cache
			     (sensetion--wordnet-lookup-lemma lemma sensetion--synset-cache))))
   :where
   (id (if pos (format "%s@%s" lemma pos) lemma))
   (matches (cl-sort
	     (sensetion--es-get-sents lemma pos)
	     (lambda (x y)
	       (if (equal (sensetion--sent-doc-id x)
			  (sensetion--sent-doc-id y))
		   (< (sensetion--sent-sent-id x)
		      (sensetion--sent-sent-id y))
		 (string-lessp (sensetion--sent-doc-id x)
			       (sensetion--sent-doc-id y))))))
   (pos (unless (equal pos "any") pos))))


(defun sensetion--annotate (matches id &optional target buffer-setup-fn)
  (let ((result-buffer (generate-new-buffer
			(sensetion--create-buffer-name id))))
    (unless matches
      (error "No matches for %s" id))
    (with-current-buffer result-buffer
      (sensetion-mode)
      (with-inhibiting-read-only
       (when buffer-setup-fn (funcall buffer-setup-fn))
       (setq sensetion--local-status (sensetion--make-collocations matches target sensetion--synset-cache)))
      (sensetion--beginning-of-buffer))
    (pop-to-buffer result-buffer)))


(defun sensetion--create-buffer-name (id)
  (format "*%s@%s*" sensetion-output-buffer-name id))


(defun sensetion--make-collocations (matches &optional target synset-cache)
  "Insert MATCHES at current buffer, return status."
  (sensetion-is
   (cl-mapc #'go matches)
   (cons done total)
   :where
   (go (sent)
       (seq-let (tokens-line status)
           (colloc sent)
         (insert
	  tokens-line
	  ;; no need to add it all the time
          (propertize "\n" 'sensetion--sent-id (sensetion--sent-id sent)))
         (cl-incf done (car status))
         (cl-incf total (cdr status))))
   (colloc (sent)
	   (sensetion--sent-colloc sent target senses))
   (senses (when target (sensetion--cache-lemma->senses target nil synset-cache)))
   (total   0)
   (done    0)))


(defun sensetion-sequential-annotate-sentence-document ()
  (interactive)
  (if sensetion--lemma
    (let* ((sentence (sensetion--get-sent-at-point))
	   (document-id (sensetion--sent-doc-id sentence))
	   (sentence-index (sensetion--sent-sent-id sentence)))
      ;; this implementation feels like an abstraction leak...
      (sensetion-sequential-annotate-doc document-id)
      (forward-line sentence-index)
      (recenter)
      (momentary-string-display (propertize "----> " 'face '(bold (:foreground "black")))
				(point)))
    (message "You are already in the context!")))


(defun sensetion--get-sent-at-point ()
  (let ((sent-id (sensetion--get-text-property-eol 'sensetion--sent-id)))
    (unless sent-id (user-error "No sentence at point"))
    (sensetion--alist->sent (sensetion--es-id->sent sent-id))))


(defun sensetion--update-sent (sent)
  (sensetion--es-update-modified-sent sent))


(defun sensetion--tk-glob? (tk)
  (pcase (sensetion--tk-kind tk)
    (`("glob" ,k)
     k)))


;; TODO: unglobbing (or any kind of editing that calls sent-colloc)
;; turns just annotated tokens into previously annotated tokens
(defun sensetion-unglob (ix sent)
  "If token of index IX in SENT at point is part of collocation,
unglob it and reinsert the sentence in the buffer.

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
  "Marks token to be globbed with the `sensetion-glob' command."
  (with-inhibiting-read-only
   (add-face-text-property beg end '(:underline t))
   (sensetion--put-text-property-eol 'sensetion--to-glob (cons ix marked))))


(defun sensetion--tks-to-glob-prop ()
  (get-text-property (line-end-position) 'sensetion--to-glob))


(defun sensetion--unmark-glob (beg end ix marked)
  (with-inhibiting-read-only
   (sensetion--put-text-property-eol 'sensetion--to-glob (cl-remove ix marked))
   (add-face-text-property beg end '(:underline nil))))


(defun sensetion-toggle-glob-mark (beg end)
  "Mark or unmark token to be globbed with the `sensetion-glob'
command."
  (interactive (sensetion--tk-points))
  (let* ((ix (sensetion--tk-ix-prop-at-point beg))
         (marked (sensetion--tks-to-glob-prop))
         (marked? (cl-member ix marked)))
    (if marked?
        (sensetion--unmark-glob beg end ix marked)
      (sensetion--mark-glob beg end ix marked))))


(defun sensetion-glob (lemma pos ixs-to-glob sent)
  "Glob all tokens in SENT whose indices are in IXS-TO-GLOB,
assigning the resulting glob token LEMMA and POS.

You can mark/unmark tokens with `sensetion-toggle-glob-mark'."
  (interactive (list
                (sensetion--completing-read-lemma "Lemma: ")
		(sensetion--completing-read-pos)
		(reverse (sensetion--get-text-property-eol 'sensetion--to-glob))
		(sensetion--get-sent-at-point)))
  (let ((globbed-sent (sensetion--glob lemma pos ixs-to-glob sent)))
    (sensetion--reinsert-sent-at-point globbed-sent)
    (with-inhibiting-read-only
     (sensetion--put-text-property-eol 'sensetion--to-glob nil))))


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
			    :form form :lemmas lemmas :tag tag :senses senses :glob glob :unsure unsure :meta meta)))))
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
  (with-inhibiting-read-only
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
  "Get pos1 of synsets assigned to TK. If there is more than one
synset and they have different pos1, return nil."
  (when-let* ((sks   (sensetion--tk-skeys tk))
              (st    (sensetion--sensekey-pos (cl-first sks)))
              (sts    (if (member st '("3" "5"))
                          '("3" "5")
                        (list st)))
              (same? (seq-every-p (lambda (sk) (member (sensetion--sensekey-pos sk) sts))
                                  (cl-rest sks))))
    (sensetion--synset-type->pos st)))


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


(defun sensetion--pos->synset-type (pos)
  (gethash
   pos
   #s(hash-table size 5 test equal rehash-size 1.5 rehash-threshold 0.8125
                 purecopy t data
                 ("n" "1" "v" "2" "a" "3" "r" "4"))))


(defun sensetion--synset-type->pos (st)
  (gethash
   st
   #s(hash-table size 5 test equal rehash-size 1.5 rehash-threshold 0.8125
                 purecopy t data
                 ("1" "n" "2" "v" "3" "a" "4" "r"))))


(defun sensetion--wordnet-lookup-lemma (lemma &optional options)
  "(hash-table ,lemma ((,pos (,sense-key ,hydra-index ,synset-id ,terms ,gloss) ...) ...)"
  (let ((poses '("n" "v" "r" "a"))
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
synset id, the terms defined by that synset, and the synset's
gloss."
  (sensetion-is
   (mapcar #'go synsets)
   :where
   (go (synset)
       (prog1
	   (list (lemma-sk lemma synset)
		 (ix->hydra-key counter)
		 (sensetion--synset-id synset)
		 (sensetion--synset-terms synset)
		 (sensetion--synset-gloss synset))
	 (cl-incf counter)))
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
              (car (cl-find lemma (cl-mapcar #'cons (sensetion--synset-keys synset) (sensetion--synset-terms synset))
			    :test #'equal :key #'cdr))
              (error "No matching sensekey for lemma %s in synset %s-%s"
                     lemma (sensetion--synset-ofs synset) (sensetion--synset-pos synset))))
   (synsets  (cl-sort (sensetion--es-lemma->synsets lemma pos) #'string< :key #'sensetion--synset-id))))


(defun sensetion--cache-lemma->senses (lemma &optional pos synset-cache)
  (unless synset-cache (sensetion--wordnet-lookup-lemma lemma synset-cache))
  (let ((senses-by-pos (gethash lemma synset-cache nil)))
    (if senses-by-pos
	(if pos
	    (alist-get pos senses-by-pos nil nil #'equal)
	  (seq-mapcat #'cdr senses-by-pos))
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
  (with-inhibiting-read-only
   (transpose-lines 1)
   (forward-line -2)))


(defun sensetion-move-line-down ()
  "Move down the current line."
  (interactive)
  (with-inhibiting-read-only
   (forward-line 1)
   (transpose-lines 1)
   (forward-line -1)))


(defun sensetion-toggle-scripts ()
  (interactive)
  (if (memq 'sensetion--scripts buffer-invisibility-spec)
      (remove-from-invisibility-spec 'sensetion--scripts)
    (add-to-invisibility-spec 'sensetion--scripts)))

(defun sensetion-refresh ()
  (interactive)
  (sensetion--map-buffer-lines
   (lambda (line-n line)
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

;;; sensetion.el --- -*- lexical-binding: t; -*-
(require 'seq)
(require 'subr-x)
(require 'ido)
(require 'f)
(require 'map)
(require 'async)
(require 'hydra)
(require 'sensetion-utils)
(require 'sensetion-data)
(require 'sensetion-client)
(require 'sensetion-edit)


;; TODO: benchmark stuff with
;; https://emacs.stackexchange.com/questions/539/how-do-i-measure-performance-of-elisp-code


(defgroup sensetion nil
  "Support for annotating word senses."
  :group 'data)


(defcustom sensetion-output-buffer-name "sensetion"
  "Buffer name where sensetion results are displayed."
  :group 'sensetion
  :type 'string)


(defcustom sensetion-elasticsearch-path
  nil
  :group 'sensetion
  :type (choice file (const nil)))


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


(defvar sensetion--completion-function
  (completion-table-dynamic
   (lambda (prefix)
     (sensetion-es-prefix-lemma prefix))))


(defvar-local sensetion--local-status
  nil
  "Local status.

A cons cell where the car is the number of tokens annotated so
far, and the cdr is the number of annotatable tokens.")


(defvar sensetion-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "s" #'sensetion-hydra/body)
    (define-key map "<" #'sensetion-previous-selected)
    (define-key map ">" #'sensetion-next-selected)
    (define-key map "/" #'sensetion-edit-sense)
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


(defvar-local sensetion--lemma
  nil
  "Lemma being annotated in the buffer.")


(defvar-local sensetion--synset-cache
  nil)


(defcustom sensetion-mode-line '(:eval (sensetion--mode-line-status-text))
  ""
  :group 'sensetion
  :type 'sexp
  :risky t)


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
  ;; customize mode line
  (setq-local mode-name '(:eval (sensetion--mode-line-status-text))))


(defun sensetion--mode-line-status-text ()
  (concat "sensetion"
          (if sensetion--local-status
              (cl-destructuring-bind (done . total) sensetion--local-status
                (format ":%.0f/%.0f" done total))
            "")))


;;;###autoload
(defalias 'sensetion #'sensetion-annotate)


(defun sensetion-annotate (lemma &optional pos)
  "Create targeted annotation buffer, where several sentences
containing tokens with LEMMA and optionally POS are displayed for
annotation."
  (interactive
   (list (sensetion--completing-read-lemma)
         (ido-completing-read "PoS tag? " '("a" "r" "v" "n" "any") nil t nil nil "any")))
  (unless lemma (user-error "Must provide lemma"))
  (sensetion-is
   (unless matches
     (if pos
         (user-error "No matches for lemma %s and PoS %s" lemma pos)
       (user-error "No matches for lemma %s as any PoS." lemma)))
   (with-current-buffer result-buffer
     (sensetion-mode)
     (with-inhibiting-read-only
      (setq sensetion--lemma lemma)
      (setq sensetion--synset-cache (sensetion--wordnet-lookup-lemma lemma))
      (setq sensetion--local-status (sensetion--make-collocations matches)))
     (sensetion--beginning-of-buffer))
   (pop-to-buffer result-buffer)
   :where
   (result-buffer (generate-new-buffer
                   (sensetion--create-buffer-name lemma pos)))
   (matches (sensetion--es-get-sents lemma pos))
   (pos (unless (equal pos "any") pos))))


(defun sensetion--create-buffer-name (lemma pos)
  (format "*%s@%s@%s*" sensetion-output-buffer-name (or pos "") lemma))


(defun sensetion--make-collocations (matches)
  "Insert MATCHES at current buffer, return status."
  (sensetion-is
   (cl-mapc #'go matches)
   (cons done total)
   :where
   (go (sent)
       (seq-let (tokens-line status)
           (colloc sent)
         (insert tokens-line
                 ;; no need to add it all the time
                 (propertize "\n" 'sensetion--sent-id (sensetion--sent-id sent)))
         (cl-incf done (car status))
         (cl-incf total (cdr status))))
   (colloc (sent)
	   (sensetion--sent-colloc sent senses))
   (senses (sensetion--cache-lemma->senses sensetion--lemma))
   (total   0)
   (done    0)))


(defun sensetion--get-sent-at-point ()
  (let ((sent-id (sensetion--get-text-property-eol 'sensetion--sent-id)))
    (sensetion--alist->sent (sensetion--es-id->sent sent-id))))


(defun sensetion--update-sent (sent)
  (sensetion--es-update-modified-sent sent))


(defun sensetion--tk-glob? (tk)
  (pcase (sensetion--tk-kind tk)
    (`(:glob ,k)
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
   (sensetion--reinsert-sent-at-point (unglob ck sent))
  
   :where

   (unglob (ck sent)
           (pcase sent
             ((cl-struct sensetion--sent id meta tokens text)
              (sensetion--make-sent
               :id id
               :meta meta
               :text text
               :tokens
               (cl-loop
                for tk in tokens
                ;; don't collect glob to be removed
                unless (when (equal ck (sensetion--tk-glob? tk))
                         ;; handle status update when glob was tagged
			 (when (sensetion--to-annotate? tk)
                           (cl-incf (cdr sensetion--local-status) -1))
                         (when (sensetion--tk-annotated? tk)
			   (cl-incf (car sensetion--local-status) -1))
                         t)
                collect (let ((tk-keys (sensetion--tk-coll-keys tk)))
                          (cond
                           ((equal (list ck) tk-keys)
                            ;; this token only part of one colloc
                            (setf (sensetion--tk-kind tk) "wf"))
                           ((member ck tk-keys)
                            ;; this token part of more than one colloc
                            (setf (sensetion--tk-kind tk)
                                  (cons "cf" (remove ck tk-keys)))))
                          tk))))))
   ;; TODO: select which colloc to undo?
   (ck (cl-first ckeys))
   (ckeys (sensetion--tk-coll-keys tk))
   (tk (elt (sensetion--sent-tokens sent) ix))))


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
                (sensetion--completing-read-lemma)
		(sensetion--completing-read-pos)
		(reverse (sensetion--get-text-property-eol 'sensetion--to-glob))
		(sensetion--get-sent-at-point)))
  (sensetion-is
   (sensetion--reinsert-sent-at-point globbed-sent)
   (with-inhibiting-read-only
    (sensetion--put-text-property-eol 'sensetion--to-glob nil))

   :where
  
   (globbed-sent (progn
		   (setf (sensetion--sent-tokens sent) globbed-tks)
		   sent))
   (globbed-tks (cl-loop
                 for tk in (sensetion--sent-tokens sent)
                 for i from 0
                 append (cond
                         ((equal i (cl-first ixs-to-glob))
                          ;; insert glob before first token in the
                          ;; collocation
                          (list new-glob (glob-tk tk new-k)))
                         ((cl-member i ixs-to-glob)
                          (list (glob-tk tk new-k)))
                         (t
                          (list tk)))))
   (glob-tk (tk key)
            (let ((cks (sensetion--tk-coll-keys tk)))
              (setf (sensetion--tk-kind tk)
                    (cl-list* "cf" key cks))
              ;; TODO: delete senses and make status "un"
              tk))
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


(defun sensetion--reinsert-sent-at-point (sent)
  "Save SENT, delete current line (where previous version of sent
was linearized), and reinsert SENT."
  (with-inhibiting-read-only
   (atomic-change-group
     (delete-region (line-beginning-position) (line-end-position))
     (seq-let (line _) (sensetion--sent-colloc sent)
       (insert line)))
   (sensetion--update-sent sent)))

;; TODO: when annotating glob, check if token is part of more than one
;; colloc

(cl-defun sensetion--sent-colloc (sent &optional senses (lemma sensetion--lemma))
  "Return a list whose first element is a propertized string
representing SENT's tokens for display in sensetion buffer, and
whose second element is a status pair, whose car is the number of
selected tokens already annotated and whose cdr is the total
number of selected tokens."
  ;; TODO: refactor this, it's too big
  (let ((done 0)
        (total 0)
        ;; stores selected glob tokens and their indices by their key;
        ;; this is used to highlight their constituent tokens and edit
        ;; them properly
        (sel-keys (make-hash-table :test 'equal))
	(senses (or senses (sensetion--cache-lemma->senses lemma))))
    (cl-labels
        ((sel-tk-props (tk &optional ix)
                       (cl-list* 'sensetion--selected t
                                 'face `(:foreground
                                         ,(pcase (sensetion--tk-tag tk)
                                            ("man-now"
                                             (if (sensetion--tk-confident-in-anno? tk)
                                                 sensetion-currently-annotated-colour
                                               sensetion-currently-annotated-unsure-colour))
                                            ("un"
                                             sensetion-unnanotated-colour)
                                            ((or "auto" "man")
                                             (if (sensetion--tk-confident-in-anno? tk)
                                                 sensetion-previously-annotated-colour
                                               sensetion-previously-annotated-unsure-colour))
                                            (_ (error "%s" tk))))
                                 (when-let ((key (sensetion--tk-glob? tk)))
                                   (list 'sensetion--glob-ix
                                         ix))))

         (token-colloc (tk ix)
                       (let* ((form-str  (sensetion--tk-form tk))
                              (kind      (sensetion--tk-kind tk))
                              (selected? (sensetion--to-annotate? tk lemma))
                              (ckeys     (sensetion--tk-coll-keys tk))
                              (glob-selected?
                               (unless (cdr ckeys)
                                 ;; don't highlight token part of more than one colloc
                                 (gethash (cl-first ckeys) sel-keys))))
                         (when selected?
                           (cl-incf total)
                           (when (sensetion--tk-annotated? tk)
                             (cl-incf done)))
                         (pcase kind
                           ((seq (or "wf" "cf"))
                            (concat
                             ;; spacing
                             " "
                             ;; collocation index
                             (if ckeys
                                 (propertize (s-join "," ckeys)
                                             'display '(raise -0.3)
                                             'invisible 'sensetion--scripts
                                             'face '(:height 0.6))
                               "")
                             ;; form string
                             (apply #'propertize
                                    form-str
                                    'sensetion--tk-ix ix
                                    (cond
                                     (glob-selected?
                                      (sel-tk-props (cdr glob-selected?)
                                                    (car glob-selected?)))
                                     (selected?
                                      (sel-tk-props tk))))
			     ;; pos script
                             (if-let ((_ selected?)
                                      (pos (or (sensetion--tk-senses-pos tk)
                                               (sensetion--tk-pos tk))))
                                 (propertize pos
                                             'display '(raise 0.4)
                                             'invisible 'sensetion--scripts
                                             'face '(:height 0.6))
                               "")
			     ;; sense scripts
                             (if-let ((_ (or selected?
                                             glob-selected?))
                                      (tk-sks (if selected?
						  (sensetion--tk-skeys tk)
						(sensetion--tk-skeys (cdr glob-selected?)))))
				 ;; should prob become sense number
				 ;; when that info is available
                                 (propertize (s-join ","
                                                     (mapcar (lambda (sk)
							       (cl-first
								(map-elt senses sk
									 nil #'equal)))
                                                             tk-sks))
                                             'display '(raise 0.4)
                                             'invisible 'sensetion--scripts
                                             'face '(:height 0.6))
                               "")))
                           (`("glob" ,glob-key)
                            (prog1 ""
                              (when selected?
                                (setf (gethash glob-key sel-keys) (cons ix tk)))))
			   ;; TODO: this could one day be used to
			   ;; customize how other kinds of tokens are
			   ;; shown
                           (_ "")))))
      ;;
      (let* ((tks        (sensetion--sent-tokens sent))
             (tks-colloc (seq-map-indexed #'token-colloc tks)))
        (list
         (substring (apply #'concat tks-colloc) 1)
         (cons done total))))))


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
              (st    (sensetion--sk-st (cl-first sks)))
              (sts    (if (member st '("3" "5"))
                          '("3" "5")
                        (list st)))
              (same? (seq-every-p (lambda (sk) (member (sensetion--sk-st sk) sts))
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


(defun sensetion--wordnet-lookup-lemma (lemma &optional pos)
  "(hash-table ,lemma ((,pos (,sense-key ,hydra-index ,synset-id ,terms ,gloss) ...) ...)"
  (let ((lemma (cl-substitute (string-to-char " ")
                              (string-to-char "_")
                              lemma :test #'eq))
	(poses (if pos (list pos) '("n" "v" "r" "a")))
	(options (make-hash-table :test 'equal :size 100)))
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


(defun sensetion--cache-lemma->senses (lemma &optional pos)
  (let ((senses-by-pos (gethash lemma sensetion--synset-cache nil)))
    (if pos
	(alist-get pos senses-by-pos nil nil #'equal)
	(seq-mapcat #'cdr senses-by-pos))))


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


(defhydra sensetion-hydra (:color blue)
  ("q" nil nil)
  ("s" nil nil)
  ("RET" nil nil)
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

;;; -*- lexical-binding: t; -*-
(require 'seq)
(require 'subr-x)
(require 'ido)
(require 's)
(require 'f)
(require 'async)
(eval-when-compile (require 'cl-lib))
(require 'cl-lib)
(require 'hydra)
(require 'trie)
(require 'sensetion-data)
(require 'sensetion-edit)


;; TODO: benchmark stuff with
;; https://emacs.stackexchange.com/questions/539/how-do-i-measure-performance-of-elisp-code

;; TODO: test the print-circle thing: see size of index of non-noun
;; synsets with and without it (I recall it being 42MB, but now it is
;; 32 WITH the noun synsets! also check if performance improves with
;; byte-compilation; it took almost 3 minutes to index and write all
;; synsets

(defgroup sensetion nil
  "Support for annotating word senses."
  :group 'data)


(defcustom sensetion-output-buffer-name "sensetion"
  "Buffer name where sensetion results are displayed."
  :group 'sensetion
  :type 'string)


(defcustom sensetion-index-file
  (expand-file-name "~/.sensetion-index")
  "Path to index file."
  :group 'sensetion
  :type 'file)


(defcustom sensetion-status-file
  (expand-file-name "~/.sensetion-status")
  "Path to status file."
  :group 'sensetion
  :type 'file)


(defcustom sensetion-annotation-dir
  nil
  "Path to annotation directory"
  :group 'sensetion
  :type 'directory)


(defcustom sensetion-annotation-file-type
  "plist"
  "File type (extension) of annotation files."
  :group 'sensetion
  :type 'string)


(defcustom sensetion-number-completions
  15
  "Number of completions to show."
  :group 'sensetion
  :type  'integer)


(defvar sensetion--completion-function
  (completion-table-dynamic
   (lambda (prefix)
     ;; TODO: randomize completion so that stuff like completing a to
     ;; a_ doesn't happen (as often)
     (trie-complete sensetion--index prefix nil sensetion-number-completions nil nil
                    (lambda (k _) (substring k 0 (- (length k) 2)))))))


(defvar sensetion--index
  nil
  "Index.

  A trie mapping lemmas to sentence ids.")

(defvar sensetion--global-status
  nil
  "Global status.

A cons cell where the car is the number of tokens annotated so
far, and the cdr is the total number of unnanotated tokens.")


(defvar-local sensetion--local-status
  nil
  "Local status.

A cons cell in the same format as `sensetion--global-status'.")


(defvar sensetion-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "<" #'sensetion-previous-selected)
    (define-key map ">" #'sensetion-next-selected)
    (define-key map "/" #'sensetion-edit)
    (define-key map "u" #'sensetion-unglob)
    (define-key map "l" #'sensetion-edit-lemma)
    (define-key map "m" #'sensetion-toggle-glob-mark)
    (define-key map "g" #'sensetion-glob)
    (define-key map "." #'sensetion-go-to-source)
    (define-key map [C-down] #'sensetion-move-line-down)
    (define-key map [C-up] #'sensetion-move-line-up)
    map)
  "Keymap for `sensetion-mode'.")


(defcustom sensetion-unnanoted-colour
  "salmon"
  "Color to display the selected tokens in."
  :group 'sensetion
  :type 'color)


(defcustom sensetion-previously-annotated-colour
  "green"
  "Color to display the tokens who have been previously
annotated."
  :group 'sensetion
  :type 'color)


(defcustom sensetion-currently-annotated-colour
  "dodger blue"
  "Color to display the selected tokens in."
  :group 'sensetion
  :type 'color)

(defcustom sensetion-unsure-colour
  "yellow"
  "Color to display the tokens whose annotation is unsure."
  :group 'sensetion
  :type 'color)


(defvar-local sensetion--lemma
  nil
  "Lemma being annotated in the buffer.")


(defvar-local sensetion--synset-cache
  nil
  "Should be an alist associating pos1 with the possible synsets
  for a lemma")


(defun sensetion--punctuation? (str)
  (gethash
   str
   #s(hash-table size 45 test equal rehash-size 1.5 rehash-threshold 0.8125
                 purecopy t data
                 ("." t "," t ":" t "!" t "?" t "'" t "]" t ")" t "..." t "»" t))))


(defun sensetion--beginning-of-buffer ()
  (goto-char (point-min)))


(defcustom sensetion-mode-line '(:eval (sensetion--mode-line-status-text))
  ""
  :group 'sensetion
  :type 'sexp
  :risky t)


(define-derived-mode sensetion-mode fundamental-mode "sensetion"
  "sensetion-mode is a major mode for annotating senses."
  (setq-local sentence-end ".$$") ;; to be able to use M-a and M-e to jump
  (setq-local buffer-read-only t)
  (visual-line-mode 1)
  (setq-local minor-mode-alist nil)
  (aset (or buffer-display-table
            (setq buffer-display-table (make-display-table)))
        ?\n [?\n?\n])
  (setq-local mode-name '(:eval (sensetion--mode-line-status-text))))


(defun sensetion--mode-line-status-text ()
  (concat "sensetion"
          ;; (if sensetion--global-status
          ;;     (cl-destructuring-bind (done . total) sensetion--global-status
          ;;       (format "|:%.0f%%" (* 100 (/ (float done) total))))
          ;;   "")
          (if sensetion--local-status
              (cl-destructuring-bind (done . total) sensetion--local-status
                (format ":%.0f/%.0f" done total))
            "")))


;;;###autoload
(defun sensetion ()
  (interactive)
  (add-hook 'kill-emacs-hook
            (lambda ()
              (sensetion--write-state)))
  (unless sensetion--index
    (if (file-exists-p sensetion-index-file)
        (progn
          (sensetion--read-state)
          (call-interactively #'sensetion-annotate))
      (sensetion-make-state sensetion-annotation-dir
                            (sensetion--done-indexing-messager)))))


;; (defun sensetion--setup-status-window ()
;;   (setq fit-window-to-buffer-horizontally t)
;;   (setq window-resize-pixelwise t)
;;   (setq display-buffer-alist
;;         (cons '("\\*sensetion-status\\*" display-buffer-in-side-window
;;                 (side . top) (slot . 0) (window-height . fit-window-to-buffer)
;;                 (preserve-size . (nil . t)) (no-other-window . t)
;;                 (no-delete-other-windows . t))
;;               display-buffer-alist)))


(cl-defun sensetion--annotation-files (&optional (anno-dir sensetion-annotation-dir))
  (f-files anno-dir
           (lambda (f) (equal (f-ext f) sensetion-annotation-file-type))))


(defun sensetion-annotate (lemma &optional pos)
  (interactive
   (list (completing-read "Lemma to annotate: " sensetion--completion-function)
         (ido-completing-read "PoS tag? " '("a" "r" "v" "n" "any") nil t nil nil "any")))
  (unless lemma (user-error "Must provide lemma"))
  ;; using regexp just to get all pos combinations
  ;; TODO: add $ to regexp
  (let* ((regexp (if (equal pos "any")
                     (concat lemma "%?[1234]?")
                   (concat lemma "%?" (sensetion--pos->synset-type pos) "?")))
         (filter-fn (sensetion--regexp-search-filter-fn))
         (matches (progn (trie-regexp-search sensetion--index regexp
                                             nil nil nil filter-fn)
                         (funcall filter-fn)))
         (result-buffer (generate-new-buffer
                         (sensetion--create-buffer-name lemma pos))))
    (unless matches
      (if pos
          (user-error "No matches for lemma %s and PoS %s" lemma pos)
        (user-error "No matches for lemma %s as any PoS." lemma)))
    (with-current-buffer result-buffer
      (sensetion-mode)
      (with-inhibiting-read-only
       (setq sensetion--lemma lemma)
       (setq sensetion--synset-cache (sensetion--wordnet-lookup lemma))
       (setq sensetion--local-status (sensetion--make-collocations matches)))
      (sensetion--beginning-of-buffer))
    (pop-to-buffer result-buffer)))


(defun safe-cons (car cdr)
  (if (listp cdr)
      (cons car cdr)
    (cons car (list cdr))))


(defun sensetion--regexp-search-filter-fn ()
  "`trie-regexp-search' returns an alist of the keys and their
  results. In our case the values might get repeated over several
  keys, which we do not want. So we use `trie-regexp-search'
  filterfn argument with this closure, which both builds a result
  in a format we want, and prevents `trie-regexp-search' from
  building its own result list.

The result function uniquifies the match values and returns nil;
when called without arguments will return a list of of two lists:
one of them contains the unique keys of the matches, and the
other the unique values."
  (let ((uniq-vals (make-hash-table :test 'equal)))
    (lambda (&optional key vals)
      (if key
          (progn
            (mapc (lambda (val) (setf (gethash val uniq-vals) t)) vals)
            nil)
        (hash-table-keys uniq-vals)))))


(defun sensetion--create-buffer-name (lemma pos)
  (format "*%s@%s@%s*" sensetion-output-buffer-name (or pos "") lemma))


(defun sensetion--get-sent (sent-id)
  (let* ((fp (sensetion--sent-id->filename sent-id))
         (text (f-read-text fp))
         (plist (read text)))
    (sensetion--plist->sent plist)))


(defun sensetion--get-sent-at-point ()
  (sensetion-is
   (unless sent-id
     (user-error "Not at sentence line"))
   (sensetion--get-sent sent-id)
   where
   (sent-id (sensetion--sent-id-prop-at-point))))


(defun sensetion--make-collocations (matches)
  "Insert MATCHES at current buffer, return status."
  (let ((done 0)
        (total 0))
    (cl-labels
        ((sent-colloc (sent-id)
                      (let ((sent (sensetion--get-sent sent-id)))
                        (seq-let (tokens-line status)
                            (sensetion--sent-colloc sent sensetion--lemma)
                          (insert tokens-line
                                  ;; no need to add it all the time
                                  (propertize "\n" 'sensetion--sent-id sent-id))
                          (cl-incf done (car status))
                          (cl-incf total (cdr status))))))
      ;;
      (seq-do #'sent-colloc matches)
      (cons done total))))


(defun sensetion--tk-glob? (tk)
  (pcase (sensetion--tk-kind tk)
    (`(:glob . ,k)
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
   
   where

   (unglob (ck sent)
           (pcase sent
             ((cl-struct sensetion--sent id terms tokens text)
              (sensetion--make-sent
               :id id
               :terms terms
               :text text
               :tokens
               (cl-loop
                for tk in tokens
                ;; don't collect glob to be removed
                unless (when (equal ck (sensetion--tk-glob? tk))
                         ;; handle status update when glob was tagged
                         (when (sensetion--tk-annotated? tk)
                           (cl-incf (car sensetion--global-status) -1)
                           (cl-incf (cdr sensetion--global-status) -1)
                           (when (sensetion--to-annotate? tk)
                             (cl-incf (car sensetion--local-status) -1)
                             (cl-incf (cdr sensetion--local-status) -1))))
                collect (let ((tk-keys (sensetion--tk-coll-keys tk)))
                          (cond
                           ((equal (list ck) tk-keys)
                            ;; this token only part of one colloc
                            (setf (sensetion--tk-kind tk) :wf))
                           ((member ck tk-keys)
                            ;; this token part of more than one colloc
                            (setf (sensetion--tk-kind tk)
                                  (cons :coll (remove ck tk-keys)))))
                          tk))))))
   ;; TODO: select which colloc to undo?
   (ck (cl-first ckeys))
   (ckeys (sensetion--tk-coll-keys tk))
   (tk (elt (sensetion--sent-tokens sent) ix))))


(defun sensetion--tk-coll-keys (tk)
  (pcase (sensetion--tk-kind tk)
    (`(:coll . ,ks) ks)))


(defun sensetion--mark-glob (beg end ix marked)
  "Marks token to be globbed with the `sensetion-glob' command."
  (with-inhibiting-read-only
   (put-text-property beg end
                      'face '(:foreground "yellow"))
   (put-text-property (line-end-position) (1+ (line-end-position))
                      'sensetion--to-glob (cons ix marked))))


(defun sensetion--tks-to-glob-prop ()
  (get-text-property (line-end-position) 'sensetion--to-glob))


(defun sensetion--unmark-glob (beg end ix marked)
  (with-inhibiting-read-only
   (put-text-property (line-end-position) (1+ (line-end-position))
                      'sensetion--to-glob (cl-remove ix marked))
   (remove-text-properties beg end '(face nil))))


(defun sensetion-toggle-glob-mark (beg end)
  "Mark or unmark token to be globbed with the `sensetion-glob'
command."
  (interactive (sensetion--tk-points))
  (unless (and beg end)
    (user-error "No token at point"))
  (let* ((ix (sensetion--tk-ix-prop-at-point beg))
         (marked (sensetion--tks-to-glob-prop))
         (marked? (cl-member ix marked)))
    (if marked?
        (sensetion--unmark-glob beg end ix marked)
      (sensetion--mark-glob beg end ix marked))))

(defun sensetion-glob (lemma)
  "Glob all tokens marked to be globbed, assigning it lemma
LEMMA.

You can mark tokens with `sensetion-toggle-glob-mark', or unmark them
with `sensetion-unmark-glob'."
  (interactive (list
                (s-join "_"
                        (s-split " "
                                 (read-string "Lemma of glob: " nil nil "") t))))
  (sensetion-is
   (sensetion--reinsert-sent-at-point globbed-sent)
   (with-inhibiting-read-only
    (put-text-property (line-end-position) (1+ (line-end-position))
                       'sensetion--to-glob nil))

   where
   
   (globbed-sent (pcase sent
                   ((cl-struct sensetion--sent id terms text)
                    (sensetion--make-sent
                     :id id
                     :terms terms
                     :text text
                     :tokens globbed-tks))))
   (globbed-tks (cl-loop
                      for tk in (sensetion--sent-tokens sent)
                      for i from 0
                      append (cond
                              ((equal i (cl-first ixs))
                               ;; insert glob before first token in the
                               ;; collocation
                               (list new-glob (glob-tk tk new-k)))
                              ((cl-member i ixs)
                               (list (glob-tk tk new-k)))
                              (t
                               (list tk)))))
   (glob-tk (tk key)
            (let ((cks (sensetion--tk-coll-keys tk)))
              (setf (sensetion--tk-kind tk)
                    (cl-list* :coll key cks))
              tk))
   (new-glob (sensetion--make-tk :form nil :lemma lemma :pos nil
                                 :status "un" :kind `(:glob . ,new-k)
                                 :anno nil :meta nil))
   (ixs   (reverse (get-text-property (line-end-position) 'sensetion--to-glob)))
   (new-k (char-to-string (1+ max-k)))
   (max-k (max-key sent))
   (max-key (sent)
            (apply #'max
                   (cons (string-to-char "`")
                         ;; in case there are no other globs, use
                         ;; "`" (which results in "a" being the
                         ;; key) , else get the maximum one
                         (seq-mapcat (lambda (tk)
                                       (when-let ((keys (sensetion--tk-coll-keys tk)))
                                         (mapcar #'string-to-char keys)))
                                     (sensetion--sent-tokens sent)))))
   (sent  (sensetion--get-sent-at-point))))


(defun sensetion--reinsert-sent-at-point (sent)
  "Delete current line, save SENT to its file, and insert SENT."
  (sensetion--save-sent sent)
  (with-inhibiting-read-only
   (delete-region (line-beginning-position) (line-end-position))
   (seq-let (line _) (sensetion--sent-colloc sent)
     (insert line))))

;; TODO: when annotating glob, check if token is part of more than one
;; colloc

(cl-defun sensetion--sent-colloc (sent &optional (lemma sensetion--lemma))
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
        (sel-keys (make-hash-table :test 'equal)))
    (cl-labels
        ((sel-tk-props (tk &optional ix)
                       (cl-list* 'sensetion--selected t
                                 'face `(:foreground
                                         ,(pcase (sensetion--tk-status tk)
                                            ((or "auto" "man")
                                             sensetion-previously-annotated-colour)
                                            ("un"
                                             sensetion-unnanoted-colour)
                                            ("unsure"
                                             sensetion-unsure-colour)
                                            ("now"
                                             sensetion-currently-annotated-colour)
                                            (_ (error "%s" tk))))
                                 (when-let ((key (sensetion--tk-glob? tk)))
                                   (list 'sensetion--glob-ix
                                         ix))))

         (token-colloc (tk ix)
                       (let* ((form-str  (sensetion--tk-form tk))
                              (selected? (sensetion--to-annotate? tk lemma))
                              (ckeys (sensetion--tk-coll-keys tk))
                              (glob-selected? (unless (cdr ckeys)
                                                ;; don't highlight
                                                ;; token part of more
                                                ;; than one colloc
                                                (gethash (cl-first ckeys) sel-keys)))
                              (punct? (sensetion--punctuation? form-str)))
                         (when selected?
                           (cl-incf total)
                           (when (sensetion--tk-annotated? tk)
                             (cl-incf done)))
                         (if-let ((glob-key (sensetion--tk-glob? tk)))
                             (prog1 ""
                               (when selected?
                                 (setf (gethash glob-key sel-keys) (cons ix tk))))
                           (concat
                            ;; spacing
                            (if punct? "" " ")
                            ;; collocation index
                            (if ckeys
                                (propertize (s-join "," ckeys)
                                            'display '(raise -0.3)
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
                            (if-let ((_ selected?)
                                     (pos (or (sensetion--tk-synset-pos tk)
                                              (sensetion--tk-pos tk))))
                                (propertize pos
                                            'display '(raise 0.4)
                                            'face '(:height 0.6))
                              "")
                            (if-let ((_ selected?)
                                     (sids (sensetion--tk-anno tk)))
                                (propertize (s-join ","
                                                    (mapcar (lambda (s)
                                                              (cl-first
                                                               (gethash s sensetion--synset-cache)))
                                                            sids))
                                            'display '(raise 0.4)
                                            'face '(:height 0.6))
                              ""))))))
      ;;
      (let* ((tks (sensetion--sent-tokens sent))
             (tks-colloc (seq-map-indexed #'token-colloc tks))
             (terms (sensetion--sent-terms sent))
             (pos   (substring (sensetion--sent-id sent) 0 1)))
        (list
         (apply #'concat "(" pos ") " (s-join "," terms) " |" tks-colloc)
         (cons done total))))))


(cl-defun sensetion--tk-has-lemma? (tk &optional (lemma sensetion--lemma))
  (sensetion-is
        (cl-member lemma
                   tk-lemmas
                   :test #'equal
                   :key #'lemma*->lemma)
   where
   (tk-lemmas (sensetion--tk-lemmas tk))))


(defun sensetion--tk-lemmas (tk)
  (sensetion-is
   (when lemma-str
     (s-split "|" lemma-str t))
   where
   (lemma-str (sensetion--tk-lemma tk))))


(cl-defun sensetion--to-annotate? (tk &optional (lemma sensetion--lemma))
  (and (sensetion--tk-annotatable? tk)
       (sensetion--tk-has-lemma? tk lemma)))


(defun sensetion--tk-synset-pos (tk)
  "Get pos1 of synsets assigned to TK. If there is more than one
synset and they have different pos1, return nil."
  (let ((senses (sensetion--tk-anno tk)))
    (when-let ((1pos  (elt (cl-first senses) 0))
               (same? (seq-every-p (lambda (s) (eql (elt s 0) 1pos)) senses)))
      (char-to-string 1pos))))


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


(defun sensetion--sent-id->filename (sent-id)
  (f-join sensetion-annotation-dir (concat sent-id "." sensetion-annotation-file-type)))


(defun sensetion--sent-id-prop-at-point ()
  (get-char-property (line-end-position) 'sensetion--sent-id))


(cl-defun sensetion--tk-ix-prop-at-point (&optional (point (point)))
  (sensetion-is
   (or ix
       (user-error "No token at point"))
   where
   (ix (get-char-property point 'sensetion--tk-ix))))


(defun sensetion-make-state (anno-dir callback)
  "Read annotated files and build index of lemmas* and their
positions, plus global status."
  (interactive
   (list (or sensetion-annotation-dir
             (read-file-name "Path to annotation directory: " nil nil t))
         (sensetion--done-indexing-messager)))
  (message "Plese wait while we index files; a box will pop when finished.")
  (async-start `(lambda ()
                  ,(async-inject-variables (regexp-opt '("load-path" "sensetion-index-file" "sensetion-status-file" "sensetion-annotation-file-type")))
                  (require 'sensetion)
                  (seq-let (index status)
                      (sensetion--make-state (sensetion--annotation-files ,anno-dir))
                    (sensetion--write-state :index index :status status)
                    t))
               (lambda (x)
                 (sensetion--read-state)
                 (funcall callback x))))


(defun sensetion--done-indexing-messager ()
  (lambda (_)
    (message-box "Done indexing files. You may call `sensetion-annotate' now")))


(defun sensetion--make-state (files)
  "Read annotated files and build index associating lemmas* to
where sentence-ids (and thus files) where they appear. Also
builds the status (how many tokens have been annotated so far)."
  (let ((index (make-trie #'<))
        (annotatable 0)
        (annotated 0))
    (cl-labels
        ((run (id f)
              (let ((sent (sensetion--plist->sent (read (f-read-text f)))))
                (index-sent id sent)))

         (index-sent (id sent)
                     (mapc (apply-partially #'index-token id)
                           (sensetion--sent-tokens sent)))

         (index-token (sent-id tk)
                      (when (sensetion--tk-annotatable? tk)
                        (cl-incf annotatable)
                        (let ((lemma (sensetion--tk-lemma tk)))
                          (unless lemma
                            (user-error "No lemma for tk %s at %s"
                                        (sensetion--tk-meta tk)
                                        sent-id))
                          (sensetion--index-lemmas index lemma sent-id))
                        (when (sensetion--tk-annotated? tk)
                          (cl-incf annotated)))))
      ;;
      (mapc (lambda (f) (run (file-name-base f) f)) files)
      (list index (cons annotated annotatable)))))


(defun sensetion--index-lemmas (index lemmas-str sent-id)
  ;; lemmas* might be pure ("love") or have pos annotation ("love%2"),
  ;; but we don't care about it here; when retrieving we gotta take
  ;; care of this.
  (mapc (lambda (lemma)
          (trie-insert index lemma sent-id #'safe-cons))
        (s-split "|" lemmas-str t)))


(defun sensetion--remove-lemmas (index old-lemma-str sent)
  (sensetion-is
   (mapc (apply-partially #'remove-lemma tokens) old-lemmas)
   where
   (tokens (sensetion--sent-tokens sent))
   (old-lemmas (s-split "|" old-lemma-str t))
   (remove-lemma (tokens old-lemma)
                 (unless (seq-some (lambda (tk)
                                     (sensetion--tk-has-lemma? tk old-lemma))
                                   tokens)
                   (trie-insert index old-lemma nil
                                (lambda (_ old-data) (remove sent-id old-data)))))
   (sent-id (sensetion--sent-id sent))))


(defun sensetion--tk-annotatable? (tk)
  ;; TODO: make status keywords
  (pcase (sensetion--tk-kind tk)
    (`(:coll . ,_) nil)
    (_ (let ((status (sensetion--tk-status tk)))
         (when (member status
                       '("man" "un" "unsure" "auto" "now"))
           status)))))


(cl-defun sensetion--write-state (&key (index sensetion--index)
                                       (status sensetion--global-status))
  (let ((print-circle t))
    (with-temp-file sensetion-index-file
      (prin1 index (current-buffer))))
  (f-write (with-output-to-string (prin1 status))
           'utf-8
           sensetion-status-file)
  t)


(defun sensetion--read-state ()
  "Read index from `sensetion-index-file', and set
`sensetion--index'. Read status from `sensetion-status-file' and
set `sensetion--global-status'. "
  ;; TODO: benchmark if f-read is faster (if needed)
  (with-temp-message "Reading index"
    (with-temp-buffer
      (insert-file-contents sensetion-index-file)
      (goto-char (point-min))           ;is this needed?
      (setq sensetion--index (read (current-buffer))))
    (setq sensetion--global-status (read (f-read-text sensetion-status-file))))
  t)


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


(defun sensetion--wordnet-lookup (lemma)
  "Return hash-table where the keys are synset ids and the values
are a list where the first element is the key (as shown in the
edit hydra) and the second is the gloss string."
  (let ((senses (make-hash-table :test 'equal)))
    (cl-labels
        ((pos3->pos1 (pos3)
                     (gethash
                      pos3
                      #s(hash-table size 5 test equal
                                    rehash-size 1.5 rehash-threshold 0.8125
                                    purecopy t data
                                    ("nou" "n" "ver" "v" "adj" "a" "adv" "r"))))

         (parse-sense (pos1 mline ix)
                      (cl-assert (null (cl-rest mline)))
                      (let* ((line (cl-first mline))
                             (beg (1+ (cl-position (string-to-char "{")
                                                   line :test #'char-equal)))
                             (end (cl-position (string-to-char "}")
                                               line :start beg :test #'char-equal))
                             (offset (substring line beg end))
                             (words-gloss (substring line (1+ end))))
                        (setf (gethash (concat pos1 offset) senses)
                              (list
                               (format "%s"
                                       (if (< ix 9)
                                           ;; handling more than 10
                                           ;; senses: will list from 1
                                           ;; to 9, then alphabetic
                                           ;; characters, which start
                                           ;; at 97 ('a') reserved 0
                                           ;; for no sense
                                           (1+ ix)
                                         (char-to-string (+ ix 88))))
                               (s-trim words-gloss))))))
      ;;
      (let* ((command (format "wn '%s' -g -o -over" lemma))
             (result  (shell-command-to-string command)))
        (when (equal (s-trim result) "")
          (user-error "No senses found for lemma %s." lemma))
        (let* ((chunks  (s-split "\nOverview of " result t))
               ;; easier to analyse first 3 characters
               (chunk-by-pos (-group-by (lambda (c) (pos3->pos1 (substring c 0 3)))
                                        chunks))
               (_  (mapc
                    (lambda (pair)
                      (seq-map-indexed
                       (apply-partially #'parse-sense (cl-first pair))
                       (s-match-strings-all "^[0-9].*$"
                                            (cl-second pair))))
                    chunk-by-pos)))
          senses)))))





(defun sensetion-edit-lemma (tk-ix sent)
  ;; TODO: if lemma changes, synset chosen doesn't make any sense
  ;; anymore; how much sense does it make to not postag at the same
  ;; time?
  (interactive (list (sensetion--tk-ix-prop-at-point)
                     (sensetion--get-sent-at-point)))
  (let* ((old   (sensetion--tk-lemma (elt (sensetion--sent-tokens sent) tk-ix)))
         (lemma (read-string "Assign lemma to token: " (cons old (1+ (length old))))))
    (sensetion--edit-lemma old lemma tk-ix sent)))


(defun sensetion--edit-lemma (old-lemma lemma tk-ix sent)
  (setf (sensetion--tk-lemma (elt (sensetion--sent-tokens sent) tk-ix))
        lemma)
  (sensetion--remove-lemmas sensetion--index old-lemma sent)
  (sensetion--reinsert-sent-at-point sent)
  (sensetion--index-lemmas sensetion--index lemma (sensetion--sent-id sent)))


(defalias 'sensetion--tk-annotated? 'sensetion--tk-anno
  "Return t if token has annotation.")


(defun sensetion--save-sent (sent)
  (f-write (pp-to-string (sensetion--sent->plist sent))
           'utf-8
           (sensetion--sent-id->filename (sensetion--sent-id sent))))


;; TODO: refactor prop names as variables
(cl-defun sensetion--tk-points (&optional (point (point)))
  (list (previous-single-property-change point 'sensetion--tk-ix
                                         nil (line-beginning-position))
        (next-single-property-change point 'sensetion--tk-ix
                                     nil (line-end-position))))


(defun sensetion--make-lemma* (lemma synset-type)
  (concat lemma "%" synset-type))


(defun lemma*->lemma (lemma*)
  (let ((st (lemma*->st lemma*)))
    (if st
        (substring lemma* 0 (- (length lemma*) 2))
      lemma*)))


(defun lemma*->st (lemma*)
  (let ((len (length lemma*)))
    (when (and (> len 1) (= (elt lemma* (- len 2)) (string-to-char "%")))
      (substring lemma* (1- len)))))


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

(defun sensetion-go-to-source (sent-id)
  (interactive (list (sensetion--sent-id-prop-at-point)))
  (sensetion-is
   (unless sent-buff
     (error "No sentence found; please report bug"))
   (pop-to-buffer sent-buff nil t)
   where
   (sent-buff (find-file-noselect sent-fp))
   (sent-fp   (sensetion--sent-id->filename sent-id))))


(defmacro with-inhibiting-read-only (&rest body)
  `(let ((inhibit-read-only t))
     ,@body))


(defmacro sensetion-is (&rest body)
  (seq-let (body wclauses)
      (-split-when (lambda (c) (eq 'where c)) body)
    (let ((body
           (-reduce-from
            (lambda (bd cl)
              (pcase cl
                (`(,var ,val)
                 `((let ((,var ,val))
                     ,@bd)))
                (`(,name ,arglist . ,body)
                 `((cl-labels ((,name ,arglist ,@body))
                     ,@bd)))))
            body
            wclauses)))
      (cl-first body))))

;; (defmacro defun/where (name arglist &rest body)
;;   "
;; \(fn NAME ARGLIST &optional DOCSTRING DECL &rest BODY)"
;;   (declare (doc-string 3) (indent 2) (debug defun))
;;   `(defun ,name ,arglist
;;      (sensetion-is
;;       ,@body)))


(provide 'sensetion)

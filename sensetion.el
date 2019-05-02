;;; sensetion.el --- -*- lexical-binding: t; -*-
(require 'seq)
(require 'subr-x)
(require 'ido)
(require 's)
(require 'f)
(require 'async)
(require 'hydra)
(require 'trie)
(require 'sensetion-utils)
(require 'sensetion-data)
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


(defcustom sensetion-backend-url
  "http://localhost:9200"
  "URL to backend server."
  :group 'sensetion
  :type 'url)


(defcustom sensetion-number-completions
  15
  "Number of lemma completions to show in `sensetion-annotate'."
  :group 'sensetion
  :type  'integer)


(defcustom sensetion-sense-menu-show-synset-id
  nil
  "Show synset id in sense menu during annotation."
  :group 'sensetion
  :type  'boolean)


(defvar sensetion--completion-function
  (completion-table-dynamic
   (lambda (prefix)
     (sensetion-es-prefix-lemma prefix sensetion-number-completions))))


(defvar-local sensetion--local-status
  nil
  "Local status.

A cons cell in the same format as `sensetion--global-status'.")


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
    (define-key map "." #'sensetion-edit-synset)
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


(define-derived-mode sensetion-mode fundamental-mode "sensetion"
  "sensetion-mode is a major mode for annotating senses."
  (setq-local sentence-end ".$$") ;; to be able to use M-a and M-e to jump
  (setq-local buffer-read-only t)
  (visual-line-mode 1)
  (setq-local buffer-invisibility-spec nil)
  (setq-local minor-mode-alist nil)
  (aset (or buffer-display-table
            (setq buffer-display-table (make-display-table)))
        ?\n [?\n?\n])
  (setq-local mode-name '(:eval (sensetion--mode-line-status-text)))
  (setq-local write-contents-functions (list (lambda () t))))


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
  (if sensetion--index
      (call-interactively #'sensetion-annotate)
      (if (and (f-exists? sensetion-index-file) (f-exists? sensetion-status-file))
          (progn
            (sensetion--read-state)
            (call-interactively #'sensetion-annotate))
        (sensetion-make-state sensetion-annotation-dir
                     (sensetion--done-indexing-messager)))))


(cl-defun sensetion--annotation-files (&optional (anno-dir sensetion-annotation-dir))
  (f-files anno-dir
           (lambda (f) (equal (f-ext f) sensetion-annotation-file-type))))


(defun sensetion--check-index-nonnil ()
  (unless sensetion--index
    (user-error "Index is missing. Have you run `M-x sensetion`? If yes, please check your annotation files and call `M-x sensetion-make-index`. If none of that works, report the bug")))


(defun sensetion-annotate (lemma &optional pos)
  (interactive
   (list (completing-read "Lemma to annotate: " sensetion--completion-function)
         (ido-completing-read "PoS tag? " '("a" "r" "v" "n" "any") nil t nil nil "any")))
  (unless lemma (user-error "Must provide lemma"))
  (sensetion--check-index-nonnil)
  ;; using regexp just to get all pos combinations
  ;; TODO: add $ to regexp (there's no need, I think)
  (sensetion-is
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
   (pop-to-buffer result-buffer)
   :where
   (matches (progn (trie-regexp-search sensetion--index regexp
                                       nil nil nil filter-fn)
                   (funcall filter-fn)))
   (filter-fn (sensetion--regexp-search-filter-fn))
   (result-buffer (generate-new-buffer
                   (sensetion--create-buffer-name lemma pos)))
   (regexp (if (equal pos "any")
               (concat lemma "%?[1234]?")
             (concat lemma "%?" (sensetion--pos->synset-type pos) "?")))))


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


(defun sensetion--get-synsets (coords)
  (mapcar #'cl-second (sensetion--get-coords-synsets coords)))

(defun sensetion--get-coords-synsets (coords)
  (sensetion-is
   (mapcan (lambda (gr)
             (go (cl-first gr)
                 (srt-lines gr)))
           coords-by)
   :where
   (srt-lines (gr)
              (sort (mapcar #'cl-second (cl-rest gr)) #'<))
   (go (fp lines)
       (with-temp-buffer
         (insert-file-contents fp)
         (cl-loop for curr in lines
                  for prev in (cons 0 lines)
                  collect
                  (progn
                    (sensetion--goto-line curr prev)
                    (let ((plist (read (thing-at-point 'line t))))
                      (list (cons (sensetion--filename->ix fp) curr) (sensetion--plist->synset plist)))))))
   (coords-by (seq-group-by #'car coords))
   (coords (mapcar #'sensetion--coordinates coords))))


(defun sensetion--get-synset (coord)
  (cl-first (sensetion--get-synsets (list coord))))


(defun sensetion--get-synset-at-point ()
  (sensetion-is
   (unless coord
     (user-error "Not at synset line"))
   (sensetion--get-synset coord)
   :where
   (coord (sensetion--synset-coord-prop-at-point))))


(defun sensetion--make-collocations (matches)
  "Insert MATCHES at current buffer, return status."
  (sensetion-is
   (cl-mapc #'go c-syns)
   (cons done total)
   :where
   (go (coord-synset)
       (seq-let (coord synset) coord-synset
           (seq-let (tokens-line status)
               (sensetion--synset-colloc synset sensetion--lemma)
             (insert tokens-line
                     ;; no need to add it all the time
                     (propertize "\n" 'sensetion--synset-coord coord))
             (cl-incf done (car status))
             (cl-incf total (cdr status)))))
   (c-syns (sensetion--get-coords-synsets matches))
   (total   0)
   (done    0)))


(defun sensetion--tk-glob? (tk)
  (pcase (sensetion--tk-kind tk)
    (`(:glob . ,k)
     k)))


;; TODO: unglobbing (or any kind of editing that calls synset-colloc)
;; turns just annotated tokens into previously annotated tokens
(defun sensetion-unglob (ix synset)
  "If token of index IX in SYNSET at point is part of collocation,
unglob it and reinsert the sentence in the buffer.

Unglobbing means making all tokens in the collocation normal
tokens, and removing the glob token corresponding to the
collocation."
  (interactive (list (sensetion--tk-ix-prop-at-point)
                     (sensetion--get-synset-at-point)))
  ;; (sensetion-is
  ;;  (unless ck
  ;;    (user-error "Token is not part of a collocation"))
  ;;  (when (cdr ckeys)
  ;;    (user-error "Please select token which is part of only one collocation"))
  ;;  (sensetion--reinsert-synset-at-point (unglob ck synset))
   
  ;;  :where

  ;;  (unglob (ck synset)
  ;;          (pcase synset
  ;;            ((cl-struct sensetion--synset ofs pos keys gloss tokens)
  ;;             (sensetion--make-synset
  ;;              :ofs ofs
  ;;              :pos pos
  ;;              :keys keys
  ;;              :gloss gloss
  ;;              :tokens
  ;;              (cl-loop
  ;;               for tk in tokens
  ;;               ;; don't collect glob to be removed
  ;;               unless (when (equal ck (sensetion--tk-glob? tk))
  ;;                        ;; handle status update when glob was tagged
  ;;                        (when (sensetion--tk-annotated? tk)
  ;;                          (cl-incf (car sensetion--global-status) -1)
  ;;                          (cl-incf (cdr sensetion--global-status) -1)
  ;;                          (when (sensetion--to-annotate? tk)
  ;;                            (cl-incf (car sensetion--local-status) -1)
  ;;                            (cl-incf (cdr sensetion--local-status) -1)))
  ;;                        t)
  ;;               collect (let ((tk-keys (sensetion--tk-coll-keys tk)))
  ;;                         (cond
  ;;                          ((equal (list ck) tk-keys)
  ;;                           ;; this token only part of one colloc
  ;;                           (setf (sensetion--tk-kind tk) :wf))
  ;;                          ((member ck tk-keys)
  ;;                           ;; this token part of more than one colloc
  ;;                           (setf (sensetion--tk-kind tk)
  ;;                                 (cons :cf (remove ck tk-keys)))))
  ;;                         tk))))))
  ;;  ;; TODO: select which colloc to undo?
  ;;  (ck (cl-first ckeys))
  ;;  (ckeys (sensetion--tk-coll-keys tk))
  ;;  (tk (elt (sensetion--synset-tokens synset) ix)))
  )


(defun sensetion--tk-coll-keys (tk)
  (pcase (sensetion--tk-kind tk)
    (`(:cf . ,ks) ks)))


(defun sensetion--mark-glob (beg end ix marked)
  "Marks token to be globbed with the `sensetion-glob' command."
  (with-inhibiting-read-only
   (put-text-property beg end
                      'face '(:foreground "brown"))
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

You can mark/unmark tokens with `sensetion-toggle-glob-mark'."
  (interactive (list
                (completing-read "Lemma to annotate: " sensetion--completion-function)))
  (sensetion--index-lemmas sensetion--index lemma (sensetion--synset-coord-prop-at-point))
  (sensetion-is
   (sensetion--reinsert-synset-at-point globbed-synset)
   (with-inhibiting-read-only
    (put-text-property (line-end-position) (1+ (line-end-position))
                       'sensetion--to-glob nil))

   :where
   
   (globbed-synset (pcase synset
                   ((cl-struct sensetion--synset ofs pos keys gloss)
                    (sensetion--make-synset
                     :ofs ofs
                     :pos pos
                     :keys keys
                     :gloss gloss
                     ;; :tokens globbed-tks
		     ))))
   (globbed-tks (cl-loop
                 for tk in (sensetion--synset-tokens synset)
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
                    (cl-list* :cf key cks))
              ;; TODO: delete senses and make status "un"
              tk))
   (new-glob (sensetion--make-tk :lemma lemma :tag "un"
                        :kind `(:glob . ,new-k) :glob "man"))
   (ixs   (reverse (get-text-property (line-end-position) 'sensetion--to-glob)))
   (new-k (char-to-string (1+ max-k)))
   (max-k (max-key synset))
   (max-key (synset)
            (apply #'max
                   (cons (string-to-char "`")
                         ;; in case there are no other globs, use "`"
                         ;; (which results in "a" being the key),
                         ;; else get the maximum one
                         (seq-mapcat (lambda (tk)
                                       (when-let ((keys (sensetion--tk-coll-keys tk)))
                                         (mapcar #'string-to-char keys)))
                                     (sensetion--synset-tokens synset)))))
   (synset  (sensetion--get-synset-at-point))))


(defun sensetion--reinsert-synset-at-point (synset)
  "Delete current line, save SYNSET to its file, and insert SYNSET."
  (let ((coord (sensetion--synset-coord-prop-at-point)))
    (sensetion--save-synset synset coord))
  (with-inhibiting-read-only
   (atomic-change-group
     (delete-region (line-beginning-position) (line-end-position))
     (seq-let (line _) (sensetion--synset-colloc synset)
       (insert line)))))

;; TODO: when annotating glob, check if token is part of more than one
;; colloc

(cl-defun sensetion--synset-colloc (synset &optional (lemma sensetion--lemma))
  "Return a list whose first element is a propertized string
representing SYNSET's tokens for display in sensetion buffer, and
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
        (ignoring?   nil))
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
                                 (gethash (cl-first ckeys) sel-keys)))
                              (punct-before? (and form-str
                                                  (sensetion--punctuation-no-space-before? form-str))))
                         (when selected?
                           (cl-incf total)
                           (when (sensetion--tk-annotated? tk)
                             (cl-incf done)))
                         (pcase kind
                           ((or :wf `(:cf . ,_))
                            (concat
                             ;; spacing
                             (if punct-before? "" " ")
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
                                     (ignoring? nil)
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
                                             'invisible 'sensetion--scripts
                                             'face '(:height 0.6))
                               "")
                             (if-let ((_ (or selected?
                                             glob-selected?))
                                      (sks (if selected?
                                               (sensetion--tk-skeys tk)
                                             (sensetion--tk-skeys (cdr glob-selected?)))))
                                 (propertize (s-join ","
                                                     (mapcar (lambda (sk)
                                                               (cl-first
                                                                (gethash sk sensetion--synset-cache)))
                                                             sks))
                                             'display '(raise 0.4)
                                             'invisible 'sensetion--scripts
                                             'face '(:height 0.6))
                               "")))
                           (`(:glob . ,glob-key)
                            (prog1 ""
                              (when selected?
                                (setf (gethash glob-key sel-keys) (cons ix tk)))))
                           (:aux
                            (if (eq (sensetion--tk-action tk) :close)
                                (setf ignoring? nil)
                              (when (eq (sensetion--tk-tag tk) :ignore)
                                (setf ignoring? t)))
                            "")
                           ((or :qf :ex :mwf :def :classif) "")
                           (_ (error "Token of kind %s does not exist" kind))))))
      ;;
      (let* ((tks        (sensetion--synset-tokens synset))
             (tks-colloc (seq-map-indexed #'token-colloc tks))
             (terms      (sensetion--synset-terms synset))
             (pos        (sensetion--synset-pos synset)))
        (list
         (apply #'concat "(" pos ") " (s-join "," terms) " |" tks-colloc)
         (cons done total))))))


(cl-defun sensetion--tk-has-lemma? (tk &optional (lemma sensetion--lemma))
  (sensetion-is
   (cl-member lemma
              tk-lemmas
              :test #'equal
              :key #'sensetion--lemma*->lemma)
   :where
   (tk-lemmas (sensetion--tk-lemmas tk))))


(defun sensetion--tk-lemmas (tk)
  (sensetion-is
   (when lemma-str
     (s-split "|" lemma-str t))
   :where
   (lemma-str (sensetion--tk-lemma tk))))


(cl-defun sensetion--to-annotate? (tk &optional (lemma sensetion--lemma))
  (and (sensetion--tk-annotatable? tk)
       (sensetion--tk-has-lemma? tk lemma)
       (null (sensetion--tk-coll-keys tk))))


(defun sensetion--tk-synset-pos (tk)
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


(defun sensetion--coordinates (coord)
  (sensetion-is
   (list
    (f-join sensetion-annotation-dir fname)
    line)
   :where
   (fname (concat fbase "." sensetion-annotation-file-type))
   (line  (cdr coord))
   (fbase (number-to-string (car coord)))))


(defun sensetion--synset-coord-prop-at-point ()
  (get-char-property (line-end-position) 'sensetion--synset-coord))


(cl-defun sensetion--tk-ix-prop-at-point (&optional (point (point)))
  (sensetion-is
   (or ix
       (user-error "No token at point"))
   :where
   (ix (get-char-property point 'sensetion--tk-ix))))

(defalias 'sensetion-make-index #'sensetion-make-state)

(defun sensetion-make-state (anno-dir callback)
  "Read annotated files and build index of lemmas* and their
positions, plus global status."
  (interactive
   (list (or sensetion-annotation-dir
             (read-file-name "Path to annotation directory: " nil nil t))
         (sensetion--done-indexing-messager)))
  (if sensetion--index-lock
      (user-error "Indexing process already started; please wait while it finishes")
    (message "Please wait while we index files; a box will pop up when finished.")
    (atomic-change-group
      (setq sensetion--index-lock t)
      (async-start `(lambda ()
                      ,(async-inject-variables (regexp-opt '("load-path" "sensetion-index-file" "sensetion-status-file" "sensetion-annotation-file-type")))
                      (require 'sensetion)
                      (seq-let (index lemma->synsets status)
                          (sensetion--make-state (sensetion--annotation-files ,anno-dir))
                        (sensetion--write-state :index index
                                       :status status
                                       :lemma->synsets lemma->synsets)
                        t))
                   (lambda (x)
                     (sensetion--read-state)
                     (setq sensetion--index-lock nil)
                     (funcall callback x))))))


(defun sensetion--done-indexing-messager ()
  (lambda (_)
    (message-box "Done indexing files. You may call `sensetion-annotate' now")))


(defun sensetion--make-state (files)
  "Read annotated files and build index associating lemmas* to
where synset-ids (and thus files) where they appear. Also
builds the status (how many tokens have been annotated so far)."
  (let ((index            (make-trie #'<))
        (lemma->synsets   (make-trie #'<))
        (annotatable       0)
        (annotated         0))
    (cl-labels
        ((run (f)
              (sensetion--map-file-lines f
                           (lambda (lno l)
                             (index-synset (sensetion--filename->ix f)
                                           lno
                                           (sensetion--plist->synset (read l))))))

         (index-synset (ix lno synset)
                       (let ((coord (cons ix lno))
                             (terms (sensetion--synset-terms synset))
                             (tokens (sensetion--synset-tokens synset)))
                         (mapc (lambda (term)
                                 (trie-insert lemma->synsets
                                              term
                                              (list coord)
                                              #'append))
                               terms)
                         (mapc (apply-partially #'index-token coord)
                               tokens)))

         (index-token (coord tk)
                      (when (sensetion--tk-annotatable? tk)
                        (cl-incf annotatable)
                        (let ((lemma (sensetion--tk-lemma tk)))
                          (unless lemma
                            (user-error "No lemma for tk %s at file %s, line %s"
                                        (sensetion--tk->plist tk)
                                        (car coord)
                                        (cdr coord)))
                          (sensetion--index-lemmas index lemma coord))
                        (when (sensetion--tk-annotated? tk)
                          (cl-incf annotated)))))
      ;;
      (mapc #'run files)
      (list index lemma->synsets (cons annotated annotatable)))))


(defun sensetion--index-lemmas (index lemmas-str coord)
  ;; lemmas* might be pure ("love") or have pos annotation ("love%2"),
  ;; but we don't care about it here; when retrieving we gotta take
  ;; care of this.
  (mapc (lambda (lemma)
          (trie-insert index lemma (list coord) #'append))
        (s-split "|" lemmas-str t)))


(defun sensetion--remove-lemmas (index old-lemma-str synset coord)
  "Remove lemmas in OLD-LEMMA-STR from INDEX if they are not
present in SYNSET's tokens."
  (sensetion-is
   (mapc (apply-partially #'remove-lemma tokens) old-lemmas)
   :where
   (tokens (sensetion--synset-tokens synset))
   (old-lemmas (s-split "|" old-lemma-str t))
   (remove-lemma (tokens old-lemma)
                 (unless (seq-some (lambda (tk)
                                     (sensetion--tk-has-lemma? tk old-lemma))
                                   tokens)
                   (trie-insert index old-lemma nil
                                (lambda (_ old-data) (remove coord old-data)))))))


(defun sensetion--tk-annotatable? (tk)
  ;; TODO: make status keywords
  (let ((status (sensetion--tk-tag tk)))
    (when (member
           status
           '("man" "man-now" "un" "auto"))
      status)))


(cl-defun sensetion--write-state (&key (index sensetion--index)
                              (status sensetion--global-status)
                              (lemma->synsets sensetion--lemma->synsets))
  (let ((print-circle t))
    (with-temp-file sensetion-index-file
      (prin1 index (current-buffer))
      (prin1 lemma->synsets (current-buffer))))
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
      (setq sensetion--index (read (current-buffer)))
      (setq sensetion--lemma->synsets (read (current-buffer))))
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


(defun sensetion--wordnet-lookup (lemma pos &optional options)
  "Return hash-table where the keys are synset keys and the
values are a list where the first element is sense key shown by
the edit hydra, the second is the synset id, the third are the
terms defined by that synset, and the fourth is the gloss."
  (sensetion-is
   (seq-mapn #'index synsets)
   options
   :where
   (index (synset)
          (let ((pos (sensetion--synset-pos synset)))
            (setf (gethash (lemma-sk lemma synset) options)
                  (list (ix->hydra-key counter)
                        (sensetion--synset-id synset)
                        (sensetion--synset-terms synset)
                        (sensetion--synset-gloss synset)))
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
              (car (cl-find lemma (cl-mapcar #'cons (sensetion--synset-keys synset) (sensetion--synset-lemmas synset))
			    :test #'equal :key #'cdr))
              (error "No matching sensekey for lemma %s in synset %s-%s"
                     lemma (sensetion--synset-ofs synset) (sensetion--synset-pos synset))))
   (synsets  (cl-sort (sensetion--es-lemma->synsets lemma pos) #'< :key #'sensetion--synset-ofs)) ;FIXME
   (options  (or options (make-hash-table :test 'equal :size 30)))
   (lemma (cl-substitute (string-to-char " ")
                         (string-to-char "_")
                         lemma))))


(defun sensetion--tk-annotated? (tk)
  (member (sensetion--tk-tag tk)
          '("man" "auto" "man-now")))


(defun sensetion--save-synset (synset coord)
  (seq-let (fp line) (sensetion--coordinates coord)
    (with-temp-file fp
      (insert-file-contents fp)
      (forward-line line)
      (atomic-change-group
        (delete-region (line-beginning-position) (line-end-position))
        (prin1 (sensetion--synset->plist synset) (current-buffer))))))


;; TODO: refactor prop names as variables
(cl-defun sensetion--tk-points (&optional (point (point)))
  (list (previous-single-property-change point 'sensetion--tk-ix
                                         nil (line-beginning-position))
        (next-single-property-change point 'sensetion--tk-ix
                                     nil (line-end-position))))


(defun sensetion--make-lemma* (lemma synset-type)
  (concat lemma "%" synset-type))


(defun sensetion--lemma*->lemma (lemma*)
  (let ((st (sensetion--lemma*->st lemma*)))
    (if st
        (substring lemma* 0 (- (length lemma*) 2))
      lemma*)))


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


(defun sensetion--filename->ix (fp)
  (string-to-number (f-base fp)))


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
  ("." sensetion-edit-synset "Edit data file" :column "Edit")
  ("m" sensetion-toggle-glob-mark "(Un)Mark token for globbing" :column "Globbing")
  ("g" sensetion-glob "Glob marked tokens" :column "Globbing")
  ("u" sensetion-unglob "Unglob token's collocation" :column "Globbing")
  ("<left>" sensetion-previous-selected "Go to previous selected token" :column "Navigation" :color pink)
  ("<right>" sensetion-next-selected "Go to next selected token" :column "Navigation" :color pink)
  ("<up>" sensetion-move-line-up "Move sentence up" :column "Navigation" :color pink)
  ("<down>" sensetion-move-line-down "Move sentence down" :column "Navigation" :color pink))


(provide 'sensetion)

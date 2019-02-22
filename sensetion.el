;;; -*- lexical-binding: t; -*-
(require 'seq)
(require 'ido)
(require 's)
(require 'f)
(require 'async)
(eval-when-compile (require 'cl-lib))
(require 'cl-lib)
(require 'hydra)
(require 'trie)


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
    (define-key map "m" #'sensetion-toggle-glob-mark)
    (define-key map "g" #'sensetion-glob)
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
      (sensetion-make-state (sensetion--annotation-files)
                            (lambda (_)
                              (call-interactively #'sensetion-annotate))))))


;; (defun sensetion--setup-status-window ()
;;   (setq fit-window-to-buffer-horizontally t)
;;   (setq window-resize-pixelwise t)
;;   (setq display-buffer-alist
;;         (cons '("\\*sensetion-status\\*" display-buffer-in-side-window
;;                 (side . top) (slot . 0) (window-height . fit-window-to-buffer)
;;                 (preserve-size . (nil . t)) (no-other-window . t)
;;                 (no-delete-other-windows . t))
;;               display-buffer-alist)))


(defun sensetion--annotation-files ()
  (f-files sensetion-annotation-dir))


(defun sensetion-annotate (lemma &optional pos)
  (interactive
   (list (completing-read "Lemma to annotate: " sensetion--completion-function)
         (ido-completing-read "PoS tag? " '("a" "r" "v" "n" "any") nil t nil nil "any")))
  (unless lemma (user-error "Must provide lemma"))
  ;; using regexp just to get all pos combinations
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
       (setq sensetion--local-status (sensetion--make-collocations matches))
       (setq sensetion--synset-cache (sensetion--wordnet-lookup lemma)))
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
  (sensetion--get-sent (get-text-property (line-end-position) 'sensetion--sent-id)))


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
                                  (propertize "\n" 'sensetion--sent-id sent-id)
                                  "\n")
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
  (unless (and ix sent)
    (user-error "No token at point"))
  (cl-labels
      ((unglob (ck sent)
               (sensetion--make-sent
                :id (sensetion--sent-id sent)
                :tokens
                (cl-loop
                 for tk in (sensetion--sent-tokens sent)
                 unless (when (equal ck (sensetion--tk-glob? tk))
                          ;; handle status update when glob was tagged
                          (when (sensetion--tk-annotated? tk)
                            (cl-incf (car sensetion--global-status) -1)
                            (cl-incf (cdr sensetion--global-status) -1)
                            (when (equal sensetion--lemma (sensetion--tk-lemma tk))
                              (cl-incf (car sensetion--local-status) -1)
                              (cl-incf (cdr sensetion--local-status) -1))))
                 collect (progn
                           (when (equal ck (sensetion--tk-coll-key tk))
                             (setf (sensetion--tk-kind tk) :wf))
                           tk)))))
    ;;
    (let* ((tk (elt (sensetion--sent-tokens sent) ix))
           (ck (sensetion--tk-coll-key tk)))
      (unless ck
        (user-error "Token is not part of a collocation"))
      (sensetion--reinsert-sent-at-point (unglob ck sent)))))


(defun sensetion--tk-coll-key (tk)
  (pcase (sensetion--tk-kind tk)
    (`(:coll . ,k) k)))


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
  (cl-labels
      ((max-key (sent)
                (apply #'max
                       (cons (string-to-char "`")
                             ;; in case there are no other globs, use
                             ;; "`" (which results in "a" being the
                             ;; key) , else get the maximum one
                             (seq-mapcat (lambda (tk)
                                           (when-let ((key (sensetion--tk-coll-key tk)))
                                             (list (string-to-char key))))
                                         (sensetion--sent-tokens sent)))))

       (glob (sent ixs new-k)
             (sensetion--make-sent
              :id (sensetion--sent-id sent)
              :tokens
              (cons
               (sensetion--make-tk :form nil :lemma lemma
                                   :status "un" :kind '(:glob . new-k)
                                   :anno nil :meta nil)
               (cl-loop
                for tk in (sensetion--sent-tokens sent)
                for i from 0
                collect (if (cl-member i ixs)
                            (progn
                              (setf (sensetion--tk-kind tk) (cons :coll new-k))
                              tk)
                          tk))))))
    ;;
    (let* ((ixs   (reverse (get-text-property (line-end-position) 'sensetion--to-glob)))
           (sent  (sensetion--get-sent-at-point))
           (max-k (max-key sent))
           (new-k (char-to-string (1+ max-k)))
           (sent-w-glob (glob sent ixs new-k)))
      (sensetion--reinsert-sent-at-point sent-w-glob)
      (with-inhibiting-read-only
       (put-text-property (line-end-position) (1+ (line-end-position))
                          'sensetion--to-glob nil)))))


(defun sensetion--reinsert-sent-at-point (sent)
  "Delete current line, save SENT to its file, and insert SENT."
  (sensetion--save-sent sent)
  (with-inhibiting-read-only
   (delete-region (line-beginning-position) (line-end-position))
   (seq-let (line _) (sensetion--sent-colloc sent)
     (insert line))))


(cl-defun sensetion--sent-colloc (sent &optional (lemma sensetion--lemma))
  "Return a list whose first element is a propertized string
representing SENT's tokens for display in sensetion buffer, and
whose second element is a status pair, whose car is the number of
selected tokens already annotated and whose cdr is the total
number of selected tokens."
  (let ((done 0)
        (total 0))
    (cl-labels
        ((token-colloc (tk ix)
                       (if (sensetion--tk-glob? tk)
                           ""
                         (let* ((form-str  (sensetion--tk-form tk))
                                (lemma-str (sensetion--tk-lemma tk))
                                (selected? (and lemma-str
                                                (cl-member
                                                 lemma
                                                 (s-split "|" lemma-str t)
                                                 :test #'equal
                                                 ;; TODO: not always a lemma*
                                                 :key #'lemma*->lemma)))
                                (punct? (sensetion--punctuation? form-str)))
                           (when selected?
                             (cl-incf total)
                             (when (sensetion--tk-annotated? tk)
                               (cl-incf done)))
                           (concat
                            ;; spacing
                            (if punct? "" " ")
                            ;; collocation index
                            (if-let ((k (sensetion--tk-coll-key tk)))
                                (propertize k
                                            'display '(raise -0.3)
                                            'face '(:height 0.6))
                              "")
                            ;; form string
                            (apply #'propertize
                                   form-str
                                   'sensetion--tk-ix ix
                                   (when selected?
                                     ;; TODO: add pos,sense-index,stuff
                                     (list
                                      'sensetion-selected t
                                      'face `(:foreground
                                              ,(pcase (sensetion--tk-status tk)
                                                 ((or "auto" "man")
                                                  sensetion-previously-annotated-colour)
                                                 ("un"
                                                  sensetion-unnanoted-colour)
                                                 ("now"
                                                  sensetion-currently-annotated-colour)
                                                 (_ (error "%s" tk))))))))))))
      ;;
      (let* ((tks (sensetion--sent-tokens sent))
             (tks-colloc (seq-map-indexed #'token-colloc tks)))
        (list
         (substring                     ; to remove starting space
          (apply #'concat tks-colloc) 1)
         (cons done total))))))


(defun sensetion-previous-selected (point)
  (interactive (list (point)))
  (let ((selected? (sensetion--selected? point)))
    (goto-char
     (previous-single-property-change point 'sensetion-selected nil (point-min)))
    (when selected? (sensetion-previous-selected (point)))))


(defun sensetion-next-selected (point)
  (interactive (list (point)))
  (let ((selected? (sensetion--selected? point)))
    (goto-char
     (next-single-property-change point 'sensetion-selected nil (point-max)))
    (when selected? (sensetion-next-selected (point)))))


(defun sensetion--selected? (point)
  (get-text-property point 'sensetion-selected))


(defun sensetion--sent-id->filename (sent-id)
  (f-join sensetion-annotation-dir (concat sent-id "." sensetion-annotation-file-type)))


(defun sensetion--sent-id-prop-at-point ()
  (get-char-property (line-end-position) 'sensetion--sent-id))


(cl-defun sensetion--tk-ix-prop-at-point (&optional (point (point)))
  (get-char-property point 'sensetion--tk-ix))

(defun sensetion-make-state (files callback)
  "Read annotated files and build index of lemmas* and their
positions, plus global status."
  (interactive
   (list (directory-files (or sensetion-annotation-dir
                              (read-file-name "Path to annotation directory: " nil nil t))
                          t
                          (concat "\\." sensetion-annotation-file-type "$"))
         (lambda (_) (message "Done indexing files"))))
  (with-temp-message "Indexing files.."
    (async-start `(lambda ()
                    ,(async-inject-variables "\\sensetion-")
                    (sensetion--make-state files))
                 callback)))


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
                          (mapc (lambda (lemma)
                                  (index-lemma sent-id lemma))
                                (s-split "|" lemma)))
                        (when (sensetion--tk-annotated? tk)
                          (cl-incf annotated))))

         (index-lemma (sent-id lemma)
                      ;; lemmas* might be pure ("love") or have pos
                      ;; annotation ("love%2"), but we don't care
                      ;; about it here; when retrieving we gotta take
                      ;; care of this.
                      (trie-insert index lemma sent-id #'safe-cons)))
      ;;
      (mapc (lambda (f) (run (file-name-base f) f)) files)
      (setq sensetion--index index)
      (setq sensetion--global-status (cons annotated annotatable))
      t)))


(defun sensetion--tk-annotatable? (tk)
  ;; TODO: make status keywords
  (pcase (sensetion--tk-kind tk)
    (`(:coll . ,_) nil)
    (_ (let ((status (sensetion--tk-status tk)))
         (when
             (cl-member status
                        '("man" "un" "auto")
                        :test #'equal)
           status)))))


(defun sensetion--write-state ()
  (with-temp-file sensetion-index-file
    (prin1 sensetion--index (current-buffer)))
  (f-write (with-output-to-string (prin1 sensetion--global-status))
           'utf-8
           sensetion-status-file)
  t)


(defun sensetion--read-state ()
  "Read index from `sensetion-index-file', and set
`sensetion--index'. Read status from `sensetion-status-file' and
set `sensetion--global-status'. "
  ;; TODO:benchmark if f-read is faster (if needed)
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
  (cl-labels
      ((pos3->pos1 (pos3)
                   (gethash
                    pos3
                    #s(hash-table size 5 test equal
                                  rehash-size 1.5 rehash-threshold 0.8125
                                  purecopy t data
                                  ("nou" "n" "ver" "v" "adj" "a" "adv" "r"))))

       (parse-sense (pos1 mline)
                    (cl-assert (null (cl-rest mline)))
                    (let* ((line (cl-first mline))
                           (beg (1+ (cl-position (string-to-char "{")
                                                 line :test #'char-equal)))
                           (end (cl-position (string-to-char "}")
                                             line :start beg :test #'char-equal))
                           (offset (substring line beg end))
                           (words-gloss (substring line (1+ end))))
                      (list (concat pos1 offset)
                            words-gloss))))
    ;;
    (let* ((command (format "wn '%s' -g -o -over" lemma))
           (result  (shell-command-to-string command)))
      (when (equal (s-trim result) "")
        (user-error "No senses found for lemma %s." lemma))
      (let* ((chunks  (s-split "\nOverview of " result t))
             ;; easier to analyse first 3 characters
             (chunk-by-pos (-group-by (lambda (c) (pos3->pos1 (substring c 0 3)))
                                      chunks))
             (senses  (mapcar
                       (lambda (pair)
                         (list (cl-first pair)
                               (mapcar (apply-partially #'parse-sense (cl-first pair))
                                       (s-match-strings-all "^[0-9].*$"
                                                            (cl-second pair)))))
                       chunk-by-pos)))
        senses))))


(defun sensetion-edit (lemma pos ix sent)
  (interactive (list (if (sensetion--selected? (point))
                         (or (buffer-local-value 'sensetion--lemma (current-buffer))
                             (error "No local sensetion--lemma"))
                       (user-error "No taggable token at point"))
                     ;; TODO: handle "other"
                     (ido-completing-read "Token PoS tag: " '("a" "n" "r" "v" "other")
                                          nil t nil nil "other")
                     (sensetion--tk-ix-prop-at-point)
                     (sensetion--get-sent-at-point)))
  (unless (and lemma pos ix sent)
    "No taggable token at point.")
  (sensetion--edit lemma pos ix sent))


(defun sensetion--edit (lemma pos1 ix sent)
  (let ((senses (cl-second (assoc pos1 sensetion--synset-cache))))
    (unless senses
      (user-error "No senses for lemma %s with pos %s" lemma pos1))
    (call-interactively
     (eval (sensetion--edit-hydra-maker lemma pos1 ix sent senses)))))


(defun sensetion--edit-hydra-maker (lemma pos1 tk-ix sent options)
  `(defhydra hydra-senses (:color blue)
     ""
     ,@(seq-map-indexed
        (lambda (x ix)
          (list (format "%s" (if (< ix 10)
                                 ;; handling more than 10 senses: will
                                 ;; list from 0 to 9, then alphabetic
                                 ;; characters, which start at 97 ('a')
                                 ix
                               (char-to-string (+ ix 87))))
                `(lambda () (interactive)
                   (sensetion--annotate-sense ,lemma
                                              ,(sensetion--pos->synset-type
                                                pos1)
                                              ,(cl-first x)
                                              ,tk-ix
                                              ,sent))
                (cl-second x) :column "Pick sense:"))
        options)))


(defun sensetion--annotate-sense (lemma st sense ix sent)
  (let ((annotated? (sensetion--tk-annotated? (elt (sensetion--sent-tokens sent) ix))))
    (setf (sensetion--tk-lemma (elt (sensetion--sent-tokens sent) ix))
          (sensetion--make-lemma* lemma st)
          (sensetion--tk-anno (elt (sensetion--sent-tokens sent) ix))
          ;; TODO:should this be list?
          (list sense)
          (sensetion--tk-status (elt (sensetion--sent-tokens sent) ix))
          "now")
    (sensetion--reinsert-sent-at-point sent)
    ;; TODO: actually search for token index?
    (sensetion-previous-selected (point))
    (unless annotated?
      (cl-incf (car sensetion--global-status))
      (cl-incf (car sensetion--local-status)))))


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
  (substring lemma* 0 (- (length lemma*) 2)))


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


(defmacro with-inhibiting-read-only (&rest body)
  `(let ((inhibit-read-only t))
     ,@body))


(cl-defstruct (sensetion--tk (:constructor nil)
                             (:constructor sensetion--make-tk))
  form lemma status kind anno meta)


(cl-defstruct (sensetion--sent (:constructor nil)
                               (:constructor sensetion--make-sent))
  id tokens)


(defun sensetion--plist->sent (plist)
  (sensetion--make-sent :id (plist-get plist :id)
                        :tokens (mapcar #'sensetion--plist->tk
                                        (plist-get plist :tokens))))


(defun sensetion--plist->tk (plist)
  (let ((form (plist-get plist :form))
        (lemma (plist-get plist :lemma))
        (status (plist-get plist :status))
        (kind (plist-get plist :kind))
        (anno (plist-get plist :anno))
        (meta (plist-get plist :meta)))
    (sensetion--make-tk :form form :lemma lemma
                        :status status :kind kind
                        :anno anno :meta meta)))

(defun sensetion--sent->plist (sent)
  (list :id (sensetion--sent-id sent)
        :tokens (mapcar #'sensetion--tk->plist (sensetion--sent-tokens sent))))


(defun sensetion--tk->plist (tk)
  (cl-mapcan #'list '(:form :lemma :status :kind :anno :meta)
             (list (sensetion--tk-form tk)
                   (sensetion--tk-lemma tk)
                   (let ((st (sensetion--tk-status tk)))
                     (if (equal st "now")
                         "man"
                       st))
                   (sensetion--tk-kind tk)
                   (sensetion--tk-anno tk)
                   (sensetion--tk-meta tk))))


(provide 'sensetion)

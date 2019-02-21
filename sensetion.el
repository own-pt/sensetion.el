;;; -*- lexical-binding: t; -*-
(require 'seq)
(require 'ido)
(require 's)
(require 'f)
(require 'dash)
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


(defvar sensetion--index
  nil
  "Index.

Maps a lemma* to a list of lists of sentence-id and token-id.")


(defvar sensetion-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "<" #'sensetion-previous-selected)
    (define-key map ">" #'sensetion-next-selected)
    (define-key map "/" #'sensetion-edit)
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


(defun sensetion--punctuation? (str)
  (gethash
   str
   #s(hash-table size 45 test equal rehash-size 1.5 rehash-threshold 0.8125
                 purecopy t data
                 ("." t "," t ":" t "!" t "?" t "'" t "]" t ")" t "..." t "»" t))))


(defun sensetion--beginning-of-buffer ()
  (goto-char (point-min)))


(define-derived-mode sensetion-mode fundamental-mode "sensetion"
  "sensetion-mode is a major mode for annotating senses."
  (setq-local sentence-end ".$$") ;; to be able to use M-a and M-e to jump
  (setq buffer-read-only t)
  (visual-line-mode 1))

;;;###autoload
(defun sensetion ()
  (interactive)
  (unless sensetion--index
    (if (file-exists-p sensetion-index-file)
        (sensetion--read-index sensetion-index-file)
      (sensetion-make-index (sensetion--annotation-files)))
    (add-hook 'kill-emacs-hook
              (lambda ()
                (sensetion--write-index sensetion-index-file sensetion--index)))
    ;; TODO: setup status window here
    (call-interactively #'sensetion-annotate)))


(defun sensetion--annotation-files ()
  (f-files sensetion-annotation-dir))


(defun sensetion-annotate (lemma &optional pos)
  (interactive
   (list (read-string "Lemma to annotate: ")
         (ido-completing-read "PoS tag?" '("a" "r" "v" "n" "any") nil t nil nil "any")))
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
    (sensetion--make-collocations lemma matches result-buffer)
    (with-current-buffer result-buffer
      (sensetion-mode)
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


(defun sensetion--make-collocations (lemma matches result-buffer)
  (cl-labels
      ((get-sent (sent-id)
                 (let* ((fp (sensetion--sent-id->filename sent-id))
                        (text (f-read-text fp))
                        (plist (read text)))
                   (sensetion--plist->sent plist)))

       (sent-colloc (sent-id)
                    (with-current-buffer result-buffer
                      (let* ((sent (get-sent sent-id))
                             (tokens-line (sensetion--sent-colloc lemma sent)))
                        (insert tokens-line
                                (propertize "\n" 'sensetion-sent sent)
                                "\n")))))
      ;;
      (seq-do #'sent-colloc matches)))


(defun sensetion--sent-colloc (lemma sent)
  (cl-labels
      ((token-colloc (tk ix)
                     (pcase (sensetion--tk-kind tk)
                       (`(:glob . ,_) "")
                       (_
                        (let* ((form-str  (sensetion--tk-form tk))
                               (lemma-str (sensetion--tk-lemma tk))
                               (selected? (and lemma-str
                                               (cl-member
                                                lemma
                                                (s-split "|" lemma-str t)
                                                :test #'equal
                                                :key #'lemma*->lemma)))
                               (punct? (sensetion--punctuation? form-str)))
                          (concat
                           (if punct? "" " ")
                           (apply #'propertize
                                  form-str
                                  'sensetion-token-ix ix
                                  (when selected?
                                    ;; TODO: add pos,sense-index,stuff
                                    (list
                                     ;; TODO: lemma could be buffer
                                     ;; local variable
                                     'sensetion-selected lemma
                                     'face `(:foreground
                                             ,(pcase (sensetion--tk-status tk)
                                                ("man" sensetion-previously-annotated-colour)
                                                ("un" sensetion-unnanoted-colour)
                                                ("now" sensetion-currently-annotated-colour))))))))))))
    ;; 
    (let* ((tks (sensetion--sent-tokens sent))
           (tks-colloc (seq-map-indexed #'token-colloc tks)))
      (substring                        ; to remove starting space
       (apply #'concat tks-colloc) 1))))


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


(defun sensetion--sent-prop-at-point ()
  (get-char-property (line-end-position) 'sensetion-sent))


(defun sensetion--token-ix-prop-at-point ()
  (get-char-property (point) 'sensetion-token-ix))

(defun sensetion-make-index (files)
  "Read annotated files and build index of lemmas* and their
positions."
  (interactive
   (list (directory-files (or sensetion-annotation-dir
                              (read-file-name "Path to annotation directory: " nil nil t))
                          t
                          (concat "\\." sensetion-annotation-file-type "$"))))
  (let ((index (sensetion--make-index files)))
    (setq sensetion--index index))
  t)


(defun sensetion--annotatable? (tk)
  ;; TODO: make status keywords
  (pcase (sensetion--tk-kind tk)
    (`(:coll . ,_) nil)
    (_ (let ((status (sensetion--tk-status tk)))
         (when
             (cl-member status
                        '("man" "un")
                        :test #'equal)
           status)))))


(defun sensetion--make-index (files)
  "Read annotated files and build hash-table associating lemmas*
to where in the files they appear."
  (let ((index (make-trie #'<)))
    (cl-labels
        ((run (id f)
              (let ((sent (sensetion--plist->sent (read (f-read-text f)))))
                (index-sent id sent)))

         (index-sent (id sent)
                     (mapc (apply-partially #'index-token id)
                           (sensetion--sent-tokens sent)))

         (index-token (sent-id tk)
                      (when (sensetion--annotatable? tk)
                        (let ((lemma (sensetion--tk-lemma tk)))
                          (unless lemma
                            (user-error "No lemma for tk %s at %s"
                                        (sensetion--tk-meta tk)
                                        sent-id))
                          (mapc (lambda (lemma)
                                  (index-lemma sent-id lemma))
                                (s-split "|" lemma)))))
         
         (index-lemma (sent-id lemma)
                      ;; lemmas* might be pure ("love") or have pos
                      ;; annotation ("love%2"), but we don't care
                      ;; about it here; when retrieving we gotta take
                      ;; care of this.
                      (trie-insert index lemma sent-id #'safe-cons)))
      ;;
      (mapc (lambda (f) (run (file-name-base f) f)) files)
      index)))

(defun sensetion--write-index (index-file index)
  "Write INDEX to INDEX-FILE."
  (with-temp-file index-file
    (prin1 index (current-buffer)))
  t)


(defun sensetion--read-index (index-file)
  "Read index from INDEX-FILE, and set `sensetion--index'."
  ;; TODO:benchmark if f-read is faster (if needed)
  (with-temp-buffer
    (insert-file-contents index-file)
    (goto-char (point-min))             ;is this needed?
    (setq sensetion--index (read (current-buffer))))
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


(defun sensetion--wordnet-lookup (lemma pos1)
  (cl-labels
      ((pos1->pos3 (pos1)
                   (gethash
                    pos1
                    #s(hash-table size 5 test equal rehash-size 1.5 rehash-threshold 0.8125
                                  purecopy t data
                                  ("n" "nou" "v" "ver" "a" "adj" "r" "adv"))))

       (parse-sense (mline)
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
      (if (equal result "")
          (user-error "No senses found for lemma %s." lemma)
        (let* ((chunks  (s-split "\nOverview of " result t))
               ;; easier to analyse first 3 characters
               (poses   (mapcar (lambda (c) (substring c 0 3)) chunks))
               (pos3    (pos1->pos3 pos1))
               (pos-ix  (seq-position poses (or pos3
                                                (error "Can't map %s to PoS tag." pos1))))
               (chunk   (elt chunks pos-ix))
               (senses  (s-match-strings-all "^[0-9].*$" chunk)))
          (mapcar #'parse-sense senses))))))


(defun sensetion-edit (lemma pos ix sent)
  (interactive (list (or (get-char-property (point) 'sensetion-selected)
                         (user-error "No taggable token at point."))
                     (ido-completing-read "Token PoS tag: " '("a" "n" "r" "v" "other")
                                          nil t nil nil "other")
                     (sensetion--token-ix-prop-at-point)
                     (sensetion--sent-prop-at-point)))
  (unless (and lemma pos ix sent)
    "No taggable token at point.")
  (sensetion--edit lemma pos ix sent))


(defun sensetion--edit (lemma pos1 ix sent)
  (let ((senses (sensetion--wordnet-lookup lemma pos1)))
    (call-interactively
     (eval (sensetion--edit-hydra-maker lemma pos1 ix sent senses)))))


(defun sensetion--edit-hydra-maker (lemma pos1 tk-ix sent options)
  `(defhydra hydra-senses (:color blue)
              ""
              ,@(seq-map-indexed
                 (lambda (x ix)
                   (list (format "%s" ix)
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
  (setf (sensetion--tk-lemma (elt (sensetion--sent-tokens sent) ix))
        (sensetion--make-lemma* lemma st)
        (sensetion--tk-anno (elt (sensetion--sent-tokens sent) ix))
        ;; TODO:should this be list?
        (list sense)
        (sensetion--tk-status (elt (sensetion--sent-tokens sent) ix))
        "now")
  (with-inhibiting-read-only
   (delete-region (line-beginning-position) (line-end-position))
   (insert (sensetion--sent-colloc lemma sent))
   ;; TODO: actually search for token index?
   (sensetion-previous-selected (point))
   (sensetion--save-sent sent)))


(defun sensetion--save-sent (sent)
  (f-write (pp-to-string (sensetion--sent->plist sent))
           'utf-8
           (sensetion--sent-id->filename (sensetion--sent-id sent))))


;; TODO: refactor prop names as variables
(cl-defun sensetion--token-points (&optional (point (point)))
  (list (previous-single-property-change point 'sensetion-token-ix
                                         nil (line-beginning-position))
        (next-single-property-change point 'sensetion-token-ix
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

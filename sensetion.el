;;; sensetion.el --- -*- lexical-binding: t; -*-
(require 'seq)
(require 'subr-x)
(require 'ido)
(require 'f)
(require 'async)
(require 'hydra)
(require 'trie)
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
  (setq-local sentence-end ".$$") ;; to be able to use M-a and M-e to jump
  ;; (setq-local buffer-read-only t)
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
          (if sensetion--local-status
              (cl-destructuring-bind (done . total) sensetion--local-status
                (format ":%.0f/%.0f" done total))
            "")))


;;;###autoload
(defalias 'sensetion #'sensetion-annotate)


(defun sensetion-annotate (lemma &optional pos)
  (interactive
   (list (completing-read "Lemma to annotate: " sensetion--completion-function)
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
      (setq sensetion--synset-cache (sensetion--wordnet-lookup lemma "n")) ;FIXME
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
           (sensetion--sent-colloc sent sensetion--lemma)
         (insert tokens-line
                 ;; no need to add it all the time
                 (propertize "\n" 'sensetion--sent sent))
             (cl-incf done (car status))
             (cl-incf total (cdr status))))
   (total   0)
   (done    0)))


(defun sensetion--get-sent-at-point ()
  (get-text-property (line-end-position) 'sensetion--sent))


(defun sensetion--store-sent (sent)
  (put-text-property-eol 'sensetion--sent sent))


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
   (put-text-property beg end
                      'face `(:foreground ,sensetion-glob-mark-colour))
   (put-text-property-eol 'sensetion--to-glob (cons ix marked))))


(defun sensetion--tks-to-glob-prop ()
  (get-text-property (line-end-position) 'sensetion--to-glob))


(defun sensetion--unmark-glob (beg end ix marked)
  (with-inhibiting-read-only
   (put-text-property-eol 'sensetion--to-glob (cl-remove ix marked))
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


(defun sensetion-glob (lemma pos)
  "Glob all tokens marked to be globbed, assigning it lemma
LEMMA.

You can mark/unmark tokens with `sensetion-toggle-glob-mark'."
  (interactive (list
                (completing-read "Lemma to annotate: " sensetion--completion-function)
		(ido-completing-read "PoS tag? " '("a" "r" "v" "n") nil t nil nil)))
  (sensetion-is
   (sensetion--reinsert-sent-at-point globbed-sent)
   (with-inhibiting-read-only
    (put-text-property-eol 'sensetion--to-glob nil))

   :where
  
   (globbed-sent (pcase sent
                   ((cl-struct sensetion--sent id meta text)
                    (sensetion--make-sent
                     :id id
                     :meta meta
                     :tokens globbed-tks
                     :text text))))
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
                    (cl-list* "cf" key cks))
              ;; TODO: delete senses and make status "un"
              tk))
   (new-glob (sensetion--make-tk :lemmas (list (sensetion--make-lemma* lemma st)) :tag "un"
                        :kind `("glob" ,new-k) :glob "man"))
   (st (sensetion--pos->synset-type pos))
   (ixs   (reverse (get-text-property (line-end-position) 'sensetion--to-glob)))
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
                                     (sensetion--sent-tokens sent)))))
   (sent  (sensetion--get-sent-at-point))))


(defun sensetion--reinsert-sent-at-point (sent)
  "Delete current line, save SENT to its file, and insert SENT."
  (with-inhibiting-read-only
   (sensetion--store-sent sent)
   (atomic-change-group
     (delete-region (line-beginning-position) (line-end-position))
     (seq-let (line _) (sensetion--sent-colloc sent)
       (insert line)))))

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
                           (`("glob" ,glob-key)
                            (prog1 ""
                              (when selected?
                                (setf (gethash glob-key sel-keys) (cons ix tk)))))
                           ((seq (or "aux" "qf" "ex" "mwf" "def" "classif")) "")
                           (_ (error "Token of kind %s does not exist" kind))))))
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


(defun sensetion--sent-coord-prop-at-point ()
  (get-char-property (line-end-position) 'sensetion--sent-coord))


(cl-defun sensetion--tk-ix-prop-at-point (&optional (point (point)))
  (sensetion-is
   (or ix
       (user-error "No token at point"))
   :where
   (ix (get-char-property point 'sensetion--tk-ix))))


(defun sensetion--index-lemmas (index lemmas-str coord)
  ;; lemmas* might be pure ("love") or have pos annotation ("love%2"),
  ;; but we don't care about it here; when retrieving we gotta take
  ;; care of this.
  (mapc (lambda (lemma)
          (trie-insert index lemma (list coord) #'append))
        (s-split "|" lemmas-str t)))


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
          (setf (gethash (lemma-sk lemma synset) options)
		(list (ix->hydra-key counter)
		      (sensetion--synset-id synset)
		      (sensetion--synset-terms synset)
		      (sensetion--synset-gloss synset)))
	  (cl-incf counter))
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
   (synsets  (cl-sort (sensetion--es-lemma->synsets lemma pos) #'< :key #'sensetion--synset-ofs))
   (options  (or options (make-hash-table :test 'equal :size 30)))
   (lemma    (cl-substitute (string-to-char " ")
                            (string-to-char "_")
                            lemma))))


(defun sensetion--tk-annotated? (tk)
  (member (sensetion--tk-tag tk)
          '("man" "auto" "man-now")))


;; TODO: refactor prop names as variables
(cl-defun sensetion--tk-points (&optional (point (point)))
  (list (previous-single-property-change point 'sensetion--tk-ix
                                         nil (line-beginning-position))
        (next-single-property-change point 'sensetion--tk-ix
                                     nil (line-end-position))))


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

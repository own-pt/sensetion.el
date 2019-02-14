;;; -*- lexical-binding: t; -*-
(require 'seq)
(require 'ido)
(require 's)
(require 'f)
(require 'cl-lib)
(require 'conllu-parse)
(require 'hydra)


(defgroup sensetion nil
  "Support for annotating senses in CoNLL-U files."
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
  "conllu"
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
    (define-key map "." #'sensetion-go-to-token)
    ;; (define-key map "/" #'sensetion-edit)
    map)
  "Keymap for `sensetion-mode'.")


(defcustom sensetion-selected-token-colour-name
  "dodger blue"
  "Color to display the selected tokens in."
  :group 'sensetion
  :type 'color)


(defun sensetion--punctuation? (str)
  (gethash
   str
   #s(hash-table size 45 test equal rehash-size 1.5 rehash-threshold 0.8125
                 purecopy t data
                 ("." t "," t ":" t "!" t "?" t "'" t "]" t ")" t "--" t "..." t "»" t))))


(defun sensetion--beginning-of-buffer ()
  (goto-char (point-min)))


(define-derived-mode sensetion-mode fundamental-mode "qonllu"
  "sensetion-mode is a major mode for annotating senses."
  (setq-local sentence-end ".$$") ;; to be able to use M-a and M-e to jump
  (setq-local truncate-lines t))


(defun sensetion-annotate (lemma &optional pos)
  (interactive
   (list (read-string "Lemma to annotate: ")
         (ido-completing-read "PoS tag?" '("a" "r" "v" "n" "any") nil t nil nil "any")))
  ;; 
  (let* ((lemmas (if (equal pos "any")
                     (mapcar (apply-partially #'concat lemma) '("%1" "%2" "%3" "%4"))
                   (list (concat lemma "%" (sensetion--pos->synset-type pos)))))
         (matches (seq-mapcat (lambda (lemma) (gethash lemma sensetion--index)) lemmas))
         (result-buffer (generate-new-buffer (sensetion--create-buffer-name lemma pos))))
    (unless matches
      (if pos
          (user-error "No matches for lemma %s and PoS %s" lemma pos)
        (user-error "No matches for lemma %s as any PoS." lemma)))
    (sensetion--make-collocations matches result-buffer t)
    (with-current-buffer result-buffer
      (sensetion-mode))
    (pop-to-buffer result-buffer)))


(defun sensetion--create-buffer-name (lemma pos)
  (format "*%s@%s@%s*" sensetion-output-buffer-name (or pos "") lemma))


(defun sensetion--make-collocations (matches result-buffer show-meta?)
  (cl-labels
      ((get-sent (sent-id)
                 (with-temp-buffer
                   (insert-file-contents (sensetion--sent-id->filename sent-id))
                   (if-let ((id-str (format "sent_id = %s" sent-id))
                            ;; moves to sentence point
                            (found (search-forward id-str nil t))
                            ;; get point of where tokens begin
                            (tokens-point (progn (conllu-forward-to-token-line) (point)))
                            (sent (conllu--parse-sent-at-point)))
                       (list sent tokens-point)
                     (unless found (user-error "No sentence with %s" id-str)))))

       (token-colloc (sel-tks tk tk-index)
                     (unless (and (not show-meta?)
                                  (conllu--token-meta? tk))
                       (let* ((form-str  (conllu-token-form tk))
                              (selected? (cl-member
                                          (conllu--token-id->string (conllu-token-id tk))
                                          sel-tks :test #'equal))
                              (punct? (sensetion--punctuation? (conllu-token-form tk))))
                         (concat
                          (if punct? "" " ")
                          (if selected?
                              (propertize form-str
                                          'sensetion-token-index tk-index
                                          'sensetion-selected (conllu-token-form tk)
                                          'face `(:foreground ,sensetion-selected-token-colour-name))
                            (propertize form-str 'token-index tk-index))))))

       (sent-colloc (sent-id sent tk-id tokens-point)
                    (with-current-buffer result-buffer
                      (let ((tokens-line (seq-map-indexed
                                          (apply-partially #'token-colloc (list tk-id))
                                          (conllu-sent-tokens sent))))
                        (insert (apply #'concat tokens-line)
                                (propertize "\n" 'sensetion-tokens-point tokens-point 'sensetion-sent-id sent-id)
                                "\n"))))

       (go (match)
           (seq-let (sent-id tk-id) match
             (seq-let (sent tokens-point) (get-sent sent-id)
               (sent-colloc sent-id sent tk-id tokens-point)))))
    ;;
    (seq-do #'go matches)))


(defun sensetion-previous-selected (point)
  ;; TODO: debug this and next-selected
  (interactive (list (point)))
  (goto-char
   (previous-single-property-change point 'sensetion-selected nil (point-min))))


(defun sensetion-next-selected (point)
  (interactive (list (point)))
  (goto-char
   (next-single-property-change point 'sensetion-selected nil (point-max))))


(defun sensetion--selected? (point)
  (get-text-property point 'sensetion-selected))


(defun sensetion--token-index (point)
  (get-char-property point 'token-index))


(defun sensetion--go-to-token (token-index tokens-point fp)
  "Go to line in CONLLU-BUFFER corresponding to the token of
TOKEN-INDEX in sentence whose tokens start TOKENS-POINT."
  (find-file fp)
  (goto-char tokens-point)
  (forward-line token-index))


(defun sensetion--sent-id->filename (sent-id)
  (f-join sensetion-annotation-dir (concat sent-id ".conllu")))


(defun sensetion-go-to-token (token-index tokens-point filename)
  "Go to line in CoNLL-U buffer where token at point is located.

See `sensetion--go-to-token' for more details."
  (interactive
   (list (or (sensetion--token-index (point))
             ;; if not at selected token, move to the next one and
             ;; pick it
             (and (sensetion-next-selected (point))
                  (sensetion--token-index (1+ (point)))))
         (get-char-property (line-end-position) 'sensetion-tokens-point)
         (sensetion--sent-id->filename
          (get-char-property (line-end-position) 'sensetion-sent-id))))
  ;; 
  (unless (and token-index tokens-point filename)
    (user-error "No token at point."))
  (sensetion--go-to-token token-index tokens-point filename))


(defun sensetion--write-index (index-file index)
  "Write INDEX to INDEX-FILE."
  (with-temp-file index-file
    (prin1 index (current-buffer)))
  "Index written")


(defun sensetion--read-index (index-file)
  "Read index from INDEX-FILE, and set `sensetion--index'."
  (with-temp-buffer
    (insert-file-contents index-file)
    (goto-char (point-min))             ;is this needed?
    (setq sensetion--index (read (current-buffer))))
  "Index read")


(defun sensetion-make-index (files)
  "Read annotated files and build index of lemmas* and their
positions."
  (interactive
   (list (directory-files (or sensetion-annotation-dir
                              (read-file-name "Path to annotation directory: " nil nil t))
                          t
                          (concat "\\." sensetion-annotation-file-type "$"))))
  
  (let ((index-ht (sensetion--make-index files)))
    (setq sensetion--index index-ht))
  "Index made")


(defun sensetion--make-index (files)
  "Read annotated files and build hash-table associating lemmas*
to where in the files they appear."
  (let ((index-ht (make-hash-table :size 200000 :test #'equal)))
    (cl-labels
        ((run (id f)
              (let ((sent (conllu--string->sent (f-read-text f))))
                (index-sent id sent)))

         (index-sent (id sent)
                     (mapc (apply-partially #'index-token id)
                           (conllu-sent-tokens sent)))

         (index-token (sent-id tk)
                      (when (and
                             ;; is to be annotated?
                             (equal "un"
                                    (conllu-token-upos tk))
                             (or (equal "_" (conllu-token-form tk))
                                 (equal "_" (conllu-token-xpos tk))))
                        (mapc (lambda (lemma) (index-lemma sent-id
                                                           (conllu--token-id->string
                                                            (conllu-token-id tk))
                                                           lemma))
                              (s-split "|" (conllu-token-lemma tk)))))

         (index-lemma (sent-id tk-id lemma)
                      (when (equal "_" lemma)
                        (user-error "No lemma at %s %s" sent-id tk-id))
                      ;; lemmas* might be pure ("love") or have pos
                      ;; annotation ("love%2"), but we don't care
                      ;; about it here; when retrieving we gotta take
                      ;; care of this.
                      (setf (gethash lemma index-ht) (cons (list sent-id tk-id) (gethash lemma index-ht nil)))))
      ;; 
      (mapc (lambda (f) (run (file-name-base f) f)) files)
      index-ht)))


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
       
       (parse-sense (line)
                    (cl-assert (null (cl-rest line)))
                    ;; TODO: don't use regexp (will fail when gloss
                    ;; has " -- " as substring
                    (let* ((sep-regexp (regexp-opt '(" {" "} " " -- ")))
                           (fields     (s-split sep-regexp (cl-first line) t)))
                      (seq-let (_ offset words gloss) fields
                        (list (concat pos1 offset)
                              words
                              gloss)))))
    ;; 
    (let* ((command (format "wn '%s' -g -o -over" lemma))
           (result  (shell-command-to-string command))
           (chunks  (s-split "\nOverview of " result t))
           ;; easier to analyse first 3 characters
           (poses   (mapcar (lambda (c) (substring c 0 3)) chunks))
           (pos-ix  (seq-position poses (pos1->pos3 pos1)))
           (chunk   (elt chunks pos-ix))
           (senses  (s-match-strings-all "^[0-9].*$" chunk)))
      (mapcar #'parse-sense senses))))


(defun sensetion-edit (lemma*)
  ;; TODO lemma* might be tagged or not; if not, ask for it; maybe ask
  ;; for it nevertheless, to confirm choice
  (interactive (list (get-char-property (point) 'sensetion-selected)))
  (unless lemma* (user-error "No taggable token at point."))
  (seq-let (lemma synset-type) (s-split "%" lemma*)
    (let* ((pos1 (sensetion--synset-type->pos synset-type))
           (senses (sensetion--wordnet-lookup lemma pos1))
           (choice (...))))
    ;; TODO: add to conllu
    ;; TODO: update index
    ;; TODO: change token color or whatever
    (call-interactively
     (eval
      `(defhydra sensetion-edit-hydra ()
         (s-join "\n" (seq-map-indexed (lambda (sense ix) (format "_%s_") (cl-second sense) (cl-third sense))
                                       senses)))))))

(defhydra sensetion-edit-hydra ()
      "
^ ^  Navigate    |     Edit
----------------------------------------
_↑_: prev tok    |  _1_: ID     %s(conllu--field-string 0)
_↓_: next tok    |  _2_: FORM   %s(conllu--field-string 1)
_←_: prev snt    |  _3_: LEMMA  %s(conllu--field-string 2)
_→_: next snt    |  _4_: UPOS   %s(conllu--field-string 3)
^ ^              |  _5_: XPOS   %s(conllu--field-string 4)
_<_: next error  |  _6_: FEATS  %s(conllu--field-string 5)
_>_: prev error  |  _7_: HEAD   %s(conllu--field-string 6)
^ ^              |  _8_: DEPREL %s(conllu--field-string 7)
_q_: quit        |  _9_: DEPS   %s(conllu--field-string 8)
^ ^              |  _0_: MISC   %s(conllu--field-string 9)
%s(conllu--if-flycheck-error-string)"
      ;; nav commands
      ("<up>"    (move-tok  -1) nil)
      ("<down>"  (move-tok   1) nil)
      ("<left>"  (move-sent -1) nil)
      ("<right>" (move-sent  1) nil)
      ;; flycheck error navigation
      ("<" (if-flycheck-next-error -1) nil)
      (">" (if-flycheck-next-error  1) nil)
      ;; editing commands
      ("1" (move-and-edit-field  1) nil)
      ("2" (move-and-edit-field  2) nil)
      ("3" (move-and-edit-field  3) nil)
      ("4" (move-and-edit-field  4) nil)
      ("5" (move-and-edit-field  5) nil)
      ("6" (move-and-edit-field  6) nil)
      ("7" (move-and-edit-field  7) nil)
      ("8" (move-and-edit-field  8) nil)
      ("9" (move-and-edit-field  9) nil)
      ("0" (move-and-edit-field 10) nil)
      ;; exit
      ("q" nil nil))


(provide 'sensetion)

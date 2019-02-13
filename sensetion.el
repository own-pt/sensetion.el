;;; -*- lexical-binding: t; -*-
(require 'seq)
(require 's)
(require 'f)
(require 'cl-lib)
(require 'conllu-parse)


(defcustom sensetion-index-file
  (expand-file-name "~/.sensetion-index")
  "Path to index file.")


(defcustom sensetion-annotation-dir
  nil
  "Path to annotation directory")


(defcustom sensetion-annotation-file-type
  "conllu"
  "File type (extension) of annotation files.")


(defvar sensetion--index nil "Index.")


(defun sensetion-annotate (lemma &optional pos)
  (interactive
   (list (read-string "Lemma to annotate: ")
         (completing-read "PoS tag?" '("noun" "n"))))
  (let ((lemma (if pos
                   (mapcar (apply-partially #'concat lemma) '("%1" "%2" "%3" "%4"))
                 (list (concat lemma "%" pos)))))
    nil))



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


(defun sensetion-make-index (index-file files)
  "Read annotated files and build index of lemmas* and their
positions."
  (interactive
   (list sensetion-index-file
         (directory-files (or sensetion-annotation-dir
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


(provide 'sensetion)

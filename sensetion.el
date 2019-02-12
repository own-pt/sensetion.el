;;; -*- lexical-binding: t; -*-
(require 'seq)
(require 's)
(require 'cl-lib)
(require 'conllu-parse)


(defcustom sensetional-index-directory
  (expand-file-name "~/.sensetion/")
  "Directory where index files lie.")


(defun sensetional--synset-type->indexfile (st)
  (let ((st->if (cl-map 'vector
                        (lambda (f) (expand-file-name f sensetional-index-directory))
                        #("noun.index" "verb.index" "adjective.index" "adverb.index"))))
    (if st
        (aref st->if st)
      (expand-file-name "all.index" sensetional-index-directory))))


(defun sensetional-make-index ()
  (cl-labels
      ((index-file (id f)
                   (with-temp-buffer
                     (insert-file-contents f)
                     (forward-line 1)
                     (index-sentence sent-id
                                     (conllu--parse-sent-at-point))))

       (index-sentence (id sent)
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

       (index-lemma (sent-id tk-id lemma-str)
                    (when (equal "_" lemma-str)
                      (user-error "No lemma at %s %s" sent-id tk-id))
                    (seq-let (lemma key) (s-split "%" lemma-str)
                      (let ((line (format "%s %s-%s\n" lemma sent-id tk-id))
                            (index-file (sensetional--synset-type->indexfile key)))
                        (append-to-file line nil index-file)))))
    ;; 
    (let ((fs (directory-files sensetional-annotation-dir t "\\.conllu$")))
      (mapc (lambda (f) (index-file (file-name-base f) f)) fs))))


(provide 'sensetional)

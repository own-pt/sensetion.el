(ql:quickload :plump)
(ql:quickload :cl-conllu)
(ql:quickload :serapeum)
(ql:quickload :alexandria)


(defparameter *sense-map-ht* nil)

(defvar *posn->pos* (alexandria:alist-hash-table
                     '((#\1 . "n") (#\2 . "v") (#\3 . "a") (#\4 . "r") (#\5 . "s"))
                     :test #'eql))


(defun xml->conllus (fps out-fp sensemap-fp)
  (with-open-file (in-map sensemap-fp)
    (setq *sense-map-ht* (sense-map->ht in-map))
    (loop
      for fp in fps do
        (with-open-file (in fp)
          (let ((sents (wordnet-sentences (aref (plump:child-elements (plump:parse in)) 0))))
            (loop for s in sents do
              (with-open-file (out (make-pathname :name (cl-conllu:sentence-id s)
                                                  :type "conllu" :defaults out-fp)
                                   :direction :output :if-exists :supersede
                                   :if-does-not-exist :create)
                (cl-conllu:write-conllu-to-stream (list s) out))))))))


;; (defun conllus->xml (conllu-dir xmls)
;;   (loop for xml in xmls do
;;     (with-open-file (in xml)
;;       (let ((root (plump:parse in)))
;;         (loop for synset in (plump:child-elements (aref (plump:child-elements root) 0)) do
;;           (let* ((synset-elems (plump:child-elements synset))
;;                  (gloss-wsd-ix (position-if #'node-gloss-wsd? synset-elems))
;;                  (gloss-wsd (aref synset-elems gloss-wsd-ix)))
            
;;             ))))))


(defun sense-map->ht (in)
  (let ((map (make-hash-table :test #'equal :size 210000)))
    (loop for line = (read-line in nil 'eof)
          until (eq line 'eof)
          do (destructuring-bind (sk syid * **)
                 (serapeum:split-sequence #\space line)
               (let* ((posn (elt (elt (serapeum:split-sequence #\% sk) 1) 0))
                      (pos (gethash posn *posn->pos* "-")))
                 (setf (gethash sk map) (concatenate 'string pos syid)))))
    map))


(defun wordnet-sentences (node)
  (assert (equal (plump:tag-name node) "wordnet"))
  (map 'list #'gloss-sentence (plump:child-elements node)))


(defun gloss-sentence (node)
  (labels ((get-wsd-gloss (node)
             (filter-child-elements node #'node-gloss-wsd?)))
    ;; 
    (assert (equal (plump:tag-name node) "synset"))
    (make-instance 'cl-conllu:sentence
                   :meta (list (cons "sent_id" (or (plump:attribute node "id") (error "."))))
                   :tokens (gloss-tokens (first (get-wsd-gloss node))))))

(defun gloss-tokens (node)
  (labels
      ((run (node)
         (let* ((tks (mapcat-indexed #'make-token (mapcat #'expand-tokens
                                                          ;; wf, qf, mwf..
                                                          (plump:child-elements node))))
                (fst (first tks))
                (misc (cl-conllu:token-misc fst)))
           (setf (cl-conllu:token-misc fst) (format nil "~a|desc=~a" misc
                                                    (plump:tag-name node)))
           (cons fst (rest tks))))

       (expand-tokens (node)
         (serapeum:string-case (plump:tag-name node)
           (("cf" "wf") (list node))
           (("mwf" "qf") (mapcat #'expand-tokens (plump:child-elements node)))))

       (make-token (node ix)
         (serapeum:string-case (plump:tag-name node)
           ("wf"
            (list (make-conllu-token node (format nil "~a" ix))))
           ("cf"
            (cf-token node ix))))
       
       (make-conllu-token (node id)
         ;; senses are always direct children of either wf or glob,
         ;; which call this function
         (let ((senses (node-get-senses node)))
           (make-instance 'cl-conllu:token
                          :id id
                          :form (node-form node)
                          :lemma (node-lemma node)
                          :feats (when senses (format nil "s=~{~a~^,~}" senses))
                          :upostag (node-annotation-tag node)
                          :xpostag (node-coll node)
                          :misc (format nil "id=~a" (node-get-id node)))))

       (node-get-senses (node)
         (let ((ids (filter-child-elements node
                                           (lambda (n) (equal (plump:tag-name n) "id")))))
           (mapcar (lambda (s)
                     (let ((sk (plump:attribute s "sk")))
                       (gethash sk *sense-map-ht* "---")))
                   ids)))
       
       (cf-token (node ix)
         (let ((globs (filter-child-elements node
                                             (lambda (n)
                                               (equal (plump:tag-name n) "glob")))))
           (cons
            (make-conllu-token node (format nil "~a" ix))
            (loop for g in globs
                  for n from 0
                  collect (glob-token g ix n)))))

       (glob-token (node ix n)
         (make-conllu-token node (format nil "~a.~a" ix n))))
    ;; 
    (mapcat #'run
            ;; def, aux, ex, classif
            (plump:child-elements node))))


(defun node-gloss-wsd? (node)
  (and (equal (plump:tag-name node) "gloss")
       (equal (plump:attribute node "desc")
              "wsd")))


(defun node-form (node)
  (let ((form (plump:render-text (plump:strip node))))
    (if (equal form "")
        "_"
        form)))


(defun node-lemma (node)
  (or (plump:attribute node "lemma") "_"))


(defun node-annotation-tag (node)
  (plump:attribute node "tag"))


(defun node-coll (node)
  (or (plump:attribute node "coll") "_"))


(defun node-get-id (node)
  (second
   (serapeum:split-sequence #\_ 
                            (plump:attribute node "id")
                            :remove-empty-subseqs t)))


(defun filter-child-elements (node p)
  (loop for c across (plump:child-elements node)
        when (funcall p c)
          collect c))


(defun mapcat (f vec)
  (loop for x across vec
        append (funcall f x)))


(defun mapcat-indexed (f lst)
  (loop for x in lst
        for i from 0
        append (funcall f x i)))

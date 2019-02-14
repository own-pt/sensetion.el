(ql:quickload :plump)
(ql:quickload :cl-conllu)
(ql:quickload :serapeum)
(ql:quickload :alexandria)


(defparameter *sense-map-ht* nil)


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


(defun sense-map->ht (in)
  (let ((map (make-hash-table :test #'equal :size 210000)))
    (loop for line = (read-line in nil 'eof)
          until (eq line 'eof)
          do (destructuring-bind (sk syid * **)
                 (serapeum:split-sequence #\space line)
               (let* ((p (position #\% sk :test #'eql))
                      (posn (char sk (1+ p)))
                      (pos (posn->pos posn)))
                 (when (eql posn #\5)
                   ;; change posn of adjective satellites to
                   ;; adjectives because annotations are this way
                   (setf (char sk (1+ p)) #\3))
                 (setf (gethash sk map) (concatenate 'string pos syid)))))
    map))


(defun sense-key->synset-id (sk)
  (gethash sk *sense-map-ht* "|#|"))


(defun posn->pos (posn)
  (gethash posn
           (alexandria:alist-hash-table
            '((#\1 . "n") (#\2 . "v") (#\3 . "a") (#\4 . "r") (#\5 . "a"))
            :test #'eql)
           "#|#"))


(defun wordnet-sentences (node)
  (assert (equal (plump:tag-name node) "wordnet"))
  (map 'list #'gloss-sentence (plump:child-elements node)))


(defun gloss-sentence (node)
  (labels ((get-wsd-gloss (node)
             (filter-child-elements node #'node-gloss-wsd?)))
    ;; 
    (assert (equal (plump:tag-name node) "synset"))
    (make-instance 'cl-conllu:sentence
                   :meta (list (cons "sent_id" (or (plump:attribute node "id")
                                                   (error "."))))
                   :tokens (gloss-tokens (first (get-wsd-gloss node))))))

(defun gloss-tokens (node)
  (labels
      ((run (nodes)
         (let* ((tks (mapcat-indexed #'make-token (mapcat #'expand-tokens
                                                          ;; wf, qf, mwf..
                                                          nodes)))
                (fst (first tks))
                (misc (cl-conllu:token-misc fst)))
           (setf (cl-conllu:token-misc fst) (add-to-string-list misc "desc"
                                                                (plump:tag-name node)))
           (cons fst (rest tks))))

       (expand-tokens (node)
         (let ((tag (plump:tag-name node)))
           (serapeum:string-case tag
             (("cf" "wf")
              (list (list node)))
             ("qf"
              (annotate "kind" (plump:attribute node "rend")
                        (mapcat #'expand-tokens
                                (plump:child-elements node))))
             ;; TODO: add identifier to each  mwf and qf (or we won't know
             ;; when they overlap
             ("mwf"
              (annotate "kind" tag
                        (mapcat #'expand-tokens
                                (plump:child-elements node))))
             (("aux" "classif" "def" "ex")
              (annotate "elem" tag
                        (mapcat #'expand-tokens
                                (plump:child-elements node)))))))

       (annotate (key val nodes)
         (mapcar (lambda (n)
                   (destructuring-bind (n . ps) n
                     (cons n (cons (list key val) ps))))
                 nodes))

       (add-to-string-list (orig k v)
         (cond
           ((and orig k v)
            (format nil "~a=~a|~a" k v orig))
           (orig
            orig)
           (t
            (format nil "~a=~a" k v))))

       (make-token (node ix)
         (destructuring-bind (node . ps) node
           (serapeum:string-case (plump:tag-name node)
             ("wf"
              (list (make-conllu-token node (format nil "~a" ix) ps)))
             ("cf"
              (cf-token node ix ps)))))
       
       (make-conllu-token (node id ps)
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
                          :misc (reduce (lambda (res p)
                                          (add-to-string-list res
                                                              (first p)
                                                              (second p)))
                                        ps
                                        :initial-value (format nil "~a=~a" "id"
                                                               (node-get-id node))))))

       (node-get-senses (node)
         (let ((ids (filter-child-elements
                     node
                     (lambda (n) (and
                                  (equal (plump:tag-name n) "id")
                                  ;; someone had the brilliant idea of
                                  ;; including this false sense key
                                  (not (equal
                                        (plump:attribute n "sk")
                                        "purposefully_ignored%0:00:00::")))))))
           (mapcar (lambda (s)
                     (let ((sk (plump:attribute s "sk")))
                       (sense-key->synset-id sk)))
                   ids)))
       
       (cf-token (node ix ps)
         (let ((globs (filter-child-elements node
                                             (lambda (n)
                                               (equal (plump:tag-name n) "glob")))))
           (cons
            (make-conllu-token node (format nil "~a" ix) ps)
            (loop for g in globs
                  for n from 0
                  collect (glob-token g ix n)))))

       (glob-token (node ix n)
         (make-conllu-token node (format nil "~a.~a" ix n) nil)))
    ;; 
    (run ;; def, aux, ex, classif
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

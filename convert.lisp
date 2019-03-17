(ql:quickload :plump)
(ql:quickload :plump-sexp)
(ql:quickload :serapeum)
(ql:quickload :alexandria)
(ql:quickload :ironclad)
(ql:quickload :cxml)

(defparameter *sense-map-ht* nil)


(defmacro with-open-files (args &body body)
  (case (length args)
    ((0)
     `(progn ,@body))
    ((1)
     `(with-open-file ,(first args) ,@body))
    (t `(with-open-file ,(first args)
	  (with-open-files
	      ,(rest args) ,@body)))))



(defun mapcat (f vec)
  (loop for x across vec
        append (funcall f x)))


(defun node-get-id (node)
  (second (serapeum:split-sequence #\_
				   (plump:attribute node "id")
				   :remove-empty-subseqs t)))

(defun node-gloss-wsd? (node)
  (and (equal (plump:tag-name node) "gloss")
       (equal (plump:attribute node "desc")
              "wsd")))


(defun node-form (node)
  (let ((form (plump:render-text (plump:strip node))))
    (if (equal form "")
        nil
        form)))


(defun meta->tk (node)
  (plump:children
   (plump-sexp:parse
    `((meta :tag "ignore" :id ,(concatenate 'string "id_" (plump:tag-name node))) ""))))


(defun qf->wf (node)
  (plump:children
   (plump-sexp:parse
    `((meta :tag "ignore")
      ,(serapeum:string-case (plump:attribute node "rend")
         ("dq" "\"")
         ("sq" "'")
         (t (error "attribute rend of ~S must be \"dq\" or \"sq\"" node)))))))


(defun node-lemma (node)
  (plump:attribute node "lemma"))


(defun node-annotation-tag (node)
  (plump:attribute node "tag"))


(defun node-coll (node)
  (let ((coll-keys (plump:attribute node "coll")))
    (when coll-keys
      (serapeum:split-sequence #\,
                               coll-keys
                               :remove-empty-subseqs t))))


(defun node-pos (node)
  (plump:attribute node "pos"))


(defun posn->pos (posn)
  (gethash posn
           (alexandria:alist-hash-table
            '((#\1 . "n") (#\2 . "v") (#\3 . "a") (#\4 . "r") (#\5 . "a"))
            :test #'eql)
           "#|#"))


(defun sense-map->ht (in)
  "IN is the stream for a txt file mapping sense keys to synset-ids."
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
  (or
   (gethash sk *sense-map-ht*)
   (error "sense key ~S does not exist" sk)))


(defun filter-child-elements (node p)
  (loop for c across (plump:child-elements node)
        when (funcall p c)
        collect c))


(defun gloss-text (node)
  (let* ((text-node (first
                     (filter-child-elements node
                                            (lambda (n)
                                              (and (equal (plump:tag-name n) "gloss")
                                                   (equal (plump:attribute n "desc")
                                                          "orig")))))))
    (plump:render-text text-node)))


(defun gloss-terms (node)
  (let* ((terms-node (first
                      (filter-child-elements node
                                             (lambda (n) (equal (plump:tag-name n) "terms")))))
         (terms (filter-child-elements terms-node
                                       (lambda (n) (equal (plump:tag-name n) "term")))))
    (mapcar #'plump:render-text terms)))


(defun gloss-tokens (node)
  (labels
      ((run (nodes)
         (mapcan #'make-token
                 (mapcat #'expand-tokens
                         ;; wf, qf, mwf..
                         nodes)))

       (expand-tokens (node)
         (let ((tag (plump:tag-name node)))
           (serapeum:string-case tag
             (("cf" "wf" "meta")
              (list node))
             ("qf"
              (let ((qf (qf->wf node)))
                (mapcat #'expand-tokens
                        (concatenate 'vector
                                     qf
                                     (plump:child-elements node)
                                     qf))))
             (("mwf" "aux" "classif" "def" "ex")
              (let ((meta-tk (meta->tk node)))
                (mapcat #'expand-tokens
                        (concatenate 'vector
                                     meta-tk
                                     (plump:child-elements node)
                                     meta-tk)))))))

       (make-token (node)
         (serapeum:string-case
          (plump:tag-name node)
          ("wf"
           (list (make-token-plist node :wf)))
          ("cf"
           (cf-token node))
          ("meta"
           (list (make-token-plist node :meta)))))

       (make-token-plist (node kind)
         ;; senses are always direct children of either wf or glob,
         ;; which call this function
         (let ((senses (node-get-senses node)))
           (list :form (node-form node)
                 :lemma (node-lemma node)
                 :pos (node-pos node)
                 :status (alexandria:if-let ((st (node-annotation-tag node)))
                           (or (serapeum:string-case st
                                 (("auto" "man")
                                  (assert senses)
                                  (when (eq senses 'nosense)
                                    (concatenate 'string st "-nosense")))
                                 (("ignore" "un")
                                  (assert (null senses))))
                               st))
                 :kind (let ((coll-keys (node-coll node)))
                         (case kind
                           (:coll
                            (assert (null senses)
                                    (senses)
                                    "when token ~S is part of a collocation, it must have no annotation (only its glob may be annotated"
                                    node)
                            (cons kind coll-keys))
                           (:glob
                            (assert (null (cdr coll-keys))
                                    (coll-keys)
                                    "when token ~S is a glob it must have be part of only one colloc"
                                    node)
                            (cons kind (first coll-keys)))
                           (otherwise kind)))
                 :anno senses
                 :meta (let ((id (node-get-id node))) (and id (list (list :id id))))
                 :conf (if (eq senses 'nosense) 0 1))))

       (node-get-senses (node)
         (let* ((ids (filter-child-elements
                      node
                      (lambda (n) (equal (plump:tag-name n) "id"))))
                (ignored? (some (lambda (n) (equal
                                             (plump:attribute n "sk")
                                             "purposefully_ignored%0:00:00::"))
                                ids)))
           (if ignored?
               (progn
                 (assert (member (node-annotation-tag node) '("man" "auto")
                                 :test #'equal)
                         (node)
                         "~S is purposefully ignored, so should have tag \"man\" or \"auto\".")
                 (assert (null (cdr ids)) (ids) "~S must be a singleton if there's a purposefully ignored token." ids)
                 'nosense)
               (mapcar (lambda (s)
                         (let ((sk (plump:attribute s "sk")))
                           (sense-key->synset-id sk)))
                       ids))))

       (cf-token (node)
         (let ((globs (filter-child-elements node
                                             (lambda (n)
                                               (equal (plump:tag-name n) "glob")))))
           (append
            (loop for g in globs
                  collect (glob-token g))
            (list (make-token-plist node :coll)))))

       (glob-token (node)
         (make-token-plist node :glob)))
    ;;
    (run ;; def, aux, ex, classif
     (plump:child-elements node))))


(defun gloss-sentence (node)
  (labels ((get-wsd-gloss (node)
             (filter-child-elements node #'node-gloss-wsd?)))
    ;;
    (assert (equal (plump:tag-name node) "synset"))
    (list :id (or (plump:attribute node "id") (error "."))
          :terms (gloss-terms node)
          :text (gloss-text node)
          :tokens (gloss-tokens (first (get-wsd-gloss node))))))


;; (defun wordnet-sentences (node)
;;   (assert (equal (plump:tag-name node) "wordnet"))
;;   (map 'list #'gloss-sentence (plump:child-elements node)))


(defun checksum (str)
  (format nil "~a"
          (mod
           (parse-integer
            (ironclad:byte-array-to-hex-string
             (ironclad:digest-sequence 'ironclad:sha224 (string-to-octets str))) :start 50 :radix 16)
           1000)))


(defun save-sent (s out-fp)
  (with-open-file (out (make-pathname :name (checksum (getf s :id))
                                      :type "plist" :defaults out-fp)
                       :direction :output :if-exists :append
                       :if-does-not-exist :create)
    (write s :pretty nil :case :downcase :stream out)
    (terpri out)))


(defun main (glosstag-fp out-fp sensemap-fp &key (*sense-map-ht* *sense-map-ht*))
  (let ((out-fp (ensure-directories-exist out-fp)))
    (with-open-file (in-map sensemap-fp)
      (let ((*sense-map-ht* (sense-map->ht in-map))
	    (in-files (directory (make-pathname :defaults glosstag-fp :name :wild :type "xml"))))
        (format t "Input Files: ~a~%Output Directory: ~a~%Sense Index: ~a~%"
                in-files
                out-fp
                sensemap-fp)
        (loop for fp in in-files
	      do (klacks:with-open-source (in (cxml:make-source fp))
                   (labels ((f (s-xml)
                              (save-sent (gloss-sentence
                                          (aref (plump:child-elements (plump:parse s-xml)) 0))
                                         out-fp)))
                     (loop while (klacks:find-element in "synset")
                           do (f (klacks:serialize-element in (cxml:make-string-sink)))))))))))



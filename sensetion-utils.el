;;; -*- lexical-binding: t; -*-

(defun sensetion--punctuation? (str)
  (gethash
   str
   #s(hash-table size 45 test equal rehash-size 1.5 rehash-threshold 0.8125
                 purecopy t data
                 ("." t "," t ":" t "!" t "?" t "'" t "]" t ")" t "..." t "»" t))))


(defun sensetion--beginning-of-buffer ()
  (goto-char (point-min)))


(defun sensetion--map-lines (file f)
  "Apply F to each line of FILE.

F must take as argument the point where the line begins and the
line string itself."
  (with-temp-buffer
    (insert-file-contents file)
    (let (res)
      (while (not (eobp))
        (setf res
              (cons 
               (funcall f (point) (thing-at-point 'line t))
               res))
        (forward-line 1))
      res)))


(defmacro with-inhibiting-read-only (&rest body)
  `(let ((inhibit-read-only t))
     ,@body))


(defmacro sensetion-is (&rest body)
  (seq-let (body wclauses)
      (-split-when (lambda (c) (eq 'where c)) body)
    (let ((body
           (-reduce-from
            (lambda (bd cl)
              (pcase cl
                (`(,var ,val)
                 `((let ((,var ,val))
                     ,@bd)))
                (`(,name ,arglist . ,body)
                 `((cl-labels ((,name ,arglist ,@body))
                     ,@bd)))))
            body
            wclauses)))
      (cl-first body))))

;; (defmacro defun/where (name arglist &rest body)
;;   "
;; \(fn NAME ARGLIST &optional DOCSTRING DECL &rest BODY)"
;;   (declare (doc-string 3) (indent 2) (debug defun))
;;   `(defun ,name ,arglist
;;      (sensetion-is
;;       ,@body)))

(provide 'sensetion-utils)

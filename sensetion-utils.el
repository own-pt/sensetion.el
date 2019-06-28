;;; sensetion.el --- -*- lexical-binding: t; -*-

(defun sensetion--beginning-of-buffer ()
  (goto-char (point-min)))


(defun sensetion--map-buffer-lines (f &optional buffer)
  "Apply F to each line of BUFFER.

BUFFER defaults to current buffer if not provided. F must take as
argument the line number and the line string itself."
  (with-current-buffer (or buffer (current-buffer))
    (save-excursion
      (sensetion--beginning-of-buffer)
      (let (res
            (counter 0))
        (while (not (eobp))
          (when-let ((line-res (funcall f counter (thing-at-point 'line))))
            (setf res (cons line-res res)))
          (cl-incf counter)
          (forward-line 1))
        res))))


(defun sensetion--map-file-lines (file f)
  "Apply F to each line of FILE.

F must take as argument the line number and the line string
itself."
  (with-temp-buffer
    (insert-file-contents file)
    (sensetion--map-buffer-lines f)))


(defmacro with-inhibiting-read-only (&rest body)
  `(let ((inhibit-read-only t))
     ,@body))


(defmacro sensetion-is (&rest body)
  (declare (debug ([&rest [&not ":where"] form]
			 &optional
			 gate ":where"
			       &rest (&define name def-form)
			       (&define name lambda-list def-body))))
  (seq-let (body wclauses)
      (-split-when (lambda (c) (eq :where c)) body)
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


(defun sensetion--goto-line (line &optional start-line)
  (unless start-line
    (sensetion--beginning-of-buffer))
  (let ((sl (or start-line 0)))
    (forward-line (- line sl))))


(defun sensetion--spaces->underlines (str)
  (cl-substitute (string-to-char "_")
                 (string-to-char " ")
                 str))

(provide 'sensetion-utils)

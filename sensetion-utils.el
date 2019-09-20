;;; sensetion.el --- -*- lexical-binding: t; -*-

(require 'cl-lib)

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


(defmacro sensetion--with-inhibiting-read-only (&rest body)
  (declare (debug t))
  `(let ((inhibit-read-only t))
     ,@body))

(defalias 'sensetion-λ #'pcase-lambda)


(defmacro sensetion-is (&rest body)
  (declare (debug ([&rest [&not ":where"] form]
			 &optional
			 gate ":where"
			       &rest &or (&define name def-form)
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


(defun sensetion--goto-line (line &optional start-line)
  (unless start-line
    (sensetion--beginning-of-buffer))
  (let ((sl (or start-line 0)))
    (forward-line (- line sl))))


(defun sensetion--spaces->underlines (str)
  (cl-substitute ?_
                 (string-to-char " ")
                 str
		 :test #'eq))


(defsubst sensetion--put-text-property-eol (property value &optional object)
  (put-text-property (line-end-position) (1+ (line-end-position))
		     property value object))


(defsubst sensetion--get-text-property-eol (property &optional object)
  (get-text-property (line-end-position) property object))


(defun sensetion--remove-nth (n list)
  (cl-remove-if (lambda (_) t) list :start n :end (1+ n)))


(defun sensetion--parse-jsonlines (&optional reverse)
  "Parse and return in a list the newline-delimited JSON values following point.
Advances point until end-of-buffer.
If REVERSE is non-nil, return the results in the proper order."
  (let ((res))
    (while (not (eobp))
      (push (json-read) res)
      ;; skip whitespace
      (skip-chars-forward "[:space:]"))
    (if reverse
	(reverse res)
      res)))


(provide 'sensetion-utils)

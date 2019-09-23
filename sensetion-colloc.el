;;; sensetion.el --- -*- lexical-binding: t; -*-

(defun sensetion--sent-colloc (sent &optional target target-senses)
  "Return propertized string with SENT contents and its statistics."
  (sensetion-is
   (list
    (concat
     (when (sensetion--project-display-meta-data-fn sensetion-current-project)
       (funcall (sensetion--project-display-meta-data-fn sensetion-current-project) sent target))
     (s-join " " tks))
    (cons done total))
   :where
   (tks (cl-loop for tk in (sensetion--sent-tokens sent) for ix from 0 append (go-tk tk ix)))
   (go-tk (tk ix)
	  (pcase tk
	    ((cl-struct sensetion--tk kind form tag senses unsure)
	     (when (select? tk)
	       (cl-incf total)
	       (when (sensetion--tk-annotated? tk)
		 (cl-incf done)))
	     (let ((kind  (car kind))
		   (ckeys (cdr kind)))
	       (pcase ckeys
		 (`(,k)			;part of exactly one glob
		  (seq-let (glob-ix glob-tk) (map-elt globs k nil #'equal)
		    (sensetion--tk-colloc ix form kind ckeys
				 (select? glob-tk)
				 (sensetion--tk-tag glob-tk)
				 (sensetion--tk-senses-pos glob-tk)
				 (map-senses (sensetion--tk-senses glob-tk))
				 (sensetion--tk-unsure glob-tk)
				 glob-ix)))
		 (`() 			;part of no glob
		  (sensetion--tk-colloc ix form kind nil (select? tk) tag
			       (sensetion--tk-senses-pos tk)
			       (map-senses senses)
			       unsure nil))
		 (_ 			;part of more than one glob
		  (sensetion--tk-colloc ix form kind ckeys nil)))))))
   (map-senses (tk-sks)
	       (when target (mapcar (lambda (sk)
				      (cl-first
				       (map-elt target-senses sk
						nil #'equal)))
				    tk-sks)))
   (select? (tk)
	    (if target
		(sensetion--to-annotate? tk target)
	      (sensetion--tk-annotatable? tk)))
   (globs (sensetion--collect-globs sent))
   (done 0)
   (total 0)))


(defun sensetion--collect-globs (sent)
  (cl-loop
   for tk in (sensetion--sent-tokens sent)
   for ix from 0
   append
   (let ((k (sensetion--tk-glob? tk)))
     (when k
       (list (list k ix tk))))))


(defun sensetion--tk-colloc (ix form kind ckeys selected?
			   &optional tag pos senses unsure? glob-ix)
  (sensetion-is
   (pcase kind
     ((or "wf" "cf")
      (sensetion-is
       (list (apply #'concat pre pform post))
       :where
       (pre (sensetion--tk-glob-keys-colloc ckeys))
       (post (when selected?
	       (list (sensetion--tk-pos-colloc pos)
		     (sensetion--tk-senses-colloc senses))))
       (pform (apply #'propertize form 'sensetion--tk-ix ix props))
       (props (append (when glob-ix (list 'sensetion--glob-ix glob-ix))
		      (sensetion--tk-status-props selected? tag unsure?))))))))


(defun sensetion--tk-glob-keys-colloc (keys)
  (propertize (s-join "," keys)
              'display '(raise -0.3)
              'invisible 'sensetion--scripts
              'face '(:height 0.6)))


(defun sensetion--tk-status-props (selected? tag unsure?)
  (when selected?
    (list 'sensetion--selected selected?
	  'face `(:foreground
		  ,(pcase tag
		     ("man-now"
		      (if unsure?
			  sensetion-currently-annotated-unsure-colour
			sensetion-currently-annotated-colour))
		     ("un"
		      sensetion-unnanotated-colour)
		     ((or "auto" "man")
		      (if unsure?
			  sensetion-previously-annotated-unsure-colour
			sensetion-previously-annotated-colour))
		     (_ (error "Unrecognized tag %s" tag)))))))


(defun sensetion--tk-pos-colloc (pos)
  (propertize (or (sensetion--pos->string pos) "")
              'display '(raise 0.4)
              'invisible 'sensetion--scripts
              'face '(:height 0.6)))


(defun sensetion--tk-senses-colloc (senses)
  (propertize (s-join "," senses)
              'display '(raise 0.4)
              'invisible 'sensetion--scripts
              'face '(:height 0.6)))

(provide 'sensetion-colloc)

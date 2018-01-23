;;;; -*- Mode:Lisp; Package:Macsyma; -*-

(defun $listarray (ary)
 `((mlist) .
      ,(cond ((mget ary 'hashar)
	      (mapcar (function (lambda (subs) ($arrayapply ary subs)))
		      (cdddr (apply '$arrayinfo (list ary)))))
	     ((mget ary 'array) (listarray (mget ary 'array)))
	     (t (displa ary)
		(error "arg to listarray must be an array")))))

(defun $fillarray (ary1 ary2)
  ((lambda (ary)
       (setq ary (cond ((mget ary1 'array))
		       (t (displa ary1)
			  (error "first arg to fillarray must be a declared array"))))
       (fillarray ary
		  (cond (($listp ary2) (cdr ary2))
			((mget ary2 'array))
			(t (displa ary2)
			   (error
			    "second arg to fillarray must be an array or list"))))
       ary1)
   nil))

(defun $rearray fexpr (l)
       (cond ((> (length l) 6.) (displa l) (error "too many args to rearray"))
	     ((< (length l) 2.) (displa l) (error "too few args to rearray")))
       ((lambda (ary)
		(setq l (cdr l)
		      l (mapcar (function
				 (lambda (x)
					 (setq x (meval x))
					 (cond ((not (fixp x))
						(displa x)
						(error
						 "non integer dimension to rearray")))
					 (1+ x)))
				l))
		(apply '*rearray (cons ary (cons (car (arraydims ary)) l))))
	(cond ((mget (car l) 'array))
	      (t (displa (car l))
		 (error "first arg to rearray must be a declared array")))))

(defun $arrayapply (ary subs) (meval ($arraymake ary subs)))

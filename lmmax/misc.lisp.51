;; -*- Mode: Lisp; Package: Macsyma -*-

;;  Maclisp compatibility package for the Lisp Machine -- run time

;;  This function should really bash the array or use an invisible pointer
;;  to be compatible with maclisp.  ARRAY-SYMBOL can be either an array object
;;  or a symbol.  This only works for one dimensional arrays right now.
;;  IGNORE is normally the type, but Maclisp only has ART-Q arrays.
;;  *REARRAY of one arg is supposed to return the array.
;;  Rewrite at some point to use ADJUST-ARRAY-SIZE.

(DEFUN *REARRAY (ARRAY-SYMBOL &OPTIONAL IGNORE &REST DIMS)
  (CHECK-ARG ARRAY-SYMBOL
	     (OR (SYMBOLP ARRAY-SYMBOL) (ARRAYP ARRAY-SYMBOL))
	     "a symbol or an array")
  (COND ((NULL DIMS))
	((NULL (CDR DIMS))
	 (LET ((OLD-ARRAY (IF (SYMBOLP ARRAY-SYMBOL)
			      (FSYMEVAL ARRAY-SYMBOL) ARRAY-SYMBOL))
	       (NEW-ARRAY (MAKE-ARRAY NIL 'ART-Q (CAR DIMS)))
	       (MIN-ARRAY-LENGTH))
	   (SETQ MIN-ARRAY-LENGTH (MIN (ARRAY-DIMENSION-N 1 OLD-ARRAY)
				       (ARRAY-DIMENSION-N 1 NEW-ARRAY)))
	   (DO I 0 (1+ I) (= I MIN-ARRAY-LENGTH)
	       (ASET (AREF OLD-ARRAY I) NEW-ARRAY I))
	   (IF (SYMBOLP ARRAY-SYMBOL) (FSET ARRAY-SYMBOL NEW-ARRAY))
	   NEW-ARRAY))
	(T (FERROR NIL "Can't handle *REARRAY with more than one dimension"))))

(DEFUN RUNTIME NIL (TIME))


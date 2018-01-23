;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp; Package: Macsyma -*- ;;;;;;;;;;;;;;;;;;;
;;;     (c) Copyright 1980 Massachusetts Institute of Technology         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(macsyma-module ratmac macro)

;; Macros for manipulating rational functions.

(DEFMACRO PCOEFP (E) `(ATOM ,E))
(DEFMACRO PZEROP (X) `(SIGNP E ,X))			;TRUE FOR 0 OR 0.0
(DEFMACRO PZERO () 0)

(DEFMACRO PTZEROP (TERMS) `(NULL ,TERMS))		;for poly terms
(DEFMACRO PTZERO () '())

(DEFMACRO CZEROP (C) `(SIGNP E ,C))
(DEFMACRO CMINUS (C) `(MINUS ,C))
(DEFMACRO CMINUSP (C) `(MINUSP ,C))
(DEFMACRO CDERIVATIVE (NIL NIL) 0)

;; Similar to REMOVE on the Lisp Machine
(DEFMACRO DELET (ITEM LIST) `(DELETE ,ITEM (APPEND ,LIST NIL)))

(DEFMACRO VALGET (ITEM) `(SYMEVAL ,ITEM))

(DEFUN ALGV MACRO (L) `(AND $ALGEBRAIC (GET ,(CADR L) 'TELLRAT)))

(DEFMACRO EQN (&REST L) `(EQUAL . ,L))

(DEFMACRO RZERO () ''(0 . 1))
(DEFMACRO RZEROP (A) `(PZEROP (CAR ,A)))

(defmacro PRIMPART (p) `(cadr (oldcontent ,p)))

;;poly constructor

(defmacro make-poly (var &optional (terms-or-e nil options?) (c nil e-c?)
			 (terms nil terms?))
  (cond ((null options?) `(cons ,var '(1 1)))
	((null e-c?) `(psimp ,var ,terms-or-e))
	((null terms?) `(list ,var ,terms-or-e ,c))
	(t `(psimp ,var (list* ,terms-or-e ,c ,terms)))))

;;Poly selector functions

(defmacro P-VAR (p) `(car ,p))

(defmacro P-TERMS (p) `(cdr ,p))

(defmacro P-LC (p) `(caddr ,p))			;leading coefficient

(defmacro P-LE (p) `(cadr ,p))

(defmacro P-RED (p) `(cdddr ,p))

;;poly terms selectors

(defmacro PT-LC (terms) `(cadr ,terms))

(defmacro PT-LE (terms) `(car ,terms))

(defmacro PT-RED (terms) `(cddr ,terms))

;; Taken from SININT and RISCH.  Somebody document these please.

(DEFMACRO R+ (R . L)
	  (COND ((NULL L) R)
		(T `(RATPL ,R (R+ ,@L)))))

(DEFMACRO R* (R . L)
	  (COND ((NULL L) R)
		(T `(RATTI ,R (R* ,@L) T))))

(DEFMACRO R- (R . L)
	  (COND ((NULL L) `(RATMINUS (RATFIX ,R)))
		(T `(RATDIF (RATFIX ,R) (R+ ,@L)))))


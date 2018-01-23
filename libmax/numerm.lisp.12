;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp; Package: Macsyma -*- ;;;;;;;;;;;;;;;;;;;
;;;     (c) Copyright 1980 Massachusetts Institute of Technology         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(macsyma-module numerm macro)

;;; Macros for interface of lisp numerical routines to macsyma,
;;; for use with the functions in Maxsrc;Numer.

(defmacro make-array$ (&rest l)
  #+Maclisp
  `(*array nil 'flonum ,@l)
  #+LISPM
  `(make-array (list ,@l) ':type 'art-q)
  )


(defmacro make-array% (&rest l)
  #+Maclisp
  `(*array nil 'fixnum ,@l)
  #+Lispm
  `(make-array (list ,@l) ':type 'art-q)
  )

(defmacro aref$ (&rest l)
  #+Maclisp
  `(arraycall flonum ,@l)
  #+Lispm
  `(aref ,@l)
  )

(defmacro aref% (&rest l)
  #+Maclisp
  `(arraycall fixnum ,@l)
  #+Lispm
  `(aref ,@l)
  )

(defmacro free-array% (a)
  #+Maclisp
  `(*rearray ,a)
  #+Lispm
  ;; not useful to call return-array unless it is at end of area.
  ;; programs do better to save arrays as a resource, this works
  ;; in maclisp too.
  a
  )
(defmacro free-array$ (a)
  #+maclisp
  `(*rearray ,a)
  #+Lispm
  a
  )

(DEFMACRO DEFBINDTRAMP$ (NARGS)
  (LET ((BIND-TRAMP$ (SYMBOLCONC 'BIND-TRAMP NARGS '$))
	(TRAMP$ (SYMBOLCONC 'TRAMP NARGS '$)))
    `(PROGN 'COMPILE
	    (IF (FBOUNDP 'SPECIAL) (SPECIAL ,TRAMP$))
	    (DEFMACRO ,BIND-TRAMP$ (F G &REST BODY)
	      `(LET ((,',TRAMP$))
		 (LET ((,F (MAKE-TRAMP$ ,G ,',NARGS)))
		   ,@BODY))))))

(DEFBINDTRAMP$ 1)
(DEFBINDTRAMP$ 2)
(DEFBINDTRAMP$ 3)

(defmacro fcall$ (&rest l)
  #+Maclisp
  `(subrcall flonum ,@l)
  #+Lispm
  `(funcall ,@l)
  )

;; Central location for some important declarations.
#+Maclisp
(IF (FBOUNDP 'FLONUM)
    (FLONUM (GCALL1$ NIL NIL)
	    (GCALL2$ NIL NIL NIL)
	    (MTO-FLOAT NIL)
	    ))



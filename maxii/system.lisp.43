;;; -*- Mode: LISP; Package: Macsyma; Ibase: 10. -*-
;;;
;;; ** (c) Copyright 1981 Massachusetts Institute of Technology **
;;;
;;; SYSTEM: The ``New'' Macsyma System Stuff

(macsyma-module system)


(eval-when (eval compile load) (sstatus feature maxii))

;;; Standard Kinds of Input Prompts

(DEFUN MAIN-PROMPT ()
  ;; instead off using this STRIPDOLLAR hackery, the
  ;; MREAD function should call MFORMAT to print the prompt,
  ;; and take a format string and format arguments.
  ;; Even easier and more general is for MREAD to take
  ;; a FUNARG as the prompt. -gjc
  (FORMAT () "(~A~D) " (STRIPDOLLAR $INCHAR) $LINENUM))

(DEFUN BREAK-PROMPT ()
  (STRIPDOLLAR $PROMPT))



;; there is absoletely no need to catch errors here, because
;; they are caught by the macsyma-listener window process on
;; the lisp machine, or by setting the single toplevel process in Maclisp. -gjc

(defmacro toplevel-macsyma-eval (x) `(meval* ,x))

(defmvar $_ '$_ "last thing read in, cooresponds to lisp +")
(defmvar $% '$% "last thing printed out, cooresponds to lisp *")
(defmvar $__ '$__ "thing read in which will be evaluated, cooresponds to -")

(declare (special *mread-prompt*))

(defvar accumulated-time 0.0)

(DEFUN CONTINUE (&OPTIONAL (STANDARD-INPUT STANDARD-INPUT) BATCH-OR-DEMO-FLAG)
  (DO ((R)
       (time-before) (time-after) (time-used)
       (EOF (LIST NIL)))
      (NIL)
    (LET (((C-TAG D-TAG)
	   (MAKE-LINE-LABELS $INCHAR $OUTCHAR)))
      (SETQ R (LET ((*MREAD-PROMPT* (MAIN-PROMPT)))
		(AND BATCH-OR-DEMO-FLAG
		     (FORMAT STANDARD-OUTPUT "~%~A" *MREAD-PROMPT*))
		(MREAD STANDARD-INPUT EOF)))
      (IF (EQ R EOF) (RETURN '$DONE))
      (TERPRI STANDARD-INPUT)
      (FUNCALL STANDARD-INPUT ':FORCE-OUTPUT)
      (SETQ $__ (CADDR R))
      (SET  C-TAG $__)
      (setq time-before (time))
      (SETQ $% (TOPLEVEL-MACSYMA-EVAL $__))
      (setq time-after (time))
      (setq time-used (quotient (time-difference time-after time-before)
				60.0))
      (setq accumulated-time (plus accumulated-time time-used))
      (SET  D-TAG $%)
      (SETQ $_ $__)
      (if $showtime
	  (mtell "Evaluation took ~S seconds." time-used))
      (IF (EQ (CAAR R) 'DISPLAYINPUT)
	  (DISPLA `((MLABLE) ,D-TAG ,$%)))
      (COND ((EQ BATCH-OR-DEMO-FLAG ':DEMO)
	     (MTELL "~&Pausing.  Type any character to continue demo.~%")
	     (IF (AND (= (FUNCALL STANDARD-OUTPUT ':TYI) #\CLEAR-SCREEN)
		      (MEMQ ':CLEAR-SCREEN (FUNCALL STANDARD-OUTPUT ':WHICH-OPERATIONS)))
		 (FUNCALL STANDARD-OUTPUT ':CLEAR-SCREEN)
		 (TERPRI STANDARD-OUTPUT))))
      ;; This is sort of a kludge -- eat newlines and blanks so that they don't echo
      (AND BATCH-OR-DEMO-FLAG
	   (MEMQ ':TYI-NO-ECHO (FUNCALL STANDARD-INPUT ':WHICH-OPERATIONS))
	   (DO (CHAR) (())
	     (SETQ CHAR (FUNCALL STANDARD-INPUT ':TYI-NO-ECHO))
	     (COND ((NOT (OR (= CHAR #\SPACE) (= CHAR #\NEWLINE) (= CHAR #\TAB)))
		    (FUNCALL STANDARD-INPUT ':UNTYI-NO-ECHO CHAR)
		    (RETURN))))))))

(DEFUN MAKE-LINE-LABELS (&REST L)
  (IF (NOT (CHECKLABEL $INCHAR)) (SETQ $LINENUM (1+ $LINENUM)))
  (MAPCAR #'(LAMBDA (CHAR)
	      (LET ((LABEL (CONCAT CHAR $LINENUM)))
		(PUSH LABEL (CDR $LABELS))
		LABEL))
	  L))



(DEFUN $BREAK (&REST ARG-LIST)
  (PROG1 (LEXPR-FUNCALL #'$PRINT ARG-LIST)
	 (MBREAK-LOOP)))

(DEFUN MBREAK-LOOP ()
  (LET ((STANDARD-INPUT TERMINAL-IO-SYNONYM))
    (*CATCH 'BREAK-EXIT
      (DO ((R)) (NIL)
	(FUNCALL STANDARD-INPUT ':FRESH-LINE)
	(SETQ R (CADDR (LET ((*MREAD-PROMPT* (BREAK-PROMPT)))
			 (MREAD STANDARD-INPUT))))
	(CASEQ R
	  (($EXIT) (*THROW 'BREAK-EXIT T))
	  (T (ERRSET (DISPLA (MEVAL R)) T)))))))

(DEFUN RETRIEVE (MSG FLAG &AUX (PRINT? NIL))
  (DECLARE (SPECIAL MSG FLAG PRINT?))
  (OR (EQ FLAG 'NOPRINT) (SETQ PRINT? T))
  (MREAD-TERMINAL
   (CLOSURE '(MSG FLAG)
	    #'(LAMBDA (STREAM CHAR) STREAM CHAR
		      (COND ((NOT PRINT?)
			     (SETQ PRINT? T))
			    ((NULL MSG))
			    ((ATOM MSG)
			     (PRINC MSG)
			     (MTERPRI))
			    ((EQ FLAG T)
			     (MAPC #'PRINC (CDR MSG))
			     (MTERPRI))
			    (T (DISPLA MSG) (MTERPRI)))))))


(DEFMFUN $READ (&REST L)
  (MEVAL (APPLY #'$READONLY L)))

(DEFMFUN $READONLY (&REST L)
  (declare (special l))
  (MREAD-TERMINAL (CLOSURE '(L)
			   #'(LAMBDA (STREAM CHAR) STREAM CHAR
				     (IF L (APPLY #'$PRINT L))))))

(DEFUN MREAD-TERMINAL (PROMPT)
  (CADDR (FUNCALL TERMINAL-IO ':RUBOUT-HANDLER `((:PROMPT ,PROMPT))
		  #'MREAD-RAW TERMINAL-IO)))



#-LISPM
(DEFUN ECHO-INPUT-STREAM-HANDLER (SELF OP DATA)
  (DECLARE (SPECIAL *INSTREAM*))
  (CASEQ OP
         ((TYI)
	  (LET ((CHAR (SFA-CALL (SFA-GET SELF 0.) OP DATA)))
	    (COND ((NOT (OR (= CHAR #^C) (= CHAR #\NULL)))
		   (TYO CHAR (SFA-GET SELF 1.))))
	    CHAR))
	 (T
	  (SFA-CALL (OR (SFA-GET SELF 0.) *INSTREAM*) OP DATA))))

#-LISPM
(DEFUN MAKE-ECHO-INPUT-STREAM (*INSTREAM*
			       &OPTIONAL (OUTSTREAM STANDARD-OUTPUT))
  (DECLARE (SPECIAL *INSTREAM*))
  (LET ((ECHO-STREAM (SFA-CREATE #'ECHO-INPUT-STREAM-HANDLER
				 2. "TYI-Echo Stream")))
    (SFA-STORE ECHO-STREAM 0. *INSTREAM*)
    (SFA-STORE ECHO-STREAM 1. OUTSTREAM)
    ECHO-STREAM))

#+LISPM
(DEFUN MAKE-ECHO-INPUT-STREAM (INSTREAM &OPTIONAL (OUTSTREAM STANDARD-OUTPUT))
  (DECLARE (SPECIAL *INSTREAM* *UNTYI* *OUTSTREAM*))
  (LET-CLOSED ((*INSTREAM* INSTREAM) (*UNTYI* NIL) (*OUTSTREAM* OUTSTREAM))
    #'ECHO-INPUT-STREAM-HANDLER))

#+LISPM
(DEFUN ECHO-INPUT-STREAM-HANDLER (OP &OPTIONAL ARG1 &REST REST)
  (DECLARE (SPECIAL *INSTREAM* *UNTYI* *OUTSTREAM*))
  (CASEQ OP
    (:TYO (TYO ARG1 *OUTSTREAM*))
    (:TYI (IF *UNTYI* (PROG1 *UNTYI* (SETQ *UNTYI* ()))
	    (LET ((CHAR (TYI *INSTREAM* -1)))
	      (IF (NOT (OR (NULL CHAR) (MINUSP CHAR) (= CHAR #/)))
		  (TYO CHAR *OUTSTREAM*))
	      CHAR)))
    (:TYI-NO-ECHO (IF *UNTYI*
		      (PROG1 *UNTYI* (SETQ *UNTYI* NIL))
		      (TYI *INSTREAM* -1)))
    (:UNTYI-NO-ECHO (OR (EQ ARG1 -1) (FUNCALL *INSTREAM* ':UNTYI ARG1)))
    (:UNTYI (SETQ *UNTYI* ARG1))
    (:WHICH-OPERATIONS '(:TYI :UNTYI :TYO :TYI-NO-ECHO :UNTYI-NO-ECHO))
    (:OTHERWISE
      (STREAM-DEFAULT-HANDLER #'ECHO-INPUT-STREAM-HANDLER
			      OP ARG1 REST))))

#+LISPM
(DEFUN MAKE-INPUT-STREAM (X Y) Y ;ignore
  X)

(DEFUN BATCH (FILENAME &OPTIONAL DEMO-P &AUX FILE-OBJ (accumulated-time 0.0))
  (UNWIND-PROTECT
    (CONTINUE (MAKE-ECHO-INPUT-STREAM
		(MAKE-INPUT-STREAM (SETQ FILE-OBJ (OPEN FILENAME))
				    "Batch Input Stream"))
	      (IF DEMO-P ':DEMO ':BATCH))
    (IF FILE-OBJ (CLOSE FILE-OBJ))
    (CURSORPOS 'A)
    (if $showtime
	(MTELL "Batch spent ~A seconds in evaluation.~%"
	       (FORMAT NIL "~2F" accumulated-time)))))

#+LISPM
(DEFUN $BATCH (&REST ARG-LIST)
  (BATCH (FILENAME-FROM-ARG-LIST ARG-LIST) NIL))

(DEFUN FILENAME-FROM-ARG-LIST (ARG-LIST)
  (IF (= (LENGTH ARG-LIST) 1)
      ($FILENAME_MERGE (CAR ARG-LIST))
      ($FILENAME_MERGE `((MLIST),@ARG-LIST))))

(defmspec $grindef (form)
  (eval `(grindef ,@(cdr form)))
  '$DONE)

#+LISPM
(DEFUN $DEMO (&REST ARG-LIST)
  (BATCH (FILENAME-FROM-ARG-LIST ARG-LIST) T))

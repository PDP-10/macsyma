;; -*- Mode: Lisp; Package: Macsyma -*-

;; Lisp Machine additions to the Macsyma suprvisor -- see 
;; MAXII;SYSTEM > and JPG;SUPRV > for the remaining gory details.

;; It would be a win if this was made into a scroll window someday...

(DEFFLAVOR MACSYMA-LISTENER ()			     
	   (TV:PROCESS-MIXIN TV:FULL-SCREEN-HACK-MIXIN
	    TV:WINDOW)
  (:DEFAULT-INIT-PLIST :SAVE-BITS T
		       :PROCESS '(MACSYMA-LISTEN-LOOP :REGULAR-PDL-SIZE 40000
						      :SPECIAL-PDL-SIZE 5000))
  (:DOCUMENTATION :COMBINATION "Normal MACSYMA window"))

;;; These are used to simulate more general streams

(DEFMETHOD (MACSYMA-LISTENER :CHAR-WIDTH) () TV:CHAR-WIDTH)
(DEFMETHOD (MACSYMA-LISTENER :LINE-HEIGHT) () TV:LINE-HEIGHT)

;;; This code tries to make Macsyma deal with Lispm more exceptions correctly.
;;; You should talk to me if you think it needs tweaking, please! --HIC
(DEFMETHOD (MACSYMA-LISTENER :MORE-EXCEPTION) ()
  (TV:SHEET-MORE-HANDLER ':MACSYMA-MORE NIL))	;This needs at least 208.19 to work

(DEFMETHOD (MACSYMA-LISTENER :MACSYMA-MORE) ()
  (MORE-FUN-INTERNAL SELF))

(DEFMETHOD (MACSYMA-LISTENER :DEFER-MORE) ()
  (OR (NULL TV:MORE-VPOS)
      ( TV:MORE-VPOS 100000)
      (INCF TV:MORE-VPOS 100000)))

(COMPILE-FLAVOR-METHODS MACSYMA-LISTENER)

;; The top level function for Macsyma windows.  The :KILL operation resets
;; the process and buries the window.  MACSYMA-TOP-LEVEL exits when the user
;; types QUIT();.  Bind TERMINAL-IO here rather than in MACSYMA-TOP-LEVEL since
;; it is already bound in the supdup server.

(DEFUN MACSYMA-LISTEN-LOOP (TERMINAL-IO)
  (MACSYMA-TOP-LEVEL)
  (FUNCALL TERMINAL-IO ':BURY))

;; Typing (MACSYMA) causes the MACSYMA-TOP-WINDOW to be selected if typed
;; from the Lisp Machine keyboard (TERMINAL-IO is a window stream).  If
;; typed from some other stream, just enter the normal read-eval-print
;; loop.  MACSYMA-TOP-WINDOW is analgous to TV:INITIAL-LISP-LISTENER.

(DEFVAR MACSYMA-TOP-WINDOW (TV:WINDOW-CREATE 'MACSYMA-LISTENER))

(DEFUN MACSYMA ()
  (COND ((TYPEP TERMINAL-IO 'TV:SHEET)
	 ;; If talking to the window system, then select a window
	 (FUNCALL MACSYMA-TOP-WINDOW ':SELECT))
	(T (MACSYMA-TOP-LEVEL))))

;; SMART-TTY and LINE-GRAHPICS-TTY are used
;; by MRG;DISPLA and are set up on ITS in ALJABR;LOADER.  RUBOUT-TTY
;; is used by SUPRV and can be flushed when we start using the Lisp Machine
;; editor.  SCROLLP and SMART-TTY are equivalent for our purposes.

(DECLARE (SPECIAL SMART-TTY RUBOUT-TTY LINE-GRAPHICS-TTY CHARACTER-GRAPHICS-TTY
		  LINEL TTYHEIGHT SCROLLP LG-OLD-X LG-OLD-Y
		  LG-CHARACTER-X LG-CHARACTER-Y LG-CHARACTER-X-2 LG-CHARACTER-Y-2))

;; The average macsyma user will want to have patches loaded, except
;; in unusual circumstances, e.g. a bad patch, in too much of a big hurry,
;; then he would set this variable to T.
;; Of course, he doesn't know about this variable, which is just as well,
;; the loading of patches for lispmachine macsyma is really important.
;; I'm not kidding either. -George Carrette.
;; You may not be kidding, but I am. --HIC
;; And don't put back those silly, pompous greetings strings, either. --HIC, again.
(DEFVAR MACSYMA-LOADED-PATCHES NIL)

(ADD-INITIALIZATION "Macsyma Patches"
		    '(SETQ MACSYMA-LOADED-PATCHES NIL)
		    '(:COLD))

(DEFUN MACSYMA-TOP-LEVEL ()
  (COND ((NOT MACSYMA-LOADED-PATCHES)
	 (SETQ MACSYMA-LOADED-PATCHES T)
	 ;; Print out a greating message of some kind so that the user
	 ;; knows something is happening. This is especially important when the
	 ;; patch filecomputer is slow.
	 (FORMAT STANDARD-OUTPUT "~&[Macsyma: Loading patches.]~%")
	 (LOAD-PATCHES ':SYSTEMS '(MACSYMA)
		       ':VERBOSE
		       ':NOSELECTIVE)
	 (FORMAT STANDARD-OUTPUT "~&[Macsyma: Loading done.]~%")))
  (LET* ((STANDARD-OUTPUT #'MACSYMA-OUTPUT)
	 (^R NIL) (^W NIL)
	 (PACKAGE (PKG-FIND-PACKAGE "MACSYMA"))
	 (BASE 10.) (IBASE 10.) (*NOPOINT T)
	 (W-O (FUNCALL TERMINAL-IO ':WHICH-OPERATIONS))
	 ;; Bind for multiple instantiations -- these variables
	 ;; are stream-dependent.
	 (SMART-TTY (MEMQ ':SET-CURSORPOS W-O))
	 (RUBOUT-TTY SMART-TTY)
	 (SCROLLP (NOT SMART-TTY))
	 (LINE-GRAPHICS-TTY (MEMQ ':DRAW-LINE W-O))
	 (LINEL) (TTYHEIGHT) (LG-OLD-X) (LG-OLD-Y)
	 (LG-CHARACTER-X) (LG-CHARACTER-Y) (LG-CHARACTER-X-2) (LG-CHARACTER-Y-2))
    ;; Uncomment this when somebody tries to take car of a number again
    ;; (SET-ERROR-MODE 1 1 1 1)
    ;; What happens to height on printing ttys?
    (MULTIPLE-VALUE (LINEL TTYHEIGHT)
      (FUNCALL STANDARD-OUTPUT ':SIZE-IN-CHARACTERS))
    (COND (LINE-GRAPHICS-TTY
	   (SETQ LG-CHARACTER-X (FUNCALL STANDARD-OUTPUT ':CHAR-WIDTH))
	   (SETQ LG-CHARACTER-Y (FUNCALL STANDARD-OUTPUT ':LINE-HEIGHT))
	   (SETQ LG-CHARACTER-X-2 (// LG-CHARACTER-X 2))
	   (SETQ LG-CHARACTER-Y-2 (// LG-CHARACTER-Y 2))))
    (CONDITION-BIND ((NIL #'MACSYMA-ERROR-HANDLER))
		    (*CATCH 'SI:TOP-LEVEL	;for the abort key
		      (*CATCH 'MACSYMA-QUIT
			(FUNCALL TERMINAL-IO ':FUNCALL-INSIDE-YOURSELF #'CONTINUE))))))

;; Add "Macsyma" to the window creation menu and System key.
(PUSH '("Macsyma" :VALUE MACSYMA-LISTENER
	:DOCUMENTATION "Macsyma Symbolic Algebra System")
      TV:DEFAULT-WINDOW-TYPES-ITEM-LIST)

(PUSH '(#/A MACSYMA-LISTENER "Macsyma" T) TV:*SYSTEM-KEYS*)

;; When changing the size of a window, must update the variables DISPLA
;; looks at.  This function is bogus since it can get called from processes
;; other than the Macsyma process.  Need some way to SET-IN-STACK-GROUP.
;; LINEL and TTYHEIGHT can't be instance vars, either, since DISPLA must
;; look at them.  Also, must synchronize with the Macsyma process to
;; redisplay the label.  Why not just punt and do the whole thing right?

;; (DEFMETHOD (MACSYMA-LISTENER :AFTER :CHANGE-OF-SIZE-OR-MARGINS) (&REST IGNORE)
;;   (MULTIPLE-VALUE (LINEL TTYHEIGHT)
;;     (FUNCALL-SELF ':SIZE-IN-CHARACTERS)))

;; Random garbage needed to make SUPRV happy.

(DEFUN FORMFEED NIL (FUNCALL STANDARD-INPUT ':CLEAR-SCREEN))

;; This is used someplace in SUPRV.

(DEFUN FILE-OPEN (FILE-OBJ)
    (EQ (FUNCALL FILE-OBJ ':STATUS) ':OPEN))

;; Takes a string file specification and returns an oldio list specification.  Similar
;; to MacLisp NAMELIST function. (UNEXPAND-PATHNAME "C: D; A B") --> (A B C D)

(DEFUN UNEXPAND-PATHNAME (STRING)
  (LET* ((PATHNAME (FS:PARSE-PATHNAME STRING))
	 (DEV (FUNCALL PATHNAME ':DEVICE)))
    (IF (STRING-EQUAL DEV "DSK")
	(SETQ DEV (FUNCALL PATHNAME ':HOST)))
    (MAPCAR 'INTERN (LIST (FUNCALL PATHNAME ':FN1)
			  (FUNCALL PATHNAME ':FN2)
			  (FUNCALL DEV ':NAME-AS-FILE-COMPUTER)
			  (FUNCALL PATHNAME ':DIRECTORY)))))

(DEFUN NAMESTRING (OLDIO-LIST)
    (INTERN
      (FORMAT NIL "~A/:~A/;~A ~A"
	    (THIRD OLDIO-LIST)
	    (FOURTH OLDIO-LIST)
	    (FIRST OLDIO-LIST)
	    (SECOND OLDIO-LIST))))

;; Takes a list like in ITS Macsyma, returns a string.
;; Device defaults to MC, as does DSK device specification.
;; Directory defaults to user-id.  Hack USERSn later.
;; (FILESTRIP '($A $B $C $D)) --> "C: D; A B"
;; (FILESTRIP '($A $B $C)) --> "MC: C; A B"

(DEFUN FILESTRIP (X &AUX FN1 FN2 DEV DIR)
  (IF (AND (= (LENGTH X) 1) (SYMBOLP (CAR X)) (= (AREF (STRING (CAR X)) 0) #/&))
      ;; A Macsyma string, use it as is
      (SUBSTRING (STRING (CAR X)) 1)
      ;; Otherwise...
      (SETQ X (FULLSTRIP X)) ;Strip the list, leave NIL as NIL.
      (SETQ FN1 (CAR X) FN2 (CADR X) DEV (CADDR X) DIR (CADDDR X))
      (IF (AND DEV (NULL DIR)) (SETQ DIR DEV DEV NIL))
      (IF (EQ DEV 'DSK) (SETQ DEV "MC"))
      ;;If case doesn't matter, don't confuse user.
      (STRING-UPCASE (:FORMAT NIL "~A: ~A; ~A ~A"
			      (OR DEV "MC") (OR DIR USER-ID)
			      (OR FN1 "MAXOUT") (OR FN2 ">")))))

(DEFUN FIND (FUNC MEXPRP)
  MEXPRP
  (COND ((GET FUNC 'AUTOLOAD)
	 (FERROR NIL "~A is needed from file ~S, but not in core"
		 FUNC (GET FUNC 'AUTOLOAD))))
  NIL)

(DECLARE (SPECIAL ERROR-CALL))
(DEFUN MACSYMA-ERROR-HANDLER (&REST IGNORE)
   (COND ((NULL ERROR-CALL) NIL)
	 (T (LET ((SIGNAL))
		 (SETQ SIGNAL (FUNCALL ERROR-CALL NIL))
		 (COND ((NULL SIGNAL) NIL)
		       ((EQ SIGNAL 'LISP)
			(SETQ EH:ERRSET-STATUS NIL
			      EH:ERRSET-PRINT-MSG T)
			NIL)
		       ((EQ SIGNAL 'EXIT) (*THROW 'SI:TOP-LEVEL NIL))
		       (T NIL))))))

(DEFUN TOP-MEVAL (FORM)
   (*CATCH 'SI:TOP-LEVEL      ;THROWS TO TOP LEVEL CAUSE RETURN OF NIL
	   (NCONS (MEVAL* FORM))))

(DECLARE (SPECIAL WRITEFILE-OUTPUT))
(DECLARE (SPECIAL WRITEFILE-OPERATIONS))
(DEFVAR ^R NIL)
(DEFVAR ^W NIL)
(DEFVAR INFILE NIL)				;Bullshit

(DEFUN MACSYMA-OUTPUT (OP &REST REST)
  (SELECTQ OP
    (:WHICH-OPERATIONS (FUNCALL TERMINAL-IO ':WHICH-OPERATIONS))
    (T (IF (AND ^R (MEMQ OP WRITEFILE-OPERATIONS))
	   (LEXPR-FUNCALL WRITEFILE-OUTPUT OP REST))
       (IF (NOT ^W) (LEXPR-FUNCALL TERMINAL-IO OP REST)))))

;; Specify entire filename when WRITEFILE is done.  No need to worry
;; about clobbering existing file when writing since output goes
;; to _LSPM_ OUTPUT until file sucessfully closed.

(DEFMFUN $WRITEFILE (&REST L)
  (LET ((NAME ($FILENAME_MERGE (FILENAME-FROM-ARG-LIST L)
			       "MAXOUT"
			       (FS:USER-HOMEDIR))))
  (SETQ WRITEFILE-OUTPUT (OPEN NAME ':OUT))
  (SETQ WRITEFILE-OPERATIONS (FUNCALL WRITEFILE-OUTPUT ':WHICH-OPERATIONS))
  (OR (MEMQ ':FRESH-LINE WRITEFILE-OPERATIONS)
      ;; patch a bug, a file *can* handle this, but which-operations doesn't
      ;; say it can.
      (PUSH ':FRESH-LINE WRITEFILE-OPERATIONS))
  (SETQ ^R T)
  NAME))

(DEFUN $CLOSEFILE ()
  (SETQ ^R NIL)
  (CLOSE WRITEFILE-OUTPUT)
  '$DONE)

;; Random useful functions to call from Macsyma Toplevel.

(DEFUN $ED () (ED))

(DEFUN $GC_ON () (GC-ON))
(DEFUN $GC_OFF () (GC-OFF))

(DEFUN $SCHEDULE (&OPTIONAL (N 1)) (DOTIMES (I N) (PROCESS-ALLOW-SCHEDULE)))
(DEFUN $SLEEP (&OPTIONAL (60THS-SEC 60.)) (PROCESS-SLEEP 60THS-SEC))

(DEFUN $BEEP () (FUNCALL STANDARD-OUTPUT ':BEEP))

;; To do:
;; Figure out some way of making $LINENUM and $% process-specific.
;; JLK suggests something like D4.23 as meaning D-line 23 in Macsyma Listener #4.

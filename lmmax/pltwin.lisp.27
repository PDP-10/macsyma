;;;-*- Mode:LISP; Package:MACSYMA -*-

;;; Window System Interface for the Macsyma Plot Package.

;;; First define the high level structures, namely the constraint frames
;;; which contain a plotting window, a menu, and (optionally) a Macsyma listener.

;;; Some special variables needed below.

(DEFVAR PLOT-FONT-MAP 'FONTS:(CPTFONT MEDFNT BIGFNT))	;fonts used on plot windows

;;; Menu item lists for the plot menu.

(DEFVAR PLOT-MENU-ITEM-LIST
	'(("Continue" :KBD #/C)
;	  ("Name"     :KBD #/N)
;	  ("Redisplay":KBD #/R)
;	  ("Hardcopy" :KBD #/H)
	  ("Other"    :KBD #/O)))

(DEFVAR PLOT-OTHER-MENU-ITEM-LIST	;invoked by OTHER-CMDS
	'(("Store"    . :STORE-PLOT)
	  ("Retrieve" . :RETRIEVE-PLOT)
	  ("Multiple Plots" . :REPLOT-MULTIPLE)
	  ("Options" . :CHANGE-OPTIONS)
	  ("Change Plot" . :CHANGE-PARAMETERS)))

;;; Define a Macsyma plot frame.

(DEFFLAVOR MACSYMA-PLOT-FRAME ((MACSYMA-LISTENER-WINDOW NIL))
  (TV:BORDERED-CONSTRAINT-FRAME)
  (:DEFAULT-INIT-PLIST :SAVE-BITS T)
  (:INITABLE-INSTANCE-VARIABLES MACSYMA-LISTENER-WINDOW)
  )

;;; Defines the panes contained in the frame and the constaints they must satisfy.
;;; Note, if MACSYMA-LISTENER-WINDOW is non-nil, it is assumed that the thus
;;; specified MACSYMA-LISTENER is to be included at the bottom of the frame.

(DEFMETHOD (MACSYMA-PLOT-FRAME :BEFORE :INIT) (IGNORE &AUX LISTENER-ITEM
							   LISTENER-PANE-ITEM
						           LISTENER-CONSTRAINT
							   PLOT-PANE-CONSTRAINT)
  (SETQ PLOT-PANE-CONSTRAINT '((PLOT-PANE :EVEN)))
  (IF MACSYMA-LISTENER-WINDOW
      (SETQ LISTENER-ITEM '(LISTENER-PANE)
	    LISTENER-PANE-ITEM '((LISTENER-PANE MACSYMA-LISTENER))
	    LISTENER-CONSTRAINT '(((LISTENER-PANE :EVEN)))
	    PLOT-PANE-CONSTRAINT '((PLOT-PANE 0.75s0))))

  (SETQ TV:PANES `((PLOT-PANE MACSYMA-PLOT-PANE :LABEL "Macsyma Plotting Window"
			      :FONT-MAP ,PLOT-FONT-MAP)
		   (PLOT-MENU TV:COMMAND-MENU-PANE :ITEM-LIST ,PLOT-MENU-ITEM-LIST
			      :FONT-MAP (FONTS:MEDFNT))
		   .,LISTENER-PANE-ITEM)
	TV:CONSTRAINTS `((MAIN . ((PLOT-PANE PLOT-MENU .,LISTENER-ITEM)
				  ((PLOT-MENU :ASK :PANE-SIZE))
				  ,PLOT-PANE-CONSTRAINT
				  .,LISTENER-CONSTRAINT)))))

;;; Now define the MACSYMA-PLOT-PANE and MACSYMA-PLOT-MENU flavors.
(DEFFLAVOR MACSYMA-PLOT-PANE () (TV:PANE-MIXIN TV:WINDOW))

;;; Window of the plotting area
(DEFMETHOD (MACSYMA-PLOT-PANE :GET-PLOTTING-RANGE) ()
  (MULTIPLE-VALUE-BIND (WIDTH HEIGHT)
      (FUNCALL-SELF ':INSIDE-SIZE)
      (IF (> WIDTH HEIGHT) (LET ((X-OFF (// (- WIDTH HEIGHT) 2)))
			     (LIST X-OFF (- TV:HEIGHT HEIGHT) (+ X-OFF HEIGHT) HEIGHT))
	  (LET ((Y-OFF (// (- HEIGHT WIDTH) 2)))
	    (LIST (// (- TV:WIDTH WIDTH) 2) Y-OFF WIDTH (+ Y-OFF WIDTH))))))

;;; Clear the plot pane, make sure it has the same IO-BUFFER as
;;; the Macsyma listener (TERMINAL-IO), select the plotting pane,
;;; and expose the whole plotting frame.
;;; (It is unclear if TERMINAL-IO is the right thing here, but...)

(DEFMETHOD (MACSYMA-PLOT-PANE :INIT-FOR-PLOTTING)
	   (&AUX (IO-BUFFER (FUNCALL TERMINAL-IO ':IO-BUFFER))
		 (MENU (FUNCALL TV:SUPERIOR ':GET-PANE 'PLOT-MENU)))
   (FUNCALL-SELF ':CLEAR-SCREEN)		;clear the plot pane before exposing
   (SETQ TV:IO-BUFFER IO-BUFFER)		;plot pane's io-buffer
   (SET-IN-INSTANCE MENU 'TV:IO-BUFFER IO-BUFFER)	;plot menu's io-buffer
   (FUNCALL TV:SUPERIOR ':EXPOSE)		;expose the whole frame
   (FUNCALL-SELF ':SELECT))			;and select the plot pane

;;; Select the original Macsyma listener (TERMINAL-IO) and bury
;;; if necessary.
(DEFMETHOD (MACSYMA-PLOT-PANE :END-PLOTTING) ()
   (TV:DESELECT-AND-MAYBE-BURY-WINDOW TV:SUPERIOR)
   (FUNCALL TERMINAL-IO ':SELECT))

(DEFVAR PLOT-FRAME)			;handy for debugging

;; generalize this to include a Macsyma Listener option
(DEFUN MAKE-PLOT-WINDOW-STREAM ()
  (SETQ PLOT-FRAME (TV:MAKE-WINDOW 'MACSYMA-PLOT-FRAME  ':SAVE-BITS T))
  (FUNCALL PLOT-FRAME ':GET-PANE 'PLOT-PANE))

(DEFVAR SPLIT-SCREEN-PLOT-FRAME NIL)

;;; Setup for plotting and Macsyma listening
(DEFUN $PLOT_SPLITSCREEN ()
  (IF (NOT SPLIT-SCREEN-PLOT-FRAME)
      (SETQ SPLIT-SCREEN-PLOT-FRAME
	    (TV:MAKE-WINDOW 'MACSYMA-PLOT-FRAME ':MACSYMA-LISTENER-WINDOW T)))
  (SETQ PLOT-STREAM (FUNCALL SPLIT-SCREEN-PLOT-FRAME ':GET-PANE 'PLOT-PANE))
  (FUNCALL SPLIT-SCREEN-PLOT-FRAME ':EXPOSE)
  (FUNCALL (FUNCALL SPLIT-SCREEN-PLOT-FRAME ':GET-PANE 'LISTENER-PANE) ':SELECT)
  '$DONE)

(DEFUN $PLOT_FULLSCREEN ()
  (SETQ PLOT-STREAM (IF (NOT (BOUNDP 'PLOT-FRAME)) (MAKE-PLOT-WINDOW-STREAM)
			(FUNCALL PLOT-FRAME ':GET-PANE 'PLOT-PANE)))
  (FUNCALL MACSYMA-TOP-WINDOW ':EXPOSE)
  (FUNCALL MACSYMA-TOP-WINDOW ':SELECT))

(COMPILE-FLAVOR-METHODS MACSYMA-PLOT-PANE MACSYMA-PLOT-FRAME)

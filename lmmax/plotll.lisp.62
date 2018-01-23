;;; -*- mode:lisp; package:macsyma -*-

;;; Low level line drawing routines for the MACSYMA plot package on the LISP machine
;;; THIS NEEDS TO BE FLUSHED SOMEDAY

;;; Todo:
;;;   correct pointi
;;;   finish $PLOT_COMMAND and related functions
;;;   see how PLOTMODE should affect the scaling
;;;   see if the clipping in VECTOR can be replaced by the standard methods
;;;   figure out what pnt-status does and see if it is necessary
;;;   replace the gross exploden, stripdollar stufff with something more reasonable
;;;   flush crufty declare specials

(declare (special plot-last-hi-y plot-last-lo-y plot-last-hi-x plot-mode
		  print-mode $loadprint $values $myoptions plot-opts
		  plot-vals char-type $charratio $plotmode dasharray
		  print-dasharray print-symbolarray symbolarray
		  dashl odashl beamon drawn print-line1 char-width char-height
		  min-x max-x min-y max-y last-x last-y size-x size-y pnt-status
		  screen-last-x screen-last-y DISPLAY-MODE))

;;; Macsyma Plot Frames are defined in LMMAX;PLTWIN >

;;; The default plotting stream is a plot frame.
(DEFVAR PLOT-STREAM (MAKE-PLOT-WINDOW-STREAM))

#+LISPM
(DEFVAR PLOT-FONT FONTS:CPTFONT)		;maybe Timesroman would be better?

;;; *** This gets redone when the change options feature gets implemented
(defun plot-startup nil
   (setq plot-opts '($clear $wait $plotbell)
	 plot-vals '(t t t nil))
   (mapcar #'(lambda (plotvar plotval) 
		     (cond ((boundp plotvar)
			    (and (memq plotvar $values)
				 (set plotvar (prog2 nil (eval plotvar)
						     (remvalue plotvar 'PLOT-STARTUP)
						     (add2lnc plotvar $myoptions)))))
			   (t (set plotvar plotval))))

	   ;; these guys aren't reset by PLOTRESET
	   (append plot-opts '($plotthick $plotscale $charratio $plotlftmar $plotbotmar))
	   (append plot-vals '(2. 1.75 2.5 150. 150.)))
   t)

(plot-startup)

;;; **** This should also change the x and y relative scales???

(DEFVAR $PLOTSCALE 1.0)

(defmspec $plotmode (l) (setq l (cdr l))
  (SETQ $PLOTMODE NIL DISPLAY-MODE NIL)
  (IF (memq '$DISPLAY l)
      (setq char-height (font-char-height plot-font)
	    char-width (font-char-width plot-font)
	    DISPLAY-MODE T))
  (IF (memq '$PAPER l)
      (setq char-height (fix (+$ 0.5 (//$ 25.0 (float $plotscale))))
	    char-width (fix (+$ 0.5 (//$ 16.0 (float $plotscale))))))
  (setq $plotmode `((mlist) ,@ l)))

(meval '(($plotmode) $DISPLAY))

;;; Enter and exit graphics mode.  Process commands at the end of plotting.

(declare (special graphic-mode $clear $wait
		  $plotbell $plotthick $plotscale
		  $plotheight $plotlftmar $plotbotmar))

(setq graphic-mode nil)

(defun $clear () (FUNCALL PLOT-STREAM ':CLEAR-SCREEN))

(defun $entergraph nil 
   (if graphic-mode (let (($wait)) ($exitgraph)))
   (setq graphic-mode t)
   (SETQ screen-last-x 0)
   (SETQ screen-last-y 0)
   T)

(defun $exitgraph (&AUX (CMD -1))
  (SETQ GRAPHIC-MODE NIL)
  (if (AND $WAIT DISPLAY-MODE)
      (progn
	(if $plotbell (FUNCALL TERMINAL-IO ':BEEP))
	(SETQ CMD (FUNCALL PLOT-STREAM ':TYI))))
  CMD)

(DEFUN $INITGRAPH () (FUNCALL PLOT-STREAM ':INIT-FOR-PLOTTING))

(DEFUN $ENDGRAPH () (FUNCALL PLOT-STREAM ':END-PLOTTING))

(DEFUN GET-PLOTTING-RANGE () (FUNCALL PLOT-STREAM ':GET-PLOTTING-RANGE))

(DEFUN $PLOT_COMMAND (CMD)
  (SELECTQ CMD
    ((#/C #\SPACE) ($ENDGRAPH))
    ((#/O) (FUNCALL TERMINAL-IO ':BEEP) ($ENDGRAPH))
    (:OTHERWISE ($ENDGRAPH)
		(IF (AND (FIXP CMD) (> CMD 0)) (FUNCALL TERMINAL-IO ':UNTYI CMD))))
  '$DONE)

;;; drawing primitives

;;; **** change this back at somepoint.  Do this via definesymbol.
(defun pointi (x y)
   (FUNCALL PLOT-STREAM ':DRAW-POINT X Y)
   (FUNCALL PLOT-STREAM ':DRAW-POINT X (1+ Y))
   (FUNCALL PLOT-STREAM ':DRAW-POINT (1+ X) Y)
   (FUNCALL PLOT-STREAM ':DRAW-POINT (1+ X) (1+ Y))
   (setq pnt-status t last-x x last-y y screen-last-x x screen-last-y y))

(defun vectori (x y)
  (MULTIPLE-VALUE-BIND (WIDTH HEIGHT) (FUNCALL PLOT-STREAM ':SIZE)
    (FUNCALL PLOT-STREAM ':DRAW-LINE LAST-X (- HEIGHT LAST-Y) X (- HEIGHT Y)))
  (setq pnt-status t last-x x last-y y screen-last-x x screen-last-y y))

(DEFUN PLOT-DRAW-CHAR (CHAR X Y)
  (MULTIPLE-VALUE-BIND (WIDTH HEIGHT) (FUNCALL PLOT-STREAM ':SIZE)
    (FUNCALL PLOT-STREAM ':DRAW-CHAR (FUNCALL PLOT-STREAM ':CURRENT-FONT)
	     CHAR X (- HEIGHT Y))))

;;; Line, Vector, Point primitives

(DEFMACRO CHECKPNT (X Y) `(not (or (< ,x min-x) (> ,x max-x) (< ,y min-y) (> ,y max-y))))

;;; setpoint

(defun $setpoint (xf yf) (setpoint (plot-x xf) (plot-y yf))) 

(defun setpoint (x y) 
   (IF (checkpnt x y) (setpointi x y)
       (setq pnt-status nil last-x x last-y y)))

(defun setpointi (x y)
   (IF (NOT (and pnt-status (= screen-last-x x) (= screen-last-y y)))
       (setq pnt-status t last-x x last-y y screen-last-x x screen-last-y y)))

;;; draw point

(defun plot-point (x y) 
  (IF (checkpnt x y) (pointi x y)
      (setq pnt-status nil last-x x last-y y)))

(defun $point (xf yf) (plot-point (plot-x xf) (plot-y yf))) 


;;; vectors

(setq dashl nil odashl nil beamon t drawn 0.
      dasharray (make-array nil 'art-q 10.)
      print-dasharray (make-array nil 'art-q 10.))

;;; patterned (dashed) vectors

(defun vectord (x y) 
  (cond ((null dashl) (vectori x y))		;simple case, solid lines
	((not (eq (typep dashl) 'list)) (setpoint x y))	;otherwise, just points
	(t (let ((save-x last-x) (save-y last-y) (del-xf (float (- x last-x)))
				 (del-yf (float (- y last-y))) (lenf 0.0) (len 0.))
		(setq lenf (sqrt (+$ (*$ del-xf del-xf) (*$ del-yf del-yf))) 
		      len (fix (+$ lenf 0.5)) 
		      lenf (if (= 0 lenf) 1.0 lenf)
		      del-xf (//$ del-xf lenf) del-yf (//$ del-yf lenf))
		(do ((runl (- (car dashl) drawn) (+ runl (car dashl)))
		     (targ-x) (targ-y))
		    (nil)
		    (setq targ-x (+ save-x (fix (+$ (*$ (float runl) del-xf) 0.5))) 
			  targ-y (+ save-y (fix (+$ (*$ (float runl) del-yf) 0.5))))
		    (cond (beamon (cond ((< runl len)
					 (vectori targ-x targ-y)
					 (setq drawn 0. dashl (cdr dashl)
					       beamon nil))
					(t (vectori x y)
					   (cond ((= runl len)
						  (setq drawn 0. dashl (cdr dashl)
							beamon nil))
						 (t (setq drawn
							  (- (car dashl)
							     (- runl len)))))
					   (return nil))))
			  (t (cond ((< runl len)
				    (setpoint targ-x targ-y)
				    (setq drawn 0. dashl (cdr dashl) beamon t))
				   (t (setpoint x y)
				      (cond ((= runl len)
					     (setq drawn 0. dashl (cdr dashl) 
						   beamon t))
					    (t (setq drawn (- (car dashl)
							      (- runl len)))))
				      (return nil))))))))))

(defun $definedash (l1 l)
       (or (and (fixp l1) (< l1 10.) (> l1 -1.))
	   (Ferror NIL "First arg to DEFINEDASH must lie between 0 and 9: ~A" L1))
       (prog2 nil
	      (list '(mlist simp) l1 l)
	      (cond ((or (null l) (eq l t) (eq (typep l) 'list))
		     (and (eq (typep l) 'list)
			  (listp (car l))
			  (eq (caar l) 'mlist) (setq l (cdr l)))
		     (or (eq l t) (setq l (mapcar 'fix l)))
		     (aset l dasharray l1))
		    (t (or (fixp l) (setq l (car (exploden (stripdollar l)))))
		       (aset l print-dasharray l1)))))

(defun $changedash (x) 
   (setq dashl (aref dasharray x) odashl nil drawn 0. beamon t)
   (cond ((eq (typep dashl) 'list)
	  (setq dashl (append dashl nil))
	  (rplacd (last dashl) dashl)))
   nil)

(defun $pushdash nil (setq odashl dashl dashl nil)) 

(defun $popdash nil (setq dashl odashl odashl nil))

(defun init-dashes nil
       ($definedash 1. '(40. 8.)) ($definedash 2. '(15. 8.)) ($definedash 3. '(1. 7.))
       ($definedash 4. '(30. 8. 1. 8.)) ($definedash 5. '(30. 8. 1. 8. 1. 8.))
       ($definedash 6. '(40. 8. 1. 8. 5. 8. 1. 8.)) ($definedash 7. '(8. 30.))
       ($definedash 8. '(1. 20.)) ($definedash 9. t) 
       (fillarray print-dasharray '(46. 42. 35. 36. 37. 38. 43. 64. 45. 0.))
       (setq print-line1 (aref print-dasharray 0.)))

(init-dashes)

;;; Can this be done with the built-in methods?
;;; ****

;;; vectors with clipping

(defun vector (x y) 
       (cond ((and pnt-status (checkpnt x y)) (vectord x y))
	     (t (prog (del-x del-y save-x save-y) 
		      (setq save-x x save-y y del-x (- x last-x) del-y (- y last-y))
		      (cond ((> last-y max-y)
			     (cond ((> y max-y) (go no-vector))
				   (t (setq last-x (intercept last-x last-y del-x del-y max-y)
					    last-y max-y)
				      (cond ((< y min-y)
					     (setq x (intercept x y del-x del-y min-y)
						   y min-y))))))
			    ((< last-y min-y)
			     (cond ((< y min-y) (go no-vector))
				   (t (setq last-x (intercept last-x last-y del-x del-y min-y)
					    last-y min-y)
				      (cond ((> y max-y)
					     (setq x (intercept x y del-x del-y max-y)
						   y max-y))))))
			    ((> y max-y)
			     (setq x (intercept x y del-x del-y max-y) y max-y))
			    ((< y min-y)
			     (setq x (intercept x y del-x del-y min-y) y min-y)))
		      (cond ((> last-x max-x)
			     (cond ((> x max-x) (go no-vector))
				   (t (setq last-y (intercept last-y last-x del-y del-x max-x)
					    last-x max-x)
				      (cond ((< x min-x)
					     (setq y (intercept y x del-y del-x min-x)
						   x min-x))))))
			    ((< last-x min-x)
			     (cond ((< x min-x) (go no-vector))
				   (t (setq last-y (intercept last-y last-x del-y del-x min-x)
					    last-x min-x)
				      (cond ((> x max-x)
					     (setq y (intercept y x del-y del-x max-x)
						   x max-x))))))
			    ((> x max-x)
			     (setq y (intercept y x del-y del-x max-x) x max-x))
			    ((< x min-x)
			     (setq y (intercept y x del-y del-x min-x) x min-x)))
		      (cond ((not pnt-status) (setpoint last-x last-y)))
		      (vectord x y)
		      no-vector
		      (cond ((not (checkpnt save-x save-y))
			     (setq pnt-status nil last-x save-x last-y save-y))))))
       nil) 

(defun intercept (x y del-x del-y max-y) (- x (// (* del-x (- y max-y)) del-y))) 

(defun $vector (xf yf) (vector (plot-x xf) (plot-y yf))) 

;;; lines

(defun line (x1 y1 x y) (setpoint x1 y1) (vector x y)) 

(defun $line (xf1 yf1 xf yf) 
       (setpoint (plot-x xf1) (plot-y yf1)) (vector (plot-x xf) (plot-y yf))) 

;;; symbols

(setq symbolarray (make-array nil 'art-q 10.)
      print-symbolarray (make-array nil 'art-q 10.))

(defun drawsymbol (x y x1) 
   ($pushdash)
   (do ((symbl0 (aref symbolarray x1) (cdr symbl0)) (draw nil (not draw)))
       ((cond ((null symbl0)) ((eq symbl0 t) (plot-point x y) t)))
       (do ((symbl1 (car symbl0) (cddr symbl1))) ((null (cdr symbl1)))
	   (cond (draw (vector (+ x (car symbl1)) (+ y (cadr symbl1))))
		 (t (setpoint (+ x (car symbl1)) (+ y (cadr symbl1)))))))
   ($popdash))

(defun $drawsymbol (xf yf x1) (drawsymbol (plot-x xf) (plot-y yf) x1)) 

(defun $definesymbol (l1 l) 
       (or (and (fixp l1) (< l1 10.) (> l1 -1.))
	   (Ferror NIL "First arg to DEFINESYMBOL must lie between 0 and 9: ~A" L1))
       (prog2 nil
	      (list '(mlist simp) l1 l)
	      (cond ((or (null l) (eq l t) (eq (typep l) 'list))
		     (and (eq (typep l) 'list)
			  (listp (car l))
			  (eq (caar l) 'mlist) (setq l (cdr l)))
		     (or (eq l t)
			 (setq l (mapcar #'(lambda (l2) (and (listp (car l2))
							     (eq (caar l2) 'mlist)
							     (setq l2 (cdr l2)))
						   (mapcar 'fix l2))
					 l)))
		     (aset l symbolarray l1))
		    (t (or (fixp l) (setq l (car (exploden (stripdollar l)))))
		       (aset l print-symbolarray l1))))) 

(defun init-symbols nil
       ($definesymbol 0. nil)
       ($definesymbol 1. '((0. 6.) (0. -6.) (-6. 0.) (6. 0.) (0. 0.)))
       ($definesymbol 2. '((4. 4.) (-4. -4.) (4. -4.) (-4. 4.) (0. 0.)))
       ($definesymbol 3. '((6. 6.) (6. -6. -6. -6. -6. 6. 6. 6.) (0. 0.) (0. 0.)))
       ($definesymbol 4. '((8. 0.) (0. -8. -8. 0. 0. 8. 8. 0.) (0. 0.) (0. 0.)))
       ($definesymbol 5. '((0. 8.) (6. -4. -6. -4. 0. 8.) (0. 0.) (0. 0.)))
       ($definesymbol 6. '((0. -8.) (6. 4. -6. 4. 0. -8.) (0. 0.) (0. 0.)))
       ($definesymbol 7. '((8. 0.) (-4. 6. -4. -6. 8. 0.) (0. 0.) (0. 0.)))
       ($definesymbol 8. '((-8. 0.) (4. 6. 4. -6. -8. 0.) (0. 0.) (0. 0.)))
;      ($definesymbol 9. '((0. 9.) (4. -6. -7. 2. 7. 2. -4. -6. 0. 9.) (0. 0.)))
       ($definesymbol 9. t)
       (fillarray print-symbolarray '(0. 65. 66. 67. 68. 69. 70. 71. 72. 73.)))

(init-symbols) 

;;; scaling functions

(declare (special min-xf max-xf min-yf max-yf size-xf size-yf scale-x scale-y)) 

(setq min-xf 0.0 min-yf 0.0 max-xf 1023.0 max-yf 1023.0 size-xf 1023.0 size-yf 1023.0)

(defun $screensize (x1 y1 x y) 
       (setq min-x x1 min-y y1 max-x x max-y y
	     size-x (- max-x min-x) size-y (- max-y min-y) 
	     scale-x (//$ size-xf (float size-x)) scale-y (//$ size-yf (float size-y)) 
	     pnt-status nil last-x min-x last-y min-y)
       nil) 

;;; what should this be?****
($screensize 0. 0. 1023. 1023.)

(defun $screensize1 (x1 y1 x y) 
       ($size (plot-xf x1) (plot-yf y1) (plot-xf x) (plot-yf y))
       ($screensize x1 y1 x y))  

(defun $size (xf1 yf1 xf yf) 
       (setq min-xf xf1 min-yf yf1 max-xf xf max-yf yf 
	     size-xf (-$ max-xf min-xf) size-yf (-$ max-yf min-yf) 
	     scale-x (//$ size-xf (float size-x)) scale-y (//$ size-yf (float size-y)))
       nil) 

(defun plot-x (xf) (+ min-x (fix (+$ 0.5 (//$ (-$ xf min-xf) scale-x))))) 

(defun plot-y (yf) (+ min-y (fix (+$ 0.5 (//$ (-$ yf min-yf) scale-y))))) 

(defun plot-xf (x) (+$ min-xf (*$ (float (- x min-x)) scale-x))) 

(defun plot-yf (y) (+$ min-yf (*$ (float (- y min-y)) scale-y))) 

(declare (special txfun-x txfun-y txfun-x-nargs txfun-y-nargs))

(defun call-x (&optional xf yf zf)
       (selectq txfun-x-nargs
		(0 xf)
		(1 (funcall txfun-x xf))
		(2 (funcall txfun-x xf yf))
		(3 (funcall txfun-x xf yf zf))
		(otherwise
		 (Ferror NIL "Wrong number arguments to x transformation function"))))

(defun call-y (&optional xf yf zf)
       (selectq txfun-y-nargs
		(0 yf)
		(1 (funcall txfun-y xf))
		(2 (funcall txfun-y xf yf))
		(3 (funcall txfun-y xf yf zf))
		(otherwise
		 (Ferror NIL "Wrong number arguments to y transformation function"))))

(defun call-init (xfun yfun)
   (setq txfun-x xfun
	 txfun-y yfun
	 txfun-x-nargs (cond (xfun (length (arglist xfun)))
			     (t 0))
	 txfun-y-nargs (cond (yfun (length (arglist yfun)))
			     (t 0))))

(defun $setpoint3 (xf yf zf) ($setpoint (call-x xf yf zf) (call-y xf yf zf)))

(defun $point3 (xf yf zf) ($point (call-x xf yf zf) (call-y xf yf zf)))

(defun $vector3 (xf yf zf) ($vector (call-x xf yf zf) (call-y xf yf zf)))

(defun $line3 (xf1 yf1 zf1 xf yf zf)
       ($line (call-x xf1 yf1 zf) (call-y xf1 yf1 zf1)
	      (call-x xf yf zf) (call-y xf yf zf)))

(defun $drawsymbol3 (xf yf zf x1) ($drawsymbol (call-x xf yf zf) (call-y xf yf zf) x1))

;;; Character drawing routines
;;; (this is pretty random and needs to be redone)

(defun gterpri nil (setpoint min-x (- last-y char-height))) 

(defun $gterpri nil (gterpri)) 

(defun ghprint (l x y a1) 
       (cond ((atom l) (setq l (exploden l)))
	     (t (setq l (apply 'append (mapcar 'exploden l)))))
       (let ((b1 (* char-width (length l)))
	     (uline (> a1 9.)))
	    (and uline (setq a1 (- a1 10.)))
	    (cond ((= a1 0.))
		  ((= a1 1.) (setq x (- x (// b1 2.))))
		  ((= a1 2.) (setq x (- x b1))))
	    (and (> (+ x b1) max-x) (setq x (- max-x b1)))
	    (and (< x min-x) (setq x min-x))
	    (and (> (+ y char-height) max-y) (setq y (- max-y char-height)))
	    (and (< y min-y) (setq y min-y))
	    (do ((l l (cdr l)) (x x (+ x char-width)) (flg t NIL))
		((or (null l) (> x (- max-x char-width))))
		(cond (flg (setpoint x y) (setq pnt-status nil)))
		(PLOT-draw-char (car l) x (+ char-height y)))
	    (if uline (line x (- y 2.) (+ x b1) (- y 2.)))
	    (setpoint (+ x b1) y)))

(defun $ghprint (l x y a1) 
       (setq l (cond ((and (listp l) (listp (car l)) (eq (caar l) 'mlistp))
                      (cdr l))
                     (t l)))
       (ghprint (cond ((atom l) (stripdollar l)) (t (mapcar 'stripdollar l)))
		x y a1))

(defun gvprint (l x y a1)
       (cond ((atom l) (setq l (exploden l)))
	     (t (setq l (apply 'append (mapcar 'exploden l)))))
       (let ((b1 (* char-height (length l))))
	    (cond ((= a1 0.))
		  ((= a1 1.) (setq y (+ y (// b1 2.))))
		  ((= a1 2.) (setq y (+ y b1))))
	    (if (< (- y b1) min-y) (setq y (+ min-y b1)))
	    (if (> y max-y) (setq y max-y))
	    (if (> (+ x char-width) max-x) (setq x (- max-x char-width)))
	    (if (< x min-x) (setq x min-x))
	    (do ((l l (cdr l)) (y (- y char-height) (- y char-height)))
		((or (null l) (< y min-y)))
		(setpoint x y)(setq pnt-status nil)
		(PLOT-draw-char (car l) x (+ char-height y)))
	    (setpoint y (- y b1))))

(defun $gvprint (l x y a1) 
       (setq l (cond ((and (listp l) (listp (car l)) (eq (caar l) 'mlistp))
                      (cdr l))
                     (t l)))
       (gvprint (cond ((atom l) (stripdollar l)) (t (mapcar 'stripdollar l)))
		x y a1))

(defun gmark (x y x1) 
  (setpoint (- x (// char-width 2.)) (- y (// char-height 2.)))
  (if pnt-status
      (cond ((checkpnt (+ last-x char-width) (+ last-y char-height))
	     (setq pnt-status nil)
	     (PLOT-draw-char x1 last-x last-y))))
  (setpoint x y))

(defun $gmark (xf yf x1) (gmark (plot-x xf) (plot-y yf) x1))  

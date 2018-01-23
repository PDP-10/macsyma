;;;; -*- Mode:Lisp; Package:Macsyma; -*-

;;; Copied From CFFK

; Amplitude for Jacobian Elliptic functions. See A+S 16.4

(defun $am1 (u m1)
   (if (or (< m1 0.0) (> m1 1.0)) (error "Modulus of AM must lie in 0<m<1"))
   (phi u (if (= m1 0.0) 1.e-300 (sqrt m1)) (sqrt (- 1.0 m1)) 1.0))

(defun $am (u m) ($am1 u (- 1.0 m)))

(defun phi (u b ca a)
   (if (not (> (* ca a) 1.e-8)) (* a u)
       (let ((phi (phi (FSC u 1) (sqrt (* a b)) (setq ca (// (* 0.5 (- a b))
							     (setq a (* 0.5 (+ a b)))))
		       a)))
	    (* 0.5 (+ phi (asin (* ca (sin phi))))))))

(defun $sn (u m) (sin ($am u m)))

(defun $cn (u m) (cos ($am u m)))

(defun $dn (u m) (sqrt (- 1.0 (* m (^ ($sn u m) 2)))))

; ref Abramowitz and Stegun eqs 17.3.34 and 36
(defun $elliptk1 (m1)
   (if (or (not (> m1 0.0)) (> m1 1.0)) (error "Arg. out of range for ELLIPTK"))
   (+ (+ (* m1 (+ (* m1 (+ (* m1 (+ (* m1 0.01451196212)
				    0.03742563713))
			   0.03590092383))
		  0.09666344259))
	 1.38629436112)
      (* (+ (* m1 (+ (* m1 (+ (* m1 (+ (* m1 0.00441787012)
				       0.03328355346))
			      0.06880248576))
		     0.12498593597))
	    0.5)
	 (log (// m1)))))

(defun $elliptk (m) ($elliptk1 (- 1.0 m)))

(defun $ellipte1 (m1)
       (if (or (< m1 0.0) (> m1 1.0)) (error "Arg. out of range for ELLIPTE"))
       (+ (+ (* m1 (+ (* m1 (+ (* m1 (+ (* m1 0.01736506451)
					0.04757383546))
			       0.06260601220))
		      0.44325141463))
	     1.0)
	  (* (* m1 (+ (* m1 (+ (* m1 (+ (* m1 0.00526449639)
					0.04069697526))
			       0.09200180037))
		      0.24998368310))
	     (log (if (= m1 0.0) 1.0 (// m1))))))

(defun $ellipte (m) ($ellipte1 (- 1.0 m)))

;AGM method for ELLIPTK1.  Approx. 4 times longer than above method.
; maybe a bit more accurate.
(comment
(defun $k1 (m)
       (do ((a 1.0 (* 0.5 (+ a b)))
	    (b (if (= m 0.0) 1.e-300 (sqrt m)) (sqrt (* a b))))
	   ((not (> a b)) (// #,(atan 1.0 0.0) a)))))

; attempt at incomplete ell fun of first kind.  Srewed by ATAN not taking
;  right branch.  Not clear how to fix.
(comment
(defun $f1 (phi m1)
       (do ((n 0 (1- n))
	    (a 1.0 (* 0.5 (+ a b)))
	    (b (if (= m1 0.0) 1.e-300 (sqrt m1)) (sqrt (* a b))))
	   ((not (> a b)) (fsc (// phi a) n))
	   (setq phi (+ phi (atan (* (// b a) (sin phi))
				   (* (// b a) (cos phi))))))))


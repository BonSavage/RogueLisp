(in-package :rl.coordinates)

;;Pos

(defstruct (pos (:constructor make-pos (x y)) (:conc-name nil))
  (x 0 :type fixnum)
  (y 0 :type fixnum))

(defconstant +combinations+ (sort (remove (make-pos 0 0) (combine #'make-pos '(1 0 -1) '(1 0 -1)) :test #'equalp)
				  (lambda (p1 p2)
				    (declare (ignore p2))
				    (/= (+ (abs (x p1))  (abs (y p1))) 2))))

(defun neighbours(pos)
  (mapcar (curry #'add pos) +combinations+))

(defun neighbours-delta()
  +combinations+)

(defun pref(array p)
  (aref array (x p) (y p)))

(declaim (inline pref))

(defsetf pref(array p) (v)
  `(setf (aref ,array (x ,p) (y ,p)) ,v))

(defun in-first-quadrant-p(pos)
  (and (> (x pos) 0) (> (y pos) 0)))

(defun transpose(p)
  (make-pos (y p) (x p)))

(defun negate(p)
  (make-pos (- (x p)) (- (y p))))

;;Rect

(deftype coordinates() 'pos)

(defstruct (rect (:constructor make-rect (start size)))
  (start (make-pos 0 0) :type pos)
  (size (make-pos 0 0) :type pos))


(defun rect-center(rect)
  (add (start rect) (make-pos (round-down (half (x (size rect))))
			      (round-down (half (y (size rect)))))))
(defun start(rect)
  (rect-start rect))

(defun size(rect)
  (rect-size rect))

(defun end(rect)
  (sub (add (start rect) (size rect)) (make-pos 1 1)))

(defun add(p1 p2)
  (make-pos (+ (x p1) (x p2))
	    (+ (y p1) (y p2))))

(defun sub(p1 p2)
  (make-pos (- (x p1) (x p2))
	    (- (y p1) (y p2))))

(defun distance-point(p1 p2)
  (declaim (inline distance-point))
  (add (make-pos 1 1) (sub p1 p2)))

(defun in-bound-p(pos rect)
  (and (>= (x pos)
	   (x (start rect)))
       (>= (y pos)
	   (y (start rect)))
       (in-first-quadrant-p (sub (size rect) (sub pos (start rect))))))

(defun upper-slice(rect)
  (make-rect (start rect) (make-pos (-> rect size x) (round-up (half (-> rect size y))))))

(defun lower-slice(rect)
  (make-rect (make-pos (-> rect start x) (+ (-> rect start y) (round-up (half (-> rect size y)))))
	     (make-pos (-> rect size x) (round-down (half (-> rect size y))))))

(defun left-slice(rect)
  (make-rect (-> rect start) (make-pos (round-up (half (-> rect size x))) (-> rect size y))))

(defun right-slice(rect)
  (make-rect (make-pos (+ (-> rect start x) (round-up (half (-> rect size x)))) (-> rect start y))
	     (make-pos (round-down (half (-> rect size x))) (-> rect size y))))

(defun rect-scale(rect +x +y)
  (make-rect (start rect) (add (make-pos +x +y) (size rect))))

(defun iter-area(rect proc)
  (let-be [pos (make-pos 0 0)
	start (start rect)
	end (end rect)]
    (iter
      (for i from (x start) to (x end))
      (psetf (x pos) i)
      (iter
	(for j from (y start) to (y end))
	(psetf (y pos) j)
	(funcall proc pos)))))

(defmacro doarea((pos rect) &body forms)
  `(iter-area ,rect (lambda (,pos) ,@forms)))

(defun iter-rectangle(rect proc)
  (let-be [pos (make-pos 0 0)
	start (start rect)
	end (end rect)]
    (iter
      (for i from (x start) to (x end))
      (for j from (y start) to (y end))
      (iter
	(for by in (list (y start) (y end)))
	(for bx in (list (x start) (x end)))
	(psetf (x pos) i (y pos) by)
	(funcall proc pos)
	(psetf (x pos) bx (y pos) j)
	(funcall proc pos)))))

(defmacro dorectangle((pos rect) &body forms)
  `(iter-rectangle ,rect (lambda (,pos) ,@forms)))

;;Compass rose

(defstruct (compass-rose (:conc-name rose-) (:type list))
  (north)
  (south)
  (west)
  (east))



;;Pos sequence

(defun ptoc(p)
  (make-pos (car p) (cdr p)))

(defun ctop(p)
  (cons (x p) (y p)))

;;;Cell

(defun raw-line(delta)
  (let ((x (x delta))
	(y (y delta)))
    (mvb (dx dy) (let ((matches (> (abs x) (abs y))))
		   (values (if (or matches (zerop y)) (signum x) (/ x (abs y))) (if matches (/ y (abs x)) (signum y))))
	 (stream-drop
	  (lambda (pair) (and (= (car pair) x) (= (cdr pair) y)))
	  (alet ((cx dx) (cy dy))
	    (cons-stream (cons cx cy) (self (+ cx dx) (+ cy dy))))))))

(defun coordinate-test(c)
  (= (abs (rem c 1)) .5))

(defun cell-line(p0 p1) ;Must be refactored
  (let ((base-x (x p0))
	(base-y (y p0)))
    (stream-map
     (lambda (p &aux (x (car p)) (y (cdr p)) (x-test (coordinate-test x)) (y-test (coordinate-test y)))
       (cons (make-pos (round-down (+ base-x x)) (round-down (+ base-y y)))
	     (when (xor x-test y-test)
	       (list (make-pos (round-up (+ base-x x)) (round-up (+ base-y y)))))))
     (raw-line (sub p1 p0)))))

(defun cell-find-if(predicate ray)
  (find predicate (stream-find-if (lambda (list) (some predicate list)) ray)))

(defun cell-segment(p0 p1) ;For the future refactoring
  (stream-drop (lambda (p) (equalp p p1)) (cell-line p0 p1)))

(defun trace-line(predicate line)
  (pm/amatch (line)
	     self(nil) t
	     self((hd . tl)) (and (every predicate hd) (self (force tl)))))

(defun cast-ray(stop-predicate line)
  (pm/amatch (line)
	     self(nil) nil
	     self((hd . tl)) (cons-stream hd (if (some stop-predicate hd)
						 nil
						 (self (force tl))))))

(defun cell-find-before(stop-predicate line)
  (pm/amatch (line (stream-car line))
	     self(nil last) last
	     self((hd . tl) butlast) (if (some stop-predicate hd)
					 butlast
					 (self (force tl) hd))))
				 

(defun distance(pos1 pos2)
  "Cell distance"
  (awith (sub pos2 pos1)
    (max (abs (x it)) (abs (y it)))))

(defun pythagorean-distance(pos1 pos2)
  "Absolute distance"
  (awith (sub pos1 pos2)
    (sqrt (+ (expt (x it) 2) (expt (y it) 2)))))

(defmacro doray((pos line &optional end-form) &body forms)
  (alexandria:with-gensyms (elem temp)
    `(dostream (,elem ,line ,end-form)
       (with ,pos)
       (iter ,(gensym)
	 (for ,temp in ,elem)
	 (psetf ,pos ,temp)
	 ,@forms))))

(defun ray-reduce(f ray)
  (stream-reduce (lambda (elem last) (reduce f elem :initial-value last))))

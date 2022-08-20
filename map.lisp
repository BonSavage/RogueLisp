;;;; Map without entities. Only terrain.
;;;;

(in-package :rl.map)

;;(defconstant +size+ (make-pos 48 48) "Size of the map")
;;(defconstant +map-rect+ (make-rect (make-pos 0 0) +size+))
;;(defvar *map* nil)

(defvar +size+)
(defvar +map-rect+)
(defvar *map*)

(defun make-map-array(&rest args)
  (apply #'make-array (cons (list (x +size+) (y +size+)) args)))

;;Terrain

(defun attack-cell(pos damage)
  (hit-terrain (mref pos) damage))

(defun remove-decorator(terrain decorator)
  (cond ((eq terrain decorator)
	 (decorated terrain))
	((typep terrain 'terrain-decorator)
	 (psetf (decorated terrain) (remove-decorator (decorated terrain)
						      decorator)))
	(t terrain)))

(defun delete-decorator(pos decorator)
  (psetf (pref *map* pos)
	 (remove-decorator (mref pos) decorator)))

;;Map

(declaim (type (map-array terrain-info) *map*))

(defun mref(p)
  (declare (type rl.coordinates:pos p))
  (pref *map* p))

(defun in-map-bound-p(pos)
  (in-bound-p pos +map-rect+))

(defun grant-on-map(pos)
  (make-pos (grant-bounds (x pos) 0 (1- (x +size+)))
	    (grant-bounds (y pos) 0 (1- (y +size+)))))

;;Map accessors

;(defstruct (map-cell (:conc-name cell-))
;  (terrain nil :type terrain)
;  (pos nil :type pos))

(defun obstaclep(p)
  (terrain-obstaclep (mref p)))

(defun solidp(p)
  (terrain-solidp (mref p)))

(defun blockedp(p)
  (terrain-blockingp (mref p)))

(defun pos-gramma(p)
  (terrain-gramma (mref p)))

(defun pos-name(p)
  (terrain-name (mref p)))

(defun search-terrain(pos type)
  (alet [it (mref pos)]
    (cond
      ((typep it type) it)
      ((typep it 'terrain-decorator) (self (decorated it))))))

(defmacro with-terrain((terrain-name p &optional (type t)) &body forms)
  "Interface for map cell. ALWAYS interact with cell only in this context!"
  `(let* ((!terrain-pos! ,p)
	  (,terrain-name (search-terrain !terrain-pos! ,type)))
     (declare (special !terrain-pos!))
     ,@forms))

;;Map instance

(defmacro initiate-map(size initial-element)
  `(progn
     (defconstant +size+ ,size)
     (defconstant +map-rect+ (make-rect (make-pos 0 0) +size+))
     (defparameter *map* (make-map-array :initial-element ,initial-element :element-type 'terrain))
     (defparameter rl.light:*lightmap* (make-map-array :initial-element 0 :element-type 'bit))
     (deftype map-array(elem-type)
       `(array ,elem-type (,(x +size+) ,(y +size+))))))

;;Algorithms

(defun shadowcast(setter center &key (bound (lambda (i j) i)) (bound-test #'in-map-bound-p) (ctg-sequence '((-1 1) (-1 1) (-1 1) (-1 1))))
  "Classic shadowcast implementation"
  (iter 
    (for map-pos in (combine (lambda (s n) (lambda (p) (rl.coordinates:add center (funcall s (funcall n p)))))
			     (list #'identity #'rl.coordinates:transpose)
			     (list #'identity #'rl.coordinates:negate)))
    (for (init-start-ctg init-end-ctg) in ctg-sequence)
    (macrolet ((map-pos (p) `(funcall map-pos ,p))
	       (bounded-i (i) `(funcall bound ,i j))
	       (build-pos (x &optional (y 'j)) `(grant-on-map (map-pos (make-pos (bounded-i ,x) ,y)))))
      (alet [start-ctg init-start-ctg
	     end-ctg init-end-ctg
	     j 1]
	(let*((start-i (bounded-i (ceiling (* start-ctg j))))
	      (end-i (bounded-i (floor (* end-ctg j))))
	      (blockedp (solidp (grant-on-map (map-pos (make-pos start-i j))))))
	  (when (and (< start-ctg end-ctg) (funcall bound-test (map-pos (make-pos 0 j))))
	    (when (solidp (build-pos (round-down (* start-ctg j))))
	      ;;   (funcall setter (build-pos (round-down (* start-ctg j))))
	      (psetf start-ctg (grant-bounds (/ (- start-i .5) j) start-ctg end-ctg)))
	    (when (solidp (build-pos (round-up (* end-ctg j))))
	      ;;  (funcall setter (build-pos (round-up (* end-ctg j))))
	      (psetf end-ctg (grant-bounds (/ (+ end-i .5) j) start-ctg end-ctg)))
	    (iter (for i from start-i to end-i)
	      (awith (map-pos (make-pos i j))
		(when (in-map-bound-p it)
		  (funcall setter it)
		  (if (solidp it)
		      (unless blockedp
			(self start-ctg (- (/ (- i .5) j) short-float-epsilon) (1+ j))
			(psetf blockedp t))
		      (when blockedp ;And not obstaclep
			(psetf start-ctg (/ (+ (- i .5) short-float-epsilon) j)
			       blockedp nil)))))
	      (finally (unless blockedp (self start-ctg end-ctg (1+ j)))))))))))




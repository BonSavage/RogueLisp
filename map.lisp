;;;; Map without entities. Only terrain.
;;;; TODO: Separate from terrain just like level is separated from entity
;;;;

(in-package :rl.map)

(defconstant +size+ (make-pos 48 48) "Size of the map")
(defconstant +map-rect+ (make-rect (make-pos 0 0) +size+))
(defvar *map* nil)

(defun make-map-array(&rest args)
  (apply #'make-array (cons (list (x +size+) (y +size+)) args)))

(deftype map-array(elem-type)
  `(array ,elem-type (,(x +size+) ,(y +size+))))

;;Terrain
(defgeneric terrain-interact(terrain entity)
  (:method  (terrain entity) (declare (ignore terrain entity)) nil))
(defgeneric terrain-obstaclep(terrain))
(defgeneric terrain-solidp(terrain))
(defgeneric terrain-gramma(terrain))
(defgeneric terrain-name(terrain))

(defclass terrain()
  ((movement-coeff :initform 1 :allocation :class :reader movement-coeff)))

(defclass terrain-info(terrain)
  ((gramma :type rl.ui:gramma :reader terrain-gramma :initarg :gramma)
   (blockingp :type boolean :reader terrain-blockingp :initarg :blockingp :documentation "Used by pathfinding algorithms.")
   (obstaclep  :type boolean :reader terrain-obstaclep :initarg :obstaclep :documentation "General property used when the creature tries to move and in many other cases.")
   (solidp :type boolean :reader terrain-solidp :initarg :solidp)
   (name :type string :reader terrain-name :initarg :name)
   (movement-coeff :type real :initarg :movement-coeff)))

(eval-when (:compile-toplevel)
  (defmethod print-object((object terrain-info) stream)
    (format stream "<Terrain ~a>" (-> object name))))

(defmacro define-terrain(terrain-name &key gramma obstacle solid name (movement-coeff 1.0))
  `(eval-when (:compile-toplevel :execute :load-toplevel)
     (progn
       (setf (get ',terrain-name 'terrain ) (load-time-value (make-instance 'terrain-info :gramma ,gramma :obstaclep ,obstacle :solidp ,solid :blockingp ,obstacle :name ,name :movement-coeff ,movement-coeff) t))
       (export ',terrain-name)
       (deftype ,terrain-name()
	 `'(eql ,(get ',terrain-name 'terrain))))))

(define-terrain wall
  :gramma (rl.ui:static-gramma #\# (rl.ui:color :gray))
  :obstacle t
  :solid t
  :name "concrete wall")

(defmacro terrain(symbol)
  `(load-time-value (or (get ',symbol 'terrain) (error "MAP: Undefined terrain name: ~a" ',symbol))))

(defclass terrain-decorator(terrain)
  ((decorated :type terrain :initarg :decorated :accessor decorated)))

(defun attack-cell(pos count)
  (hit-terrain (mref pos) count pos))

(defun door-open(door pos)
  (psetf (openp door) t)
  (update-lights pos))

(defun door-close(door pos)
  (psetf (openp door) nil)
  (update-lights pos))

(defgeneric decorator-instance(decorator))

(defmethod decorator-instance(dec)
  dec)

(defun decorate-terrain(pos decorator-name &rest args)
  (psetf (pref *map* pos) (decorator-instance (apply #'make-instance decorator-name :decorated (mref pos) args))))

(defun remove-decorator(pos decorator)
  (psetf (pref *map* pos) (delete decorator (mref pos) :test #'eq)))

;;Map

(defparameter *map* (make-map-array :initial-element (terrain wall) :element-type 'terrain-info))
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

(defstruct (map-cell (:conc-name cell-))
  (terrain nil :type terrain)
  (pos nil :type pos))

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
    (cond ((typep it 'terrain-decorator) (self (decorated it)))
	  ((typep it type) (make-map-cell :terrain it :pos pos)))))

(defun open-door(cell)
  (door-open (cell-terrain cell) (cell-pos cell)))

(defun close-door(cell)
  (door-close (cell-terrain cell) (cell-pos cell)))

(defmethod destroyedp((cell map-cell))
  (destroyedp (cell-terrain cell)))

(defmethod openp((cell map-cell))
  (openp (cell-terrain cell)))

(defun interact(creature cell)
  (terrain-interact (cell-terrain cell) creature))

(defun map-cell(p)
  (make-map-cell :terrain (mref p) :pos p))

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




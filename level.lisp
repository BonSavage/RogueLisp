(in-package :rl.level)

;;Entities on the map

(defparameter *actor* nil)
(defparameter *entities* (make-hash-table :test 'equalp :size 40 :key-type 'pos :value-type 'list :rehash-size 1.5 :rehash-threshold .8))

(defgeneric get-entities(pos &optional type))
(defgeneric remove-entity(pos entity))
(defgeneric add-entity(pos entity))

(defmethod get-entities((pos pos) &optional type)
  (let ((lst (gethash pos *entities*)))
    (aif type
	 (remove-if (lambda (entity) (not (typep entity it))) lst)
	 lst)))

(defmacro do-entities((pvar entities &optional result) &body forms)
  `(dohash (,pvar ,entities *entities* ,result)
	   ,@forms))

(defun (setf get-entities)(new-val place)
  (setf (gethash place *entities*) new-val))

(defmethod remove-entity((pos pos) (entity entity))
  (when (null (setf (get-entities pos) (delete entity (get-entities pos) :count 1)))
    (rem-entities pos)))

(defmethod add-entity((pos pos) (entity entity))
  (push entity (gethash pos *entities*)))

(defun rem-entities(pos)
  (remhash pos *entities*))

(defun clear-level()
  (clrhash *entities*))

(defun pos-entities-alist()
  (iter
    (for (k v) in-hashtable *entities*)
    (collect (cons k v))))

;;;Map interface

(defun get-entities-gramma(entities)
  (localf f(type) (find-if (of-type type) entities)
	  (acond
	   ((f 'rl.entity:proto-creature) (rl.entity:get-gramma it))
	   ((f '(or rl.item:item-stack rl.entity:corpse))
	    (if (f 'rl.entity:proto-trap)
		(rl.ui:augment-gramma (rl.entity:get-gramma it) :background (rl.ui:color :orange))
		(rl.entity:get-gramma it)))
	   ((f 'rl.entity:proto-trap) (rl.entity:get-gramma it)))))

;;Lee

(defstruct (lee-map (:conc-name lee-))
  (array (make-map-array :element-type '(unsigned-byte 8)) :type (map-array (unsigned-byte 8)))
  (center (make-pos 0 0) :type pos))

(defun make-lee-check(lee-array)
  (lambda(pos heat)
    (and (in-map-bound-p pos) (not (blockedp pos)) (< (pref lee-array pos) heat) (/= heat 0))))

(defun near-step(lee-map pos)
  "Somewhat slow"
  (awith (lee-array lee-map)
    (find-best (lambda (p last) (and (in-map-bound-p p)
				     (or (> (pref it p) (pref it last))
					 (and (= (pref it p) (pref it last))
					      (or
					       (< (pythagorean-distance p (lee-center lee-map)) (pythagorean-distance last (lee-center lee-map)))
					       (rnd:bernoulli))))))
	       (cons pos (neighbours pos)))))

(defun near-ways(lee-map pos)
  (awith (lee-array lee-map)
	 (remove-if (lambda (p) (= (pref it p) 0)) (neighbours pos))))

(defun lee-implementation(lee-map lee-check)
  "Efficient Lee-algorithm implementation. Returns a thunk of type (or thunk nil). That thunk has the same type."
  (alambda (heat pos)
	   (declare (dynamic-extent pos heat) (optimize (speed 3)))
	   (when (funcall lee-check pos heat)
	     (setf (pref lee-map pos) heat)
	     (delay
	      (alet ((next (mapcar (curry #'self (1- heat)) (rl.coordinates:neighbours pos)))) ;This is the first self, not this one. Это вам тут не это.
		    (declare (dynamic-extent next))
		    (delay (if (null next)
			       nil
			       (self (mapcar #'force (remove nil next))))))))))

(defun lee(lee-map pos heat)
  "Lee-algorithm entry point. Currently it is bottleneck of performance."
  (psetf (lee-center lee-map) pos)
  (awith (lee-array lee-map)
	 (doarea (pos +map-rect+)
		 (setf (pref it pos) 0))
	 (alet ((thunk (funcall (lee-implementation it (make-lee-check it)) heat pos))) ;Looks like continuation-passing style
	       (declare (dynamic-extent thunk))
	       (when thunk
		 (self (force thunk))))))

(defun way-distance(pos lee-map &key center)
  (- (pref (lee-array lee-map) center)
     (if (rl.map:obstaclep pos)
	 (find-best #'> (mapcar (lambda (p) (pref (lee-array lee-map) p)) (neighbours pos)))
	 (pref (lee-array lee-map) pos))))


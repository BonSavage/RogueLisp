(in-package :rl.light)

(defparameter *lightmap* nil)
(declaim (type (map-array bit) *lightmap*))

;;;Light

(defstruct (light-source (:conc-name light-))
  (center (rl.coordinates:make-pos 0 0) :type pos)
  (radius 5 :type number))

(defvar *light-sources* nil)
(declaim (type (list light-source) *light-sources*))

(defun lights-intersectp(source1 source2)
  (<= (rl.coordinates:pythagorean-distance (light-center source1) (light-center source2))
      (abs (- (light-radius source1) (light-radius source2)))))

(defun light-neighbours(source)
  (remove-if (lambda (src) (or (eq source src) (not (lights-intersectp source src))))  *light-sources*))

(defun radius-bound(source &aux (radius2 (expt (light-radius source) 2)))
  (lambda (i j)
    (awith (isqrt (- radius2 (expt j 2)))
      (grant-bounds i (- it) it))))

(defun light-setter(source)
  (lambda (pos)
    (unless (rl.map:obstaclep pos)
      (setf (pref *lightmap* pos) 1))))

(defun shadow-setter(source)
  (lambda (pos)
    (setf (pref *lightmap* pos) 0)))

(defun in-radius(source)
  (lambda(p) (< (floor (rl.coordinates:pythagorean-distance p (light-center source))) (light-radius source))))

(defun light-on(source &optional (ctg-sequence '((-1 1) (-1 1) (-1 1) (-1 1))))
  (awith (light-setter source)
    (funcall it (light-center source))
    (rl.map:shadowcast it (light-center source) :bound (radius-bound source) :bound-test (in-radius source) :ctg-sequence ctg-sequence)))

(defun light-off(source)
  (fill-circle (shadow-setter source) source)
  (dolist (light (light-neighbours source))
    (light-on light)))

(defun update-light(source)
  (light-on source))

(defun add-light(source)
  (push source *light-sources*)
  (light-on source))

(defun remove-light(source)
  (setf *light-sources* (delete source *light-sources* :test #'eq :count 1))
  (light-off source))

(defun replace-light(source pos)
  (light-off source)
  (setf (light-center source) pos)
  (light-on source))

(defun relight-area(source)
  (light-off source)
  (light-on source))

(defun update-lights(pos)
  "Must be optimized"
  (awith (remove-if (complement (lambda (src) (funcall (in-radius src) pos))) *light-sources*)
    (dolist (src it)
      (light-off src))
    (dolist (src it)
      (light-on src))))

(defun create-light(pos radius)
  (awith (make-light-source :center pos :radius radius)
    (add-light it)
    it))

(defun litp(p)
  (not (zerop (pref *lightmap* p))))

(defun fill-circle(setter source &aux (radius (light-radius source)) (center (light-center source)) (radius2 (expt radius 2)))
  (let-be [pos (make-pos 0 0)]
    (iter
      (for i from (- radius) to radius)
      (awith (isqrt (- radius2 (expt i 2)))
	(iter
	  (for j from (- it) to it)
	  (funcall setter (grant-on-map (add center (amutf (x pos) i (y pos) j)))))))))

(in-package :rl.perception)

;;Actor vision
(defstruct vision
  (plan (make-map-array :element-type 'bit) :type (map-array bit))
  (visible (make-map-array :element-type 'bit) :type (map-array bit)))

(defun make-fov()
  (make-vision))

(defun vision-visiblep(vision p)
  (= (pref (vision-visible vision) p) 1))

(defun make-visible-setter-debug(fov center)
  (declare (ignore center))
  (lambda (pos)
    (setf (pref (vision-visible fov) pos) 1)))

(defun make-visible-setter(fov center)
  (with-accessors ((visible vision-visible)
		   (plan vision-plan))
      fov
    (lambda (pos)
      (awith (or (solidp pos) (obstaclep pos))
	(if (or (litp pos)
		(and it (every (lambda (pos) (= (pref visible pos) 1)) (stream-car (geom:cell-line pos center)))))
	    (progn
	      (setf (pref visible pos) 1)
	      (setf (pref plan pos) (if (and it (blockedp pos)) 1 0))))))))

;;Shadowcast algorithm
(defun fov-shadowcast(fov center)
  "Entry point of the shadowcast algorithm"
  (awith (vision-visible fov)
	 (doarea (pos rl.map:+map-rect+)
		 (setf (pref it pos) 0)))
  (awith (make-visible-setter fov center)
	 (setf (pref (vision-visible fov) center) 1)
	 (rl.map:shadowcast it center)))
;;Fov info

(defstruct (mark (:constructor make-mark(gramma text)))
  (gramma nil :type rl.ui:gramma)
  (text "" :type string))

(defgeneric get-fov(obj))

(defclass fov-info()
  ((fov :type vision :initarg :fov :reader get-fov)
   (center :type pos :reader get-center :initarg :center)
   (marks :initarg :marks :reader fov-marks)
   (entity-positions :initform nil :type list :reader fov-entity-positions :initarg :entity-positions)))

(defmethod visiblep((info fov-info) pos)
  (= (pref (-> info fov visible) pos) 1))

(defmethod seenp((info fov-info) pos)
  (= (pref (-> info fov plan) pos) 1))

(defun visible-entities(fov)
  (mappend #'cdr (fov-entity-positions fov)))

(defun fov-pos-entities(fov pos)
  (cdr (assoc pos (-> fov entity-positions) :test #'equalp)))

(defun update-vision(fov pos)
  (fov-shadowcast fov pos)
  fov)				      
				      
(defun get-gramma(fov-info pos)
  (declare (type fov-info fov-info))
  (cond ((visiblep fov-info pos)
	 (rl.map:pos-gramma pos))
	(t (get-plan-gramma fov-info pos))))

(defun get-plan-gramma(fov-info pos)
  (if (seenp fov-info pos)
      (rl.ui:static-gramma (code-char 219) (rl.ui:color :dark-gray))
      (rl.ui:static-gramma #\Space (rl.ui:color :black))))

(defun fov-pos-description(fov-info pos)
  (acond
    ((not (visiblep fov-info pos)) "I have no vision here.")
    ((fov-pos-entities fov-info pos) (formatted "~{~a~} on ~a" (mapcar #'rl.entity:get-name it) (rl.map:pos-name pos)))
    (t (rl.map:pos-name pos))))

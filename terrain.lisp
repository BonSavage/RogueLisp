(in-package :rl.terrain)

;;Terrain itself

(defgeneric terrain-interact(terrain entity)
  (:method  (terrain entity) (declare (ignore terrain entity)) nil))
(defgeneric terrain-obstaclep(terrain))
(defgeneric terrain-solidp(terrain))
(defgeneric terrain-gramma(terrain))
(defgeneric terrain-name(terrain))

(defgeneric hit-terrain(terrain damage))

(defun terrain-pos(terrain)
  (declare (special !terrain-pos!))
  !terrain-pos!)

(defclass terrain()
  ((movement-coeff :initform 1 :allocation :class :reader movement-coeff)))

(eval-when (:compile-toplevel)
  (defmethod print-object((object terrain) stream)
    (format stream "<Terrain ~a>" (terrain-name object))))

(defclass terrain-decorator(terrain)
  ((decorated :type terrain :initarg :decorated :accessor decorated)))

(defgeneric openp(door))
(defgeneric (setf openp)(door val))

(defclass proto-door(terrain)
  ((blockingp :type boolean :reader terrain-blockingp :initform nil :allocation :class)))

(defmethod terrain-obstaclep((terrain proto-door))
  (not (openp terrain)))

(defmethod terrain-solidp((terrain proto-door))
  (not (openp terrain)))

;;

(defun %add-terrain(terrain-name terrain-instance)
  (setf (get terrain-name 'terrain) terrain-instance))

(defmacro terrain(symbol)
  `(load-time-value (or (get ',symbol 'terrain) (error "MAP: Undefined terrain name: ~a" ',symbol))))

;;

(defgeneric decorator-instance(decorator))

(defmethod decorator-instance(dec)
  dec)

(defun open-door(door)
  (psetf (openp door) t)
  (rl.light:update-lights (terrain-pos door)))

(defun close-door(door)
  (psetf (openp door) nil)
  (rl.light:update-lights (terrain-pos door)))

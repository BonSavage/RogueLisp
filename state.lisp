(in-package :rl.state)

;;States of creatures
(defgeneric state-execute(state turn))
(defgeneric init-state(state creature))
(defgeneric state-end(time-state))

(defclass standard-state() ())

(defclass dead(standard-state) ())

(defclass time-state()
  ((duration :initarg :duration :reader state-duration)))

(defun make-state(type &rest other-args)
  (apply #'make-instance type other-args))

(defmethod init-state(state creature)
  nil)

(defmethod state-execute((state standard-state) turn)
  (take-turn (entity turn) turn))

(defmethod state-end((state time-state))
  (make-state 'standard-state))

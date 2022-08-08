;;;;Buffs & debuffs
(in-package :rl.effect)

;;(defgeneric combine-effects(ef1 ef2))
(defgeneric update-effect-state(ef delta))
(defgeneric effect-activep(ef))

(defclass effect()
  ())

(defclass resistance(effect)
  ((res-type :initform nil :type (or symbol list) :initarg :from :reader resistance-type)))

(defclass durable-effect(effect)
  ((duration :initarg :duration :accessor effect-duration)))

(defun make-effect(type &rest args)
  (apply #'make-instance type args))

(defmethod effect-activep(effect)
  t)

(defmethod effect-activep((effect durable-effect))
  (> (effect-duration effect) 0))

(defmethod update-effect-state(effect delta)
  nil)

(defmethod update-effect-state((effect durable-effect) delta)
  (decf (effect-duration effect) delta))

(in-package :rl.entity)

;;Interface

;;Predicates
(defgeneric can-move-p(creature dir))
(defgeneric seesp(creature what))
;;Selectors
(defgeneric get-melee-damage(creature))
(defgeneric get-weapon(humanoid))
(defgeneric get-hp(creature))
(defgeneric get-max-hp(creature))
(defgeneric get-speed(creature))
(defgeneric get-dodge-bonus(creature))
(defgeneric get-memory(creature))
(defgeneric get-protection(creature))
(defgeneric get-gramma(entity))
;;Primitive mutators (used by event commands)
(defgeneric perform-movement(creature delta))
(defgeneric die(creature)) 
(defgeneric take-damage(creature damage-info)) ;TODO: Must be generic for any entity
(defgeneric decrease-health(creature count))

;;Classes

;;And generic class

(defclass proto-creature(entity)
  ((state :initform (rl.state:make-state 'rl.state:standard-state) :type state :initarg :state :accessor get-state)
   (effects :initform nil :type list :accessor get-effects)))

(defun make-creature(type &rest args)
  (awith (apply #'make-instance type args)
    (rl.level:add-entity (get-pos it) it)
    it))

(defun set-state(creature state)
  (setf (get-state creature) state)
  (rl.state:init-state state creature))

(defmethod has-effect(creature effect)
  (find-if (of-type effect) (get-effects creature)))

(defclass corpse(entity)
  ((gramma :initform (rl.ui:static-gramma #\& (rl.ui:layer-color :white)) :initarg :gramma)
   (entity :initarg :entity :reader corpse-owner)))

(defmethod get-name((entity corpse))
  (formatted "a corpse of ~a" (get-name (corpse-owner entity))))

;;Updates (pseudo-hooks)

(defgeneric on-turn-start(creature))
(defgeneric on-turn-end(creature delta))
(defgeneric on-position-change(creature delta))

;;Oh, life and death...

(defmethod spawn-corpse((creature proto-creature))
  (awith (make-instance 'corpse :gramma (rl.ui:make-gramma (char-code #\&) (rl.ui:gramma-color (get-gramma creature))) :entity creature)
    (rl.level:add-entity (get-pos creature)
		      it)
    it))

(defmethod die((creature proto-creature))
  (unless (alivep creature)
    (report-death creature)
    (rl.level:remove-entity (get-pos creature) creature)
    (set-state creature (rl.state:make-state 'rl.state:dead))
    (spawn-corpse creature)))

(defmethod alivep(creature)
  (and (not (typep (get-state creature) 'rl.state:dead))
       (> (get-hp creature) 0)))

;;Movement

(defmethod perform-movement((creature proto-creature) (delta pos))
  (progn
    (rl.level:remove-entity (get-pos creature) creature)
    (amutf (get-pos creature) (add it delta))
    (rl.level:add-entity (get-pos creature) creature)))

(defmethod perform-movement :after ((creature proto-creature) delta)
  (on-position-change creature delta))

(defmethod can-move-p((creature proto-creature) (dir pos))
  (and (null (rl.level:get-entities (add (get-pos creature) dir) 'proto-creature)) (not (rl.map:obstaclep (add (get-pos creature) dir)))))

;;Damage & combat

(defmethod take-damage(attackee damage-info)
  (awith (rl.combat:calculate-attack damage-info (get-protection attackee))
    (decrease-health attackee it)
    it))

(defmethod decrease-health(attackee count)
  (when (> count 0)
    (decf (get-hp attackee) count)))

(defmethod decrease-health :after(attackee count)
  (when (not (alivep attackee))
    (die attackee)))

;;Perception

(defmethod seesp(caller (_ null))
  nil)

(defmethod seesp(caller (creature proto-creature))
  "Standard LOS-based algorithm"
  (seesp caller (get-pos creature)))

(defmethod seesp(caller (cell pos))
  (geom:trace-line (lambda (p) (not (rl.map:solidp p)))
		   (geom:cell-line cell (get-pos caller))))

;;Effects

(defgeneric invoke-effect(creature effect))

;;Traps

(defclass proto-trap(entity)
  ())

;;;Entity

(defclass entity()
  ((gramma :reader get-gramma :allocation :class)
   (name)))

(defmethod get-name((entity entity))
  (get-singular-name entity))

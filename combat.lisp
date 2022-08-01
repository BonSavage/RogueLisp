(in-package :rl.combat)

(defstruct (basic-damage (:constructor damage (damage &optional (hit-check 0) (type 'mechanic))) (:conc-name damage-))
  (damage (rnd:dices 0 0) :type rnd:dices)
  (hit-check 0 :type fixnum))

(defun damage-count(info)
  (rnd:throw-dices (damage-damage info)))

(defun dodgesp(damage dodge-check)
  (<= (random (+ 100 (damage-hit-check damage) (- dodge-check)))
      50))

(defun calculate-attack(damage armor)
  (- (damage-count damage)
     armor))

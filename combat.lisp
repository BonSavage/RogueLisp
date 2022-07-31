(in-package :rl.combat)

(defstruct (basic-damage (:constructor damage (damage hit-check &optional (type 'mechanic))) (:conc-name damage-))
  (damage (rnd:dices 0 0) :type rnd:dices)
  (hit-check (rnd:dices 1 6) :type rnd:dices))

(defun damage-count(info)
  (rnd:throw-dices (damage-damage info)))

(defun dodgesp(damage dodge-coeff)
  (<= (rnd:throw-dices (damage-hit-check damage)) (rnd:throw-dices dodge-coeff)))

(defun calculate-attack(damage armor)
  (- (damage-count damage)
     armor))

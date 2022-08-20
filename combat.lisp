(in-package :rl.combat)

(defstruct (basic-damage (:conc-name damage-))
  (damage (rnd:dices 0 0) :type rnd:dices)
  (hit-check 0 :type fixnum))

(defun damage-count(info)
  (rnd:throw-dices (damage-damage info)))

(defun dodgesp(damage dodge-check)
  (< (random 100)
      (grant-bounds (+ 50 dodge-check (- (damage-hit-check damage)))
		    0
		    100)))
		    

(defun calculate-attack(damage armor)
  (- (damage-count damage)
     armor))

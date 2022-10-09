(in-package :rl.combat)

(defstruct (basic-damage (:conc-name damage-))
  (damage (rnd:dices 0 0) :type rnd:dices)
  (hit-check 0 :type fixnum))

(defun damage-count(info)
  (rnd:throw-dices (damage-damage info)))

(defun hit-check-bounds(hit-check)
  (grant-bounds hit-check 0 100))

(defun damage-real-hit-check(dmg)
  (hit-check-bounds (+ 50 (damage-hit-check dmg))))

(defun dodgesp(damage dodge-check)
  (< (random 100)
     (hit-check-bounds (+ 50 dodge-check (- (damage-hit-check damage))))))

(defun calculate-attack(damage armor)
  (- (damage-count damage)
     armor))

;;Ranged

(defgeneric projectile-trajectory(projectile))
(defgeneric projectile-hit(projectile creature))

(defclass projectile()
  ((start :initarg :start :reader projectile-start)
   (end :initarg :end :reader projectile-end)))

(defmethod projectile-trajectory((projectile projectile))
  (geom:cell-line (projectile-start projectile) (projectile-end projectile)))

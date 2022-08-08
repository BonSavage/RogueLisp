(in-package :rl.item)

;;Generic class

(defclass item()
  ((gramma :allocation :class :reader get-gramma)
   (condition :initform 1 :type real :initarg :condition)))

;;Methods

(defclass weapon(item)
  ((melee-damage :allocation :class :reader get-melee-damage)
   (melee-speed :initform 1.0 :allocation :class :reader get-melee-speed)))

(defclass melee(weapon)
  ())

(defclass usable(item) ())

(defclass armor(item)
  ((protection :reader get-protection :allocation :class)))

(defgeneric same-item-p(item1 item2))
(defgeneric use(item stack user))
(defgeneric reload(firearm ammo-stack))
(defgeneric take-shot(firearm))
(defgeneric get-melee-damage(weapon))
(defgeneric get-speed(weapon))
(defgeneric use(item stack user))

(defmethod use((item usable) stack user)
  (declare (ignore item))
  (rl.inventory:take-stack (rl.entity:get-inventory user) (item-stack item)))

(defun stack-use(stack user)
  (use (stack-item stack) stack user))

(defmethod get-description((item item))
  (formatted "~a~&Weight: ~a~&Condition: ~a%~&" (-> item description) (-> item weight) (* 100(-> item condition))))

(defmethod copy-item(item)
  (make-instance (type-of item) :condition (-> item condition)))

(defmethod get-melee-damage(weapon)
  (-> weapon melee-damage))

(defmethod get-speed(weapon)
  (-> weapon speed))

;;Interface

(defgeneric stack-count(stack))
(defgeneric item-name(stack))
(defgeneric item-description(stack))
(defgeneric same-item-p(stack1 stack2))
(defgeneric merge-stacks(stack1 stack2))
(defgeneric item-actions(stack creature))

;;Item stack

(defclass item-stack(entity)
  ((item :type item :initarg :item)
   (count :initform 1 :type fixnum :initarg :count)))

(defun make-item(type &rest key-pairs)
  (apply #'make-stack (list* type 1 key-pairs)))

(defun make-stack(type count &rest key-pairs)
  (make-instance 'item-stack :item (apply #'make-free-item (cons type key-pairs)) :count count))

(defun make-free-item(type &rest key-pairs)
  (apply #'make-instance (cons type key-pairs)))

(defmethod same-item-p((item1 item) (item2 item))
  (and (eq (class-of item1) (class-of item2)) (= (-> item1 condition) (-> item2 condition))))

(defmethod stack-count((stack item-stack))
  (-> stack count))

(defmethod get-name((stack item-stack))
  (with-slots (count item) stack
    (if (> count 1)
	(formatted "~a ~a" count (get-plural-name (-> stack item)))
	(get-singular-name (-> stack item)))))

(defmethod get-description((stack item-stack))
  (get-description (-> stack item)))

(defmethod item-description((stack item-stack))
  (get-description (-> stack item)))

(defmethod merge-stacks((stack1 item-stack) (stack2 item-stack))
  (assert (same-item-p (-> stack1 item) (-> stack2 item)))
  (amutf (-> stack1 count) (+ it (-> stack2 count))
	 (-> stack2 count) 0)
  stack1)

(defmethod same-item-p((stack1 item-stack) (stack2 item-stack))
  (same-item-p (-> stack1 item) (-> stack2 item)))

(defmethod get-gramma((stack item-stack))
  (get-gramma (-> stack item)))

(defmethod item-name((stack item-stack))
  (-> stack item name))

(defmethod free-item((stack item-stack))
  (progn
    (decf (-> stack count))
    (copy-item (-> stack item))))

(defun stack-type(stack creature)
  (type-of (-> stack item)))

(defun stack-item(stack)
  (-> stack item))

(defun item-stack(item)
  (make-instance 'item-stack :item item :count 1))

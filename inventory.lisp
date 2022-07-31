(in-package :rl.inventory)

(deftype backpack()
  '(list item-stack))

(defun add-stack(inventory item-stack)
  (unless (zerop (-> item-stack count))
    (aif (find-if (lambda (istack) (and istack (same-item-p (-> item-stack item) (-> istack item))))(inventory-backpack inventory))
	 (merge-stacks it item-stack)
	 (push item-stack (inventory-backpack inventory)))))

(defun remove-stack(inventory item-stack)
  (amutf (inventory-backpack inventory) (delete item-stack it :count 1 :test #'eq)))

(defun take-stack(inventory stack &key (test #'same-item-p))
  "Takes stack by non strict pattern"
  (aif (find (stack-item stack) (inventory-backpack inventory) :key #'stack-item :test test)
       (relation-case ((-> stack count) (-> it count))
		      (< (progn
			   (decf (-> it count) (-> stack count))
			   stack))
		      (= (progn
			   (remove-stack inventory it)
			   stack))
		      (> (progn
			   (remove-stack inventory it)
			   it)))))

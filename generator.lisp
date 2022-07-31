;;;;Generator
;;;;

(in-package :rl.generator)
		  
;;;Keys & doors


(defun make-door(deepness)
  (if (< (random (1+ deepness)) 7)
      (let-be [next (make-door (1+ deepness))]
	(if (rnd:bernoulli)
	    (list next nil)
	    (list nil next)))
      inftree))

;;;Data types

(defstruct (compass-rose (:conc-name rose-) (:type list))
  (north)
  (south)
  (west)
  (east))

(defstruct (room-data (:conc-name room-) (:include rect))
  (doors (make-compass-rose) :type compass-rose))

(defun key-doorpos(key length)
  (1+
   (pm/amatch (key (- length 2))
	      self(?inftree length) (grant-bounds (+ (nhalf length) (1- (random 3)))
						  0
						  (max 1 length))
	      self((nil nil) length) (nhalf length)
	      self((first nil) length) (self first (round-down (/ length 2)))
	      self((nil second) length) (+ (round-up (/ length 2)) (self second (round-down (/ length 2)))))))

(defun make-tunnel(rect doors)
  (let-be [length (1- (x (size rect)))
	   height (1- (y (size rect)))
	   nx (+ 1 (x (start rect)) (key-doorpos (rose-north doors) length))
	   sx (+ 1 (x (start rect)) (key-doorpos (rose-south doors) length))
	   wy (+ 1 (y (start rect)) (key-doorpos (rose-west doors) height))
	   ey (+ 1 (y (start rect)) (key-doorpos (rose-east doors) height))
	   center1 (make-pos (min nx sx) (min wy ey))
	   center2 (make-pos (max nx sx) (max wy ey))]
    (remove nil (list
		 (make-room-data :start center1
				 :size (distance-point center2 center1)
				 :doors (list nil nil nil nil))
		 (when (rose-north doors) (make-room-data :start (make-pos nx (y (start rect)))
							  :size (make-pos 1 (y (distance-point center1 (start rect))))
							  :doors (list nil nil nil nil)))
		 (when (rose-south doors) (make-room-data :start (make-pos sx (y center2))
							  :size (make-pos 1 (1+ (y (distance-point (end rect) center2))))
							  :doors (list nil nil nil nil)))
		 (when (rose-west doors) (make-room-data :start (make-pos (x (start rect)) wy)
							 :size (make-pos (x (distance-point center1 (start rect))) 1)
							 :doors (list nil nil nil nil)))
		 (when (rose-east doors) (make-room-data :start (make-pos (x center2) ey)
							 :size (make-pos (1+ (x (distance-point (end rect) center2))) 1)
							 :doors (list nil nil nil nil)))))))



(defun make-room(rect doors)
  (symbol-macrolet ((length (x (size rect)))
		    (height (y (size rect))))
    (let* ((opens (make-compass-rose :north (and (rose-north doors) (< length height) (rnd:bernoulli 5))
				     :south (and (rose-south doors) (< length height) (rnd:bernoulli 5))
				     :west (and (rose-west doors) (< height length) (rnd:bernoulli 5))
				     :east (and (rose-east doors) (< height length) (rnd:bernoulli 5))))
	   (inferior-start (add (start rect)
				(make-pos (if (rose-west opens) 0 1)
					  (if (rose-north opens) 0 1))))
	   (inferior-end (add (end rect)
			      (make-pos (if (rose-east opens) 1 0)
					(if (rose-south opens) 1 0)))))
	(make-room-data :start inferior-start :size (distance-point inferior-end inferior-start)
			:doors (mapcar (lambda (door openedp pos) (when (and door (not openedp))
								    pos))
				       doors opens
				       (make-compass-rose :north (make-pos (+ (x (start rect)) (key-doorpos (rose-north doors) (1- length)))
									   (1- (y inferior-start))) ;Need modf!
							  :south (make-pos (+ (x (start rect)) (key-doorpos (rose-south doors) (1- length)))
									   (1+ (y inferior-end)))
							  :west (make-pos (1- (x inferior-start))
									  (+ (y (start rect)) (key-doorpos (rose-west doors) (1- height))))
							  :east (make-pos (1+ (x inferior-end))
									  (+ (y (start rect)) (key-doorpos (rose-east doors) (1- height))))))))))

;;Room list
(defun make-room-list(rect &optional (deepness 0) (keys (make-compass-rose :north (make-door 2) :south (make-door 2))) doors)
  (format t "Seed: ~a ~%" *random-state*) ;TODO: Must be room tree
  (mapcar (lambda (room) (list (make-rect (start room) (size room)) (-> room doors))) (split-room rect deepness keys doors)))


;;Generator


(defun north(vkey)
  (first vkey))

(defun south(vkey)
  (second vkey))

(defun west(hkey)
  (first hkey))

(defun east(hkey)
  (second hkey))

;;Sector
(defstruct (sector-tree (:conc-name tree-)) ;This is where ML is better
  (type 'vertical :type (member vertical horizontal))
  (first-branch nil :type (or sector-tree t null))
  (second-branch nil :type (or sector-tree t null)))

(defun print-tree(tree deepness stream)
  (dotimes (i deepness)
    (format stream "-"))
  (if (typep tree 'sector-tree)
      (progn
	(format stream "~a" (tree-type tree))
	(format stream "~&")
	(print-tree (tree-first-branch tree) (1+ deepness) stream)
	(format stream "~&")
	(print-tree (tree-second-branch tree) (1+ deepness) stream))
      (format stream " ~a" tree)))

(defmethod print-object((object sector-tree) stream)
  (print-tree object 0 stream))

(defun vertical-sectorp(sector)
  (eq (tree-type sector) 'vertical))

(defun horizontal-sectorp(sector)
  (eq (tree-type sector) 'horizontal))

(deftype vertical-sector()
  '(and sector-tree (satisfies vertical-sectorp)))

(deftype horizontal-sector()
  '(and sector-tree (satisfies horizontal-sectorp)))

(defun split-horizontal(rect deepness)
  (let-be [left (left-slice rect)
	   right (right-slice rect)]
    (make-sector-tree :type 'horizontal
		      :first-branch (split-area left (1+ deepness) 'horizontal)
		      :second-branch (split-area right (1+ deepness) 'horizontal))))

(defun split-vertical(rect deepness)
  (let-be [upper (upper-slice rect)
	   lower (lower-slice rect)]
    (make-sector-tree :type 'vertical
		      :first-branch (split-area upper (1+ deepness) 'vertical)
		      :second-branch (split-area lower (1+ deepness) 'vertical))))

(defun split-area(rect deepness previous)
  (if (or (< (- (random 5) deepness) -2) (< (x (size rect)) 6) (< (y (size rect)) 6))
      rect
      (awith (random 1.0)
	(if (< it (if (eq previous 'vertical) .35 .65))
	    (split-vertical rect deepness)
	    (split-horizontal rect deepness)))))

;;Keys and doors
(defstruct links
  (keys nil :type compass-rose)
  (doors nil :type compass-rose))

(defstruct (linked-sector (:conc-name sector-))
  (area nil :type rect)
  (keys nil :type compass-rose)
  (doors nil :type compass-rose))

(localf shift (rose &key ;... and this is where Lisp is better than ML
		    (north #'identity)
		    (south #'identity)
		    (west #'identity)
		    (east #'identity))
    (make-compass-rose :north (funcall north (rose-north rose))
		       :south (funcall south (rose-south rose))
		       :west (funcall west (rose-west rose))
		       :east (funcall east (rose-east rose)))
  (defun rose-shift(rose side-symbol counter-side-value &aux (delayed (lambda (&rest args) counter-side-value)))
    (ecase side-symbol
      (:north (shift rose :west #'north :east #'north :south delayed))
      (:south (shift rose :west #'south :east #'south :north delayed))
      (:west (shift rose :south #'west :north #'west :east delayed))
      (:east (shift rose :south #'east :north #'east :west delayed)))))

(defun internal-keys(deepness &key first-side second-side)
  (let-be [internal-link (if (or (and (car first-side) (cadr first-side))
				 (and (car second-side) (cadr second-side)))
			     nil
			     t)
	   first-link (when internal-link (rnd:bernoulli 2))
	   second-link (when internal-link (not first-link))
	   first-key (when first-link (make-door deepness))
	   second-key (when second-link (make-door deepness))]
    (values first-key second-key)))

(defun final-rose(&key north south west east)
  (make-compass-rose :north north :south south :west west :east east))

(defun link-vertical(tree deepness keys doors)
  (let-be [north (tree-first-branch tree)
	   south (tree-second-branch tree)
	   (north->south south->north) (internal-keys deepness :first-side (rose-west keys) :second-side (rose-east keys))]
    (if north->south
	      (let-be [(new-north north-doors) (link-sectors north
							     (1+ deepness)
							     (rose-shift keys :north nil)
							     (rose-shift doors :north north->south))
		       (new-south south-doors) (link-sectors south
							     (1+ deepness)
							     (rose-shift keys :south (rose-south north-doors))
							     (rose-shift doors :south nil))]
		(values
		 (make-sector-tree :type 'vertical
				   :first-branch new-north
				   :second-branch new-south)
		 (final-rose :north (rose-north north-doors)
			     :west (rose-west doors)
			     :east (rose-east doors)
			     :south (rose-south south-doors))))
	      ;;else: south->north or nothing
	      (let-be [(new-south south-doors) (link-sectors south
					      (1+ deepness)
					      (rose-shift keys :south nil)
					      (rose-shift doors :south south->north))
		    (new-north north-doors) (link-sectors north
					      (1+ deepness)
					      (rose-shift keys :north (rose-north south-doors))
					      (rose-shift doors :north nil))]
		(values
		 (make-sector-tree :type 'vertical
				   :first-branch new-north
				   :second-branch new-south)
		 (final-rose :north (rose-north north-doors)
			     :west (rose-west doors)
			     :east (rose-east doors)
			     :south (rose-south south-doors)))))))

(defun link-horizontal(tree deepness keys doors)
  (let-be [west (tree-first-branch tree)
	   east (tree-second-branch tree)
	   (west->east east->west) (internal-keys deepness :first-side (rose-north keys) :second-side (rose-south keys))]
    (if west->east
	(let-be [(new-west west-doors) (link-sectors west
						     (1+ deepness)
						     (rose-shift keys :west nil)
						     (rose-shift doors :west west->east))
		 (new-east east-doors) (link-sectors east
						     (1+ deepness)
						     (rose-shift keys :east (rose-east west-doors))
						     (rose-shift doors :east nil))]
	  (values
	   (make-sector-tree :type 'horizontal
			     :first-branch new-west
			     :second-branch new-east)
	   (final-rose :north (rose-north doors)
			      :west (rose-west west-doors)
			      :east (rose-east east-doors)
			      :south (rose-south doors))))
	;;else: east->west or nothing
	(let-be [(new-east east-doors) (link-sectors east
						     (1+ deepness)
						     (rose-shift keys :east nil)
						     (rose-shift doors :east east->west))
		 (new-west west-doors) (link-sectors west
						     (1+ deepness)
						     (rose-shift keys :west (rose-west east-doors))
						     (rose-shift doors :west nil))]
	  (values
	   (make-sector-tree :type 'horizontal
			     :first-branch new-west
			     :second-branch new-east)
	   (final-rose :north (rose-north doors)
			      :west (rose-west west-doors)
			      :east (rose-east east-doors)
			      :south (rose-south doors)))))))

(defun link-sectors(sector deepness keys doors)
  (if (and (<= (count-of doors) 1) (not (any keys)))
      (format t "Removed ~a ~&" sector)
      (etypecase sector
	(rect (values
	       (make-linked-sector :area sector
				   :keys keys
				   :doors doors)
	       doors))
	(vertical-sector (link-vertical sector deepness keys doors))
	(horizontal-sector (link-horizontal sector deepness keys doors)))))

;;Contents (monsters, treasures)



;;Rooms

(defun build-area(linked-sector)
  (when (or (> (count-of (sector-doors linked-sector)) 1) (any (sector-keys linked-sector)))
    (cond ((not (any (sector-keys linked-sector)))
	   (make-tunnel (sector-area linked-sector) (sector-doors linked-sector)))
	  (t (list (make-room (sector-area linked-sector) (sector-doors linked-sector)))))))

(defun build-sector(sector)
  (etypecase sector
    (sector-tree (append (build-sector (tree-first-branch sector)) (build-sector (tree-second-branch sector))))
    (linked-sector (build-area sector))
    (null nil)))

;;Split
(defun split-room(rect deepness keys doors)
  (build-sector (link-sectors (split-area rect deepness 'horizontal)
			      deepness
			      keys
			      doors)))

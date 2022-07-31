(in-package :rl.event)
;;;; Event facility
;;;; While entities have primitive mutators and action-performers, this facility arranges them in time.

;;;Main

(defclass event()
  ((energy :initform 0 :type fixnum :accessor event-energy :initarg :energy)))

(defvar *next-list* '())
(defvar *zero-energy* 0)

(defmacro in-event-context(form)
  (alexandria:with-gensyms (blck retry)
    `(block ,blck
       (tagbody ,retry
	  (restart-case
	      (return-from ,blck ,form)
	    (retry-event ()
	      :report "Retry last event"
	      (go ,retry))
	    (abandon ()
	      :report "Forget last event"
	      nil))))))

(defun process-events(&aux (event-list *next-list*))
  (psetf *next-list* nil)
  (iterate (while event-list) 
    (let-be [chosen-event (pop (if (and *next-list* (< (event-energy (car *next-list*)) (event-energy (car event-list))))
				   *next-list*
				   event-list))
	     *zero-energy* (event-energy chosen-event)
	     new-list (aif (in-event-context (exec chosen-event))
			   (add-before (let ((e (event-energy it)))
					 (amutf (event-energy it) (+ e *zero-energy*)))
				       #'< *next-list* :key #'event-energy :before-last t)
			   *next-list*)]
      (setf *next-list* new-list)))
  (setf *next-list* (when *next-list* (mapcar (let-be [floor (event-energy (car *next-list*))]
						(lambda (event) (amutf (-> event energy) (- it floor))))
					      *next-list*))))

(defun add-event(ev)
  (psetf (event-energy ev) (+ (event-energy ev) *zero-energy*)
	 *next-list* (add-before ev #'< *next-list* :key #'event-energy :before-last t)))

;;;Classes

(defclass turn(event)
  ((actor :type enitity :reader entity :initarg :entity)))

(defclass map-entity-turn(turn)
  ((pos :type pos :reader get-pos :initarg :pos)))

(defclass thunk-event(event)
  ((thunk :type (function () t) :initarg :thunk)))

(defclass action(turn)
  ())

(defmethod make-update((entity entity) pos eu)
  (make-instance 'map-entity-turn :entity entity :pos pos :energy (/ eu (get-speed entity))))

(defmethod make-turn((entity proto-creature) eu)
  (assert entity)
  (make-instance 'turn :entity entity :energy (/ eu (get-speed entity))))

(defun make-thunk-event(thunk eu)
  (make-instance 'thunk-event :thunk thunk :energy eu))

(defmethod exec((ev turn))
  (rl.state:state-execute (get-state (entity ev)) ev))

(defmethod exec((turn map-entity-turn))
  (update-entity (entity turn) (get-pos turn)))

(defmethod exec((event thunk-event))
  (funcall (-> event thunk)))

(defmethod take-turn(entity turn)
  (rl.ai:action-execute turn (entity turn)))

;;Standard events

(defun move-accurate!(creature dir)
  "Smart move"
  (or
	 (interact-with-cell! creature dir)
	 (interact-with-cell! creature (find-best (lambda (p1 p2) (if (and (can-move-p creature p1)
									   (or (< (distance dir p1) (distance dir p2))
									       (and (= (distance dir p1) (distance dir p2)) (rnd:bernoulli))))
								      p1
								      p2))
						  (neighbours-delta)))
	 (try-to-move! creature (make-pos 0 0))
	 (error "~a failed to move from ~a to ~a" creature (get-pos creature) (add (get-pos creature) dir))))

(defmethod try-to-move!(creature (dir pos))
  (if (and (zerop (x dir)) (zerop (y dir)))
      (make-turn creature 100)
      (when (can-move-p creature dir)
	(perform-movement creature dir)
	(make-turn creature (if (or (zerop (x dir)) (zerop (y dir))) 100 141)))))

(defun move-random!(creature &optional (tries 8))
  (or (try-to-move! creature (make-pos (1- (random 3)) (1- (random 3))))
      (if (zerop tries) (make-turn creature 100) (move-random! creature (1- tries)))))

(defmethod interact-with-cell!(creature dir)
  (try-to-move! creature dir))

;;Hit

(defun hit(attacker attackee)
  "Melee hit"
  (let-be [dmg (get-damage attacker)
	   dodgesp (rl.combat:dodgesp dmg (get-dodge-coeff attackee))]
	  (report-attack attacker attackee dodgesp)
	  (unless dodgesp
	   (when (zerop (take-damage attackee dmg))
	     (report-reflection attackee)))))
	 
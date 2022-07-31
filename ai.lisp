(in-package :rl.ai)

(defgeneric aciton-execute(action actor))
(defgeneric valid-action-p(action actor))

(defclass ai-state(action)
  ((next-action :initform nil :initarg :next :type ai-state :reader action-next)))

(defun go-next(ai-state)
  (action-execute
   (aif (action-next ai-state)
	it
	(make-turn (entity ai-state) 0))
   (entity ai-state)))

(defun continuate(state turn &rest other-args)
  (apply #'make-instance (type-of state)
	 :entity (entity turn)
	 :energy (event-energy turn)
	 other-args))

(defun jump(state-type &rest args)
  (take-turn (getf args :entity) (apply #'make-instance state-type :energy 0 args)))

(defclass wander(ai-state) ())

(defclass move-to-point(ai-state)
  ((way :initarg :destination :type (vector pos))))

(defclass follow(ai-state)
  ((who :initarg :destination :type creature)))

(defclass move-dir(ai-state)
  ((dir :initarg :direction :type pos)))

(defclass fight(ai-state) ())

;;Methods

(defmethod valid-action-p(action creature)
  t)

(defmethod action-execute :around (action creature)
  (if (valid-action-p action creature)
      (call-next-method)
      (go-next action)))

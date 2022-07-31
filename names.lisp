(in-package :rl.names)

(defgeneric get-singular-name(ent))
(defgeneric get-concrete-name(ent))
(defgeneric get-plural-name(ent))
  
(defstruct (names (:constructor make-names (singular concrete plural)))
  (plural "" :type string)
  (singular "" :type string)
  (concrete "" :type string))

(defmethod get-full-name(entity)
  (get-name entity))

(defmethod get-name(entity)
  (get-singular-name entity))

(defmethod get-singular-name(ent)
  (names-singular (-> ent name)))

(defmethod get-plural-name(ent)
  (names-plural (-> ent name)))

(defmethod get-concrete-name(ent)
  (names-concrete (-> ent name)))

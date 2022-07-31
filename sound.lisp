;;;; Sound simulation
(in-package :rl.sound)

(defun snd(str intensity &optional (color (rl.ui:color :gray)))
  (make-sound (rl.ui:make-gui-string str color) intensity))

(defstruct (sound (:constructor make-sound(message intensity)))
  (message (rl.ui-lang:str "I hear a sound.") :type rl.ui:gui-string)
  (intensity 8 :type fixnum))

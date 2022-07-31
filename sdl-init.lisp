(in-package :api)
(defvar *loaded* cl:nil)

(unless *loaded*
  (setf *loaded* t)
  (load-foreign-library "RL_SDL.dll")
  (defcfun ("initialize") :void)
  (initialize))

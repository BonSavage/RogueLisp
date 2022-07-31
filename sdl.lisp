(in-package :api)

(eval-when (:load-toplevel)
  (load "sdl-init.lisp"))

(defcstruct color
  (red :uint8)
  (green :uint8)
  (blue :uint8))

(defcstruct gramma
  (char :uint8)
  (red :uint8)
  (green :uint8)
  (blue :uint8))

(defcstruct cpos
  (x :uint8)
  (y :uint8))

(defcstruct rect
  (start (:struct cpos))
  (size (:struct cpos)))

(defcfun ("draw_gramma") :void
  (gramma (:pointer (:struct gramma)))
  (pos (:pointer (:struct cpos))))

(defcfun ("fill_cell") :void
  (color (:pointer (:struct color)))
  (pos (:pointer (:struct cpos))))

(defcfun (render-clear "clear") :void)
(defcfun (render-present "present") :void)
(defcfun ("get_key_event") :int)
(defcfun ("key_to_event") :int (key :int))
(defcfun ("draw_rectangle") :void
  (rect (:pointer (:struct rect)))
  (color (:pointer (:struct color))))
(defcfun ("draw_frame") :void
  (rect (:pointer (:struct rect)))
  (color (:pointer (:struct color))))

(defmacro with-foreign-temporary((name type &rest slots) &body forms)
  `(cffi:with-foreign-object (,name ',type)
     (macrolet ,(mapcar (lambda (slot-name) `(,slot-name (object) (foreign-slot-value ,object ,type ,slot-name))) slots)
       ,@forms)))

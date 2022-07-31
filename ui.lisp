;;;; ui.lisp
;;; Todo:

(in-package :rl.user-interface)

;;;Constants
(eval-when (:compile-toplevel :load-toplevel)
  (def-symbol-map color-map
    (:black (0 0 0))
    (:dark-gray (64 64 64))
    (:gray (128 128 128))
    (:white (255 255 255))
    (:crimson (192 0 0))
    (:red (128 0 0))
    (:brown (128 24 24))
    (:dark-brown (64 24 24))
    (:yellow (128 96 96))
    (:silver (192 192 192))
    (:green (0 128 0))
    (:olivine (0 96 48))
    (:blue (0 0 128))
    (:purple (32 0 128))
    (:orange (128 64 64)))

  (def-symbol-map cell-color-map
    (:default (:gray :black))
    (:title (:white :black))
    (:selected (:black :white))
    (:hp (:crimson :black))
    (:armor (:blue :black))
    (:sound-mark (:yellow :black))
    (:selected-alpha (:white :black))
    (:limit (:black :gray))))


;;;Low-level

(defstruct (simple-color (:conc-name color-) (:constructor make-color (red green blue)))
  (red 0 :type (unsigned-byte 8))
  (green 0 :type (unsigned-byte 8))
  (blue 0 :type (unsigned-byte 8)))

(defstruct (cell-color (:constructor make-cell-color (char-color background-color)))
  (char-color nil :type color)
  (background-color nil :type color))

(deftype color()
  '(or simple-color cell-color))

(defstruct (gramma (:constructor construct-gramma(code color)) (:predicate grammap))
  (code 0 :type (unsigned-byte 8))
  (color nil :type (or color cell-color)))

(defmethod print-object((object simple-color) stream)
  (format stream "<Color>"))

(defmethod print-object((object gramma) stream)
  (format stream "<Gramma>"))

(defun make-gramma(code color)
  (construct-gramma code color))

(defun free-gramma(cell-gramma)
  (make-gramma (-> cell-gramma code) (-> cell-gramma color char-color)))

(defun cell-gramma-p(gramma)
  (and (grammap gramma) (cell-color-p (-> gramma color))))

(defun simple-gramma-p(gramma)
  (and (grammap gramma) (simple-color-p (gramma-color gramma))))

(deftype simple-gramma() '(satisfies simple-gramma-p))

(deftype cell-gramma() '(satisfies cell-gramma-p))

(defmacro let-foreign-pos((cpos-name pos) &body body)
  `(cffi:with-foreign-object (,cpos-name '(:struct api:cpos))
     (cffi:with-foreign-slots ((api:x api:y) ,cpos-name (:struct api:cpos))
       (psetf api:x (x ,pos)
	      api:y (y ,pos)))
     ,@body))

(defmacro let-foreign-gramma((cgramma-name gramma) &body body)
  `(cffi:with-foreign-object (,cgramma-name '(:struct api:gramma))
     (cffi:with-foreign-slots ((api:char api:red api:green api:blue) ,cgramma-name (:struct api:gramma))
       (awith (-> gramma color)
	 (psetf api:char (-> ,gramma code)
		api:red (-> it red)
		api:green (-> it green)
		api:blue (-> it blue))))
     ,@body))

(defmacro let-foreign-color((ccolor-name color) &body body)
  `(cffi:with-foreign-object (,ccolor-name '(:struct api:color))
     (cffi:with-foreign-slots ((api:red api:green api:blue) ,ccolor-name (:struct api:color))
       (psetf api:red (-> ,color red)
	      api:green (-> ,color green)
	      api:blue (-> ,color blue)))
     ,@body))

(defmacro let-foreign-rect((crect-name rect) &body body)
  `(cffi:with-foreign-object (,crect-name '(:struct api:rect))
     (cffi:with-foreign-slots ((api:start api:size) ,crect-name (:struct api:rect))
       (let-foreign-pos (strt (start ,rect))
	 (let-foreign-pos (size (size ,rect))
	   (psetf api:start strt
		  api:size size))))
     ,@body))

(defun draw-simple-gramma(gramma pos)
  (let-foreign-gramma(gr gramma)
    (let-foreign-pos (ps pos)
      (api:draw-gramma gr ps))))

(defun fill-cell(color pos)
  (let-foreign-color (clr color)
    (let-foreign-pos (ps pos)
      (api:fill-cell clr ps))))

(defun draw-cell-gramma(cell-gramma pos &aux (gramma (free-gramma cell-gramma)))
  (let-foreign-gramma (gr gramma)
    (let-foreign-color (clr (-> cell-gramma color background-color)) ;Ad-hocery for performance
      (let-foreign-pos (ps pos)
	(api:fill-cell clr ps)
	(api:draw-gramma gr ps)))))


;;;High-order routines

(defun map-color(i)
  (apply #'make-color (cdr (index->pair +color-map+ i))))

(defun map-cell-color(i)
  (apply #'make-cell-color
	 (mapcar (lambda (sym)
		   (apply #'make-color (cdr (assoc sym +color-map+))))
		 (cdr (index->pair +cell-color-map+ i)))))

(defmacro cell-color(symbol)
  `(load-time-value (map-cell-color (symbol->index +cell-color-map+ ,symbol))))

(defmacro color(symbol)
  `(load-time-value (map-color (symbol->index +color-map+ ,symbol))))

(defmacro layer-color(symbol)
  `(make-cell-color (color ,symbol) (color :black)))

;;Types

(defmacro static-gramma(char color)
  "To be used outside this package"
  `(load-time-value (make-gramma (char-code ,char)
				 ,color)
		    t))

(defstruct (gui-string (:constructor make-gui-string (chars color)))
  (chars "" :type string)
  (color (color :gray) :type (or color cell-color)))

(defun gramma(gstr i)
  (make-gramma (char-code (aref (gui-string-chars gstr) i)) (gui-string-color gstr)))

(defun augment-gramma(gramma &key (background (make-color 0 0 0)) color)
  (let* ((gramma-color (gramma-color gramma))
	 (char-color (cond(color color)
			  ((cell-color-p gramma-color) (cell-color-char-color gramma-color))
			  (t gramma-color)))
	 (background-color (cond (background background)
				 ((cell-color-p gramma-color) (cell-color-background-color gramma-color))
				 (t background))))
    (construct-gramma (gramma-code gramma) (make-cell-color char-color background-color))))

(defun gstr-length(gui-string)
  (length (gui-string-chars gui-string)))

;;Drawers

(defun draw-frame(color rect)
  (let* ((background-color (if (cell-color-p color) (-> color background-color)))
	 (frame-color (if (cell-color-p color) (-> color char-color) color)))
    (let-foreign-rect (r rect)
      (let-foreign-color (fc frame-color)
	(when background-color
	  (let-foreign-color (bc background-color)
	    (api:draw-rectangle r bc)))
	(api:draw-frame r fc)))))

(defun draw-rectangle(color rect)
  (let-foreign-color (c color)
    (let-foreign-rect (r rect)
      (api:draw-rectangle r c))))

(defun draw-gramma(gramma cursor)
  (typecase gramma
    (simple-gramma (draw-simple-gramma gramma cursor))
    (cell-gramma (draw-cell-gramma gramma cursor)))
  (incf (-> cursor x)))

(defun draw-string(gui-string cursor)
  "Draw a string on a line"
  (awith (gstr-length gui-string)
    (unless (zerop it)
      (let* ((gramma (gramma gui-string 0))
	     (draw (if (cell-gramma-p gramma) #'draw-cell-gramma #'draw-simple-gramma)))
	(dotimes (i it cursor)
	  (funcall draw (amutf (gramma-code gramma) (char-code (aref (-> gui-string chars) i))) cursor)
	  (incf (-> cursor x)))))))

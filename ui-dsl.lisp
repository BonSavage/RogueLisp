(in-package :rl.ui-lang)
;;;; Has two parts: framework and DSL inself
;;;; Framework is low-level facility for DSL, and library (ui.lisp) is low-level for framework

(defvar *drawable* nil "List of static drawable widgets")
(declaim (special *drawable*))

(defun draw-start()
  "Must be called in draw-procedures"
  (progn
    (api:render-clear)
    (dolist (drw (reverse *drawable*))
      (draw-view drw))))

(defun draw-end()
  (api:render-present))

(defun draw-all()
  (draw-start)
  (draw-end))

;;;Main

(defmacro change-layer(form)
  "To clear current context"
  `(let (*drawable*)
     ,form))

(defmacro with-drawable(view &body forms)
  `(let ((*drawable* (cons ,view *drawable*)))
     ,@forms))


(defmacro define-panel(name lambda-list &key named drawable controller)
  `(defun ,name ,lambda-list
     (let ,named
       (with-drawable ,drawable
	 (loop
	    (draw-start)
	    (draw-end)
	   ,controller)))))

(defmacro define-view(name elem-view view-selected)
  `(defparameter ,name (make-elem-view :elem-view ,elem-view :view-selected ,view-selected)))

(defun elem-view(elem-view view-selected)
  (make-elem-view :elem-view elem-view :view-selected view-selected))

;;Low-level


(defun make-view(content rect)
  (funcall content rect))

(defun gstr-construct(gstrs size)
  (funcall gstrs size))

(defun draw-view(view)
  (force view))

(defun get-page(delayed-page)
  (force delayed-page))

(defmacro view(&body forms)
  `(delay
    (progn ,@forms)))

(defmacro delayed-page(&body forms)
  `(delay
    (progn ,@forms)))

(defmacro page-view(page start)
  `(view
     (draw-page ,page ,start)))

;;Contents

(defun frame(color content)
  "Decorator of the content"
  (lambda (rect)
    (awith (make-view content (make-rect (add (start rect) (make-pos 1 1)) (sub (size rect) (make-pos 2 2))))
	   (view
	    (draw-frame color rect)
	    (draw-view it)))))

(defun page(gstr-content)
  (lambda (rect)
    (awith (gstr-construct gstr-content (size rect))
	   (view
	    (draw-page (get-page it) (start rect))))))


;;String-content

(defun text(gui-strings)
  (lambda (size)
    (awith (text-lines gui-strings (x size))
	   (delayed-page it))))

(defun catalogue(gui-strings)
  (lambda (size)
    (awith (catalogue-lines gui-strings (x size))
      (delayed-page it))))

(defun plain(gui-strings)
  (lambda (size)
    (declare (ignore size))
    (delayed-page (list gui-strings))))


;;Other

(defun str(chars &optional (color (color :gray)))
  (make-gui-string chars color))

(defun framed(color content)
  (frame color content))

(defun background(color content)
  (lambda (rect)
    (awith (make-view content rect)
      (view
	(draw-rectangle color rect)
	(draw-view it)))))

(defmacro rect(start end)
  `(make-rect (make-pos ,@start) (make-pos ,@(mapcar #'- end start (list -1 -1)))))

(defmacro pos(x y)
  `(make-pos ,x ,y))

(defun field(rect content)
  (make-view content rect))

(defmacro views(&body forms)
  `(awith (list ,@forms)
	  (lambda ()
	    (dolist (form it)
	      (force form)))))

(defun horizontal-split(left right)
  (lambda (rect)
    (let ((left-rect (left-slice rect))
	  (right-rect (right-slice rect)))
      (let ((left-view (make-view left left-rect))
	    (right-view (make-view right right-rect)))
	(view
	 (draw-view left-view)
	  (draw-view right-view))))))

(defun standard-frame(&optional (content (lambda (arg) (view))))
  (lambda (&optional (rect (make-rect (make-pos 0 0) (make-pos 79 25))))
    (awith (make-view content (make-rect (add (start rect) (make-pos 0 1))
					 (sub (size rect) (make-pos 0 2))))
      (view
	(draw-standard-frame rect)
	(draw-view it)))))

(defun scroll-bar(rect)
  (lambda (scrollable)
    (awith (construct-scrollable))
    (delayed-page
     (draw-scroll-bar scrollable (start rect))
     (it))))

;;High level

(defun line(&rest args)
  args)

;;Handlers

(defstruct elem-view
  (elem-view nil :type (function (t coordinates) t) :read-only t)
  (view-selected nil :type (function (t) t)  :read-only t))

(defun string-select(str &optional (color (cell-color :selected)))
  (gstr-select str color))

;;TODO: Need define-scrollable macro to abstract interface from internals

(defmethod instance((model rl.perception:fov-info) &key (center-coeff (cons .5 .5)))
  (lambda (rect)
    (let ((center (add (start rect)
		       (make-pos (floor (* (car center-coeff) (x (size rect))))
				 (floor (* (cdr center-coeff) (y (size rect))))))))
      (view
	(draw-map model (start rect) (end rect) center)))))




;;;OOP internals

;;Category: scrollable

(defclass handlered()
  ((handler :initarg :handler)))

(defclass scrollable-view(handlered)
  ((gstr-content :initarg :gstr-content)))

(defclass menu(handlered)
  ((elem-view :initarg :elem-view)
   (content :initarg :content)))

(defclass alphabetic-menu(menu) ())

(defclass complex-menu(menu)
  ((sections :initarg :sections)))

(defclass buffer-view(scrollable-view)())

;;Protocols:
;;1. Construction
(defgeneric construct-scrollable(view size))
;;2. View
(defgeneric view-page(view))
;;3.Use

(defun view-instance(scrollable-view &key from-end scroll-bar)
  (with-slots (handler) scrollable-view
    (lambda (rect &aux (start (start rect)) (size (size rect)))
      (awith (construct-scrollable scrollable-view size)
	     (when from-end
	       (scroll-lines it (length (-> it sequence))))
	     (set-dependent (-> scrollable-view handler) it)
	     (view
	      (if scroll-bar (draw-scroll-bar it start))
	      (draw-page (view-page scrollable-view) start))))))

(defun selected-description(scrollable-view elem-description) ;Isn't it better to use macro instead?
  (lambda (size)
    (delayed-page
     (text-lines (funcall elem-description (aif (-> scrollable-view content)
						(elt it (-> scrollable-view handler dependent index))
						nil))
		 (x size)))))
;;Realization:

(defmethod construct-scrollable((view scrollable-view) size)
  (make-scrollable :sequence (get-page (funcall (-> view gstr-content) size))
		   :index 0
		   :size size))

(defmethod construct-scrollable((view menu) size)
  (make-scrollable :sequence (mapcar (alexandria:rcurry (-> view elem-view elem-view) (x size)) (-> view content))
		   :size size
		   :index 0))

(defmethod view-page((view handlered))
  (scrollable-page (-> view handler dependent)))

(defmethod view-page((view buffer-view))
  (progn
    (let-be [handler (-> view handler)
	  dependent (-> handler dependent)]
      (reset-dependent handler (modf (-> dependent sequence)
				     (get-page (funcall (-> view gstr-content) (-> dependent size)))))
      (call-next-method))))

(defmethod view-page((view menu))
  (menu-page (-> view handler dependent) :convert (-> view elem-view view-selected)))

(defmethod view-page((view alphabetic-menu))
  (alphabetic-page (-> view handler dependent) :convert (-> view elem-view view-selected)))

(defmethod view-page((view complex-menu))
  (complex-page (-> view handler dependent) :sections (-> view sections) :convert (-> view elem-view view-selected)))

(defun extract-sections(sequence)
  (pm/amatch (0 sequence nil)
	     self(_ nil res) (reverse res)
	     self(pos (hd . tl) res) (self (+ 0 (section-length hd))
					   tl
					 (cons (make-menu-section
						:name (section-heading hd)
						:pos pos)
					       res))))

(defun extract-content(sequence)
  (mappend #'section-content sequence))

;;DSL interface

(defstruct (section (:constructor section (heading content)))
  (heading nil :type gui-string)
  (content nil :type list))

(defstruct label
  (alpha)
  (content))

(defun section-length(sect)
  (1+ (length (section-content sect))))

(defun scrollable(gstr-content &optional (closure #'control-scrollable))
  (make-instance 'scrollable-view :handler (handler closure) :gstr-content gstr-content))

(defmacro simple-menu(&rest title2action)
  (alexandria:with-gensyms (titles actions)
    `(let* ((,titles (list ,@(mapcar (lambda (t2a) (first t2a)) title2action)))
	    (,actions (vector ,@(mapcar (lambda (t2a) `(lambda () ,(second t2a))) title2action))))
       (make-instance 'menu
		      :handler (handler (make-menu-controller (lambda (index) (funcall (elt ,actions index)))))
		      :content ,titles
		      :elem-view (make-elem-view :elem-view (lambda (seq len) seq)
						 :view-selected (lambda (seq) (string-select seq)))))))

(defun menu(sequence view &key (action (lambda (index) (elt sequence index))))
  (make-instance 'menu
		 :handler (handler (make-menu-controller action))
		 :elem-view view
		 :content sequence))


(defun alphabetic-menu(sequence view &key (action (lambda (index) (elt sequence index))))
  (make-instance 'alphabetic-menu 
		 :handler (handler (make-alphabetic-controller action))
		 :elem-view view
		 :content sequence))

(defun complex-menu(sequence view &key  (action (lambda (index content) (elt content index))))
  (awith (extract-content sequence)
    (make-instance 'complex-menu
		   :handler (handler (make-menu-controller (alexandria:rcurry action it)))
		   :elem-view view
		   :sections (extract-sections sequence)
		   :content it)))

(defun buffer(source)
  (let ((handler (handler #'control-buffer)))
    (make-instance 'buffer-view
		   :handler handler
		   :gstr-content (lambda (size) (delayed-page (funcall source (aif (-> handler dependent) (-> it sequence) nil) size))))))

;;;Controllers

(defun control-scrollable(subscribed &optional (event (api:get-key-event)))
  (controller-body event
		   (down (scroll-lines subscribed 1))
		   (up (scroll-lines subscribed -1))
		   (#\Return (scroll-pages subscribed 1))
		   (#\Backspace (scroll-pages subscribed -1))))

(defun control-buffer(dependent)
  (if (last-page-p dependent)
      'exit
      (control-scrollable dependent)))

(defun make-menu-controller(action)
  (lambda (subscribed)
    (handle-input
     (down (scroll-lines subscribed 1))
     (up (scroll-lines subscribed -1))
     (#\Escape 'exit)
     (#\Return (funcall action (-> subscribed index))))))

(defun call-handler(model)
  (funcall (-> model handler closure) (-> model handler dependent)))

(defmacro handle(model)
  `(awith (call-handler ,model)
	  (cond ((eq it 'exit) (return nil))
		((null it) nil)
		(t (return it)))))

(defun make-alphabetic-controller(action)
  (lambda (dependent)
    (awith (api:get-key-event)
	   (if (and (>= it 4) (<= it 29)) ;a - z
	       (awith (- it 4)
		      (if (= it (-> dependent index))
			  (funcall action (-> dependent index))
			  (psetf (-> dependent index) (grant-bounds (+ it (page-index dependent)) 0 (1- (length (-> dependent sequence)))))))
	       (controller-body it
				(down (scroll-lines dependent 1))
				(up (scroll-lines dependent -1))
				(#\Return (funcall action (-> dependent index)))
				(#\Escape 'exit))))))

(defun make-complex-controller(action subcontroller)
  (lambda (subscribed)
    (awith (api:get-key-event)
      (controller-body it
	(down (scroll-lines subscribed 1))
	(up (scroll-lines subscribed -1))
	(#\Return (funcall action (-> subscribed index)))
	(t (funcall subcontroller subscribed content))))))

;;;Refactoring needed

(defgeneric draw-map(fov draw-start draw-end center))

(defclass lookup-fov(rl.perception:fov-info)
  ((focus :type pos :accessor get-focus :initarg :focus)))

(defmethod get-map-focus((fov lookup-fov))
  (add (rl.perception:get-center fov) (get-focus fov)))

(defclass target-fov(lookup-fov)
  ((targets :type cons :initarg :target-list)))

(defun next-target(target-fov)
  (psetf (-> target-fov focus)
	 (aif (pop (-> target-fov targets))
	      (sub it (rl.perception:get-center target-fov))
	      (-> target-fov focus))))

(defmethod make-lookup-fov((fov rl.perception:fov-info) &optional (focus (make-pos 0 0)))
  (make-instance 'lookup-fov :entity-positions (rl.perception:fov-entity-positions fov)
			     :center (rl.perception:get-center fov)
			     :focus focus
			     :fov (rl.perception:get-fov fov)
			     :marks (rl.perception:fov-marks fov)))

(defmethod update-instance-for-different-class :after ((old lookup-fov) (new target-fov) &key actor)
  (psetf (-> new targets) (apply #'circular-list
				 (sort (mapcar #'rl.entity:get-pos (remove actor (remove-if (alexandria:rcurry #'typep '(not rl.entity:proto-creature))(rl.perception:visible-entities old))))
				       (lambda (p1 p2)(< (distance p1 (rl.perception:get-center old)) (distance p2 (rl.perception:get-center old)))))))
  (next-target new))

(defmethod make-target-fov((fov rl.perception:fov-info) actor)
  (change-class (make-lookup-fov fov) 'target-fov :actor actor))

(defun draw-terrain(fov-info center)
  (declare (special map-rect fov-center))
  (geom:doarea (pos map-rect)
    (relation-case (fov-info pos)
      (rl.perception:visiblep (draw-gramma (rl.perception:get-gramma fov-info pos) (add center (sub pos fov-center))))
      (rl.perception:seenp (draw-gramma (rl.perception:get-plan-gramma fov-info pos) (add center (sub pos fov-center))))
      (t nil))))

(defmethod draw-map((fov-info rl.perception:fov-info) draw-start draw-end center)
  (declare (special map-rect fov-center) (ignore draw-end draw-start) (optimize (speed 3))) ;TODO: Must be optimized. Must use a foreign object
  (draw-terrain fov-info center)
  (iter
    (for (pos . entities) in (rl.perception:fov-entity-positions fov-info))
    (let-be [gramma (rl.level:get-entities-gramma entities)]
      (when (and gramma (in-bound-p pos map-rect))
	(draw-gramma gramma (add center (sub pos fov-center))))))
  (iter
    (for (pos . mark) in (rl.perception:fov-marks fov-info))
    (when (in-bound-p pos map-rect)
      (draw-gramma (rl.perception:mark-gramma mark) (add center (sub pos fov-center))))))

(defmethod draw-map :around ((fov-info rl.perception:fov-info) draw-start draw-end center)
  (let* ((start-offset (sub draw-start center))
	 (end-offset (sub draw-end center))
	 (fov-center (rl.perception:get-center fov-info))
	 (map-start (rl.map:grant-on-map (add fov-center start-offset)))
	 (map-end (rl.map:grant-on-map (add fov-center end-offset)))
	 (map-rect (make-rect map-start (distance-point map-end map-start))))
    (declare (special map-start map-end map-rect fov-center))
    (call-next-method)))

(defmethod draw-map((fov lookup-fov) draw-start draw-end center)
  (declare (special map-rect) (ignore draw-start draw-end))
  (let ((offset (get-focus fov))
	(map-focus (get-map-focus fov)))
    (call-next-method)
    (when (in-bound-p map-focus map-rect)
      (draw-gramma (make-gramma 206 (if (rl.perception:visiblep fov map-focus) (color :green) (color :crimson))) (add center offset)))))

(defmethod draw-map((fov target-fov) draw-start draw-end center)
  "Draw map and LOS"
  (declare (special map-start map-end map-rect fov-center) (ignore draw-start draw-end))
  (let ((map-focus (rl.map:grant-on-map (get-map-focus fov))))
    (progn 
      (call-next-method)
      (geom:doray (pos (cast-ray (lambda (pos) (rl.map:in-map-bound-p pos) (and (rl.perception:visiblep fov pos) (rl.map:obstaclep pos)))
				 (cell-line fov-center map-focus)))
	(when (in-bound-p pos map-rect)
	  (draw-gramma (make-gramma (char-code #\*) (cond ((not (rl.perception:visiblep fov pos)) (make-color 200 100 0))
							  ((not (rl.map:obstaclep pos))(make-color 0 200 0))
							  (t (make-color 200 0 0))))
		       (add center (sub pos fov-center)))))
      (draw-gramma (make-gramma (char-code #\X) (if (rl.perception:visiblep fov map-focus) (make-color 0 200 0) (make-color 200 100 0)))
		   (add center (get-focus fov))))))

(defmethod draw-map :around ((fov lookup-fov) draw-start draw-end center)
  (call-next-method fov draw-start draw-end (sub center (get-focus fov))))

(defun move-focus(fov pos)
  (amutf (-> fov focus) (add it pos))
  nil)

(defun sources-append(s1 s2 &key with-previous)
  (lambda (lines size)
    (funcall s1 (funcall s2 (if with-previous lines nil) size) size)))

(defun buffer-source(buffer)
  (lambda (lines size)
    (append lines (text-lines (rl.message:pop-unprinted buffer) (x size)))))

(defun fov-description(fov)
  (lambda (lines size)
    (declare (ignore lines))
    (text-lines (list (str (formatted "X: ~a Y: ~a" (+ (x (rl.perception:get-center fov)) (x (-> fov focus))) (+ (y (rl.perception:get-center fov))(y (-> fov focus)))))
		      (str (rl.perception:fov-pos-description fov (add (rl.perception:get-center fov) (-> fov focus)))))
		(x size))))

(defun text-source(&rest gstrs)
  (lambda (lines size)
    (declare (ignore lines))
      (text-lines gstrs (x size))))

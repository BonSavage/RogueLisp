(in-package :rl.ui-lang)

;;String lists

(defun str-lines(str line-length)
  (mvb [sliced rest] (str-slice str (1- line-length))
       (if rest
	   (cons sliced (str-lines rest line-length))
	   (list sliced))))

(defun str-slice(str slice-pos)
  (awith (grant-bounds slice-pos 0 (length str))
    (mvb (separator-pos self) (or
				(position #\Newline str :end it)
				(when (> (length str) slice-pos) (position #\Space str :end it :from-end t))
				(values it t))
	 (values (subseq str 0 separator-pos) (unless (= (length str) separator-pos) (subseq str (if self separator-pos (1+ separator-pos))))))))

(defun gstr-lines(gstr line-length)
  (mapcar
   (lambda (str)
     (make-gui-string str (-> gstr color)))
   (str-lines (-> gstr chars) line-length)))

(defun gstr-slice(gstr slice-pos)
  (let-be [(sliced rest) (str-slice (-> gstr chars) slice-pos)]
    (values (make-gui-string sliced (-> gstr color)) (when rest (make-gui-string rest (-> gstr color))))))

(defun text-lines(gstr-list line-length &aux (line-length (1- line-length)))
  "Formatted gui-string lines"
  (pm/amatch [gstr-list line-length nil]
	     self(nil _ res) (when res (list (reverse res)))
	     self(gstr-list 0 res) (cons (reverse res) (self gstr-list line-length nil))
	     self(gstr-list 1 res) (self gstr-list 0 res)
	     self((gstr . next) [len (integer 2)] res) (let-be  [(sliced rest) (gstr-slice gstr (1- len))]
							 (self (aif rest (cons it next) next)
							       (if rest
								   0
								   (- len (gstr-length sliced) 1))
							       (cons sliced res)))))

(defun catalogue-lines(gstr-list sub-length)
  "Formatted and aligned lines"
  (apply #'append (mapcar #'(lambda (gstr) (gstr-lines gstr sub-length)) gstr-list)))

(defun one-lines(page line-length)
  "Aligned, elem per line"
  (mapcar (lambda (elem)
	    (pm/amatch [elem line-length]
		       self(_ 0) nil
		       self([str gui-string] len) (gstr-slice str len)
		       self((gstr . rest) len) (awith (gstr-slice gstr len)
						      (cons it (self rest (- (length it) len))))))
	  page))

(defun select-line(page selected-index &key (convert #'gstr-select))
  (when page ;TODO: Should throw exception
    (awith (elt page selected-index)
	   (substitute (funcall convert it) it page :start selected-index :count 1 :test #'eq))))

(defun page-alphabetic(page)
  (mapcar (lambda (alpha title) (cons (alpha-string alpha) (if (listp title) title (list title))))
	  (range #\a #\z (lambda (ch) (code-char (1+ (char-code ch)))) #'char=) page))

(defun gstr-select(gstr &optional (color (cell-color :selected)))
  (declare (ignore len))
  (make-gui-string (-> gstr chars) color))

(defun alpha-select(alpha-string)
  (declare (ignore len))
  (make-gui-string (-> alpha-string chars) (cell-color :selected-alpha)))

(defun alpha-string(char &optional (color (color :gray)))
  (make-gui-string (formatted "~a)" char) color))


;;Dynamic

(defstruct (handler (:constructor handler(closure)))
  (closure nil :type (function (handler) t))
  (dependent nil))

(defmethod set-dependent(handler dep)
  (assert (null (-> handler dependent)))
  (setf (-> handler dependent) dep))

(defmethod reset-dependent(handler dep)
  (setf (-> handler dependent) dep))

;;Scrollable

(defstruct indexed-sequence
  (sequence nil :type sequence :read-only t)
  (index 0 :type fixnum))

(defun indexed-subsequence(is)
  (with-slots (sequence index) is
    (subseq sequence (grant-bounds index 0 (length sequence)))))

(defstruct (scrollable (:include indexed-sequence))
  (size (make-pos 1 1) :type rl.coordinates:coordinates :read-only t))

(defun scroll-pages(scrollable count)
  (scroll-lines scrollable (* count (y (-> scrollable size)))))

(defun scroll-lines(scrollable count &aux (len (length (-> scrollable sequence))))
  (progn
    (amutf (-> scrollable index) (grant-bounds (+ it count) 0 (1- len)))
    nil))

(defun end-reached-p(scrollable)
  "Is index pointing to the last line?"
  (with-slots (index sequence) scrollable
    (>= index (1- (length sequence)))))

(defun last-page-p(scrollable)
  "Is index inside the last page?"
  (with-slots (index sequence size) scrollable
    (<= (- (length sequence) index) (y size))))

(defun scroll-to(scrollable position)
  (psetf (-> scrollable index) (grant-bounds position 0 (1- (length (-> scrollable sequence))))))

(defun page-index(scrollable)
  "Number of the current page and dependent index of the selected line"
  (with-slots (index size) scrollable
    (floor index (y size))))

(defun page-length(scrollable)
  "Constant in fact"
  (-> scrollable size y))

(defun page-bounds(scrollable)
  "Start index and end index of the current page"
  (awith (page-index scrollable)
	 (values it (grant-bounds (+ it (y (-> scrollable size))) it (length (-> scrollable sequence))))))

(defun scrollable-page(scrollable &key aligned)
  "Current page of the scrollable"
  (with-slots (sequence index size) scrollable
    (multiple-value-call #'scrollable-subseq scrollable
			 (if aligned
			     (page-bounds scrollable)
			     (values (grant-bounds index 0 (length sequence))
				     (grant-bounds (+ index (y size)) index (length sequence)))))))

(defun scrollable-subseq(scrollable start &optional end)
  "Just get the subsequence of the scrollable lines"
  (with-slots (sequence index) scrollable
    (subseq sequence start end)))

(defun menu-page(scrollable &key (convert #'gstr-select))
  "Scrollable page with selected line"
  (select-line (scrollable-page scrollable :aligned t) (value-second (page-index scrollable)) :convert convert))

(defun menu-selected(scrollable)
  "Selected line"
  (when (-> scrollable sequence)
    (elt (-> scrollable sequence) (-> scrollable index))))

(defun alphabetic-page(scrollable &key (convert  #'gstr-select))
  (select-line (page-alphabetic (menu-page scrollable :convert convert)) (value-second (page-index scrollable))
	       :convert (lambda (line) (cons (alpha-select (first line)) (rest line)))))

(defstruct menu-section
  (name nil :type gui-string)
  (pos 0 :type fixnum))

(defun label-sections(gstrs sections)
  (pm/amatch (gstrs 0 sections)
	     self(nil _ _) nil
	     self(lst _ nil) lst 
	     self((hd . tl) i (sec . rest)) (if (= i (menu-section-pos sec))
						(cons (menu-section-name sec) (self (cons hd tl) (1+ i) rest))
						(cons hd (self tl (1+ i) (cons sec rest))))))

(defun calculate-offset(scrollable sections)
  (pm/amatch (sections)
	     self(nil) 0
	     self((hd . tl)) (if (<= (menu-section-pos hd)
				     (scrollable-index scrollable))
				 (1+ (self tl))
				 0)))

(defun complex-page(scrollable &key sections (convert #'gstr-select))
  (let-be [selected (select-line (-> scrollable sequence)
				 (-> scrollable index)
				 :convert convert)
	   labeled (label-sections selected sections)
	   offset (calculate-offset scrollable sections)
	   converted (make-scrollable :sequence labeled :index (+ offset (scrollable-index scrollable)) :size (scrollable-size scrollable))]
    (scrollable-page converted :aligned t)))


;;Dynamic

(defmacro control(controller &optional else)
  (alexandria:with-gensyms (res)
    `(let ((,res (funcall ,controller (api:get-key-event))))
       (cond
	 ((eq 'exit ,res) (return nil))
	 ((null ,res) ,else)
	 (t (return ,res))))))

(defmacro controller(&rest key2expr)
  `(lambda (key) (controller-body key ,@key2expr)))

(defmacro handle-input(&rest key2expr)
  `(controller-body (api:get-key-event) ,@key2expr))

(defmacro controller-let(&rest key2expr)
  `(control (controller ,@key2expr)))

(defun expr-event(expr)
  (predicate-case expr
		  (characterp (api:key-to-event (char-code expr)))
		  (^(eq 't _) 't)
		  (listp (if (eq (first expr) 'or)
			     (mapcar #'expr-event (rest expr))))
		  (symbolp (case expr
			     (up 82)
			     (down 81)
			     (right 79)
			     (left 80)
			     (t (api:key-to-event
				 (char-code (name-char expr))))))))

(defun build-controller-case(cs event)
  (awith (car cs)
	 (if (and (listp it) (eq (first it) 'and))
	     (destructuring-bind (op first . rest) it
	       (if rest
		   `(,event
		     (handle-input
		      ((and ,@rest)
		       (t nil))))
		   (build-controller-case (cons first (cdr cs)))))
	     (cons
	      (expr-event it)
	      (cdr cs)))))

(defmacro controller-body(event &body key2expr)
  `(case ,event
     ,@(mapcar
        (alexandria:rcurry #'build-controller-case event)
	key2expr)))


;;;Primitive drawers (must be called indirectly through closures)

(defun draw-cascade(cascade cursor)
  "Draw some strings on a line"
  (doseq (elem cascade cursor)
	 (predicate-case elem
			 (gui-string-p (progn (draw-string elem cursor) (amutf (x cursor) (1+ it))))
			 (grammap (draw-gramma elem cursor)))))

(defun draw-line(elem cursor)
  (predicate-case elem
		  (listp (draw-cascade elem cursor))
		  (gui-string-p (draw-string elem cursor))
		  (grammap (draw-gramma elem cursor))))

(defun draw-page(page cursor &aux (cursor (make-pos (x cursor) (y cursor))) (start-x (x cursor)))
  "Draw on some lines"
  (predicate-case page
		  (gui-string-p (draw-line page cursor))
		  (t (doseq (elem page cursor)
			    (draw-line elem cursor)
			    (amutf (x cursor) start-x
				   (y cursor) (1+ it))))))

(defun draw-scrollable(scrollable) ;Shoud be removed. Or not?
  (with-slots (rectangle index sequence) scrollable
    (progn
      (draw-page (scrollable-page scrollable)
		 (start (-> scrollable rectangle))))))

;;Other drawers
(defun draw-standard-frame(rect)
  (awith (rl.ui:static-gramma #\# (color :gray))
    (dotimes (i (x (size rect)))
      (draw-simple-gramma it (add (start rect) (make-pos i 0)))
      (draw-simple-gramma it (sub (end rect) (make-pos i 0))))))

(defun draw-scroll-bar(scrollable start)
  (awith (static-gramma #\* (color :green))
    (draw-simple-gramma it
			(make-pos (1- (+ (x start) (x (-> scrollable size))))
				  (+ (y start) (round (* (y (-> scrollable size)) (/ (-> scrollable index) (length (-> scrollable sequence))))))))))

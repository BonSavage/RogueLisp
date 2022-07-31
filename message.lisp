(in-package :rl.message)

(defclass buffer()
  ((unprinted :initform nil :type list)))

(defclass message-buffer(buffer)
  ((print-once :initform (make-buffer) :type buffer)
   (printed :initform nil :type list)))

(defun add-message(buf gui-string)
  (push gui-string (-> buf unprinted))
  nil)

(defun add-note(buf gui-string)
  (add-message (-> buf print-once) gui-string))

(defmethod emptyp((buf buffer))
  (null (-> buf unprinted)))

(defmethod pop-unprinted((buf message-buffer))
  (if (-> buf unprinted)
      (awith (call-next-method buf)
	     (setf (-> buf printed) (nconc (-> buf printed) it))
	     it)
      (pop-unprinted (-> buf print-once))))

(defmethod pop-unprinted((buf buffer))
  (awith (-> buf unprinted)
	 (psetf (-> buf unprinted) nil)
	 (reverse it)))

(defun make-message-buffer()
  (make-instance 'message-buffer))

(defun make-buffer()
  (make-instance 'buffer))

(defun get-printed(buf)
  (-> buf printed))

;;Report

(defun message-report(message &optional (receiver level:*actor*))
  (add-message (get-message-buffer receiver) message))

(defun msg(str &optional (color (ui:color :gray)))
  (make-gui-string str color))

(define-compiler-macro msg(&whole form str &optional color)
  (if (stringp str)
      `(make-gui-string (load-time-value ,str) . ,(aif color (list it) '((ui:color :gray))))
      `(make-gui-string ,str . ,(aif color (list it) '((ui:color :gray))))))

(defgeneric simulate-noise(entity sound))

(defun simple-message(creature format-string &rest args)
  (message-report (msg (apply #'formatted format-string args)) creature))

(defun simple-note(creature format-string &rest args)
  (add-note (get-message-buffer creature) (msg (apply #'formatted format-string args))))

(defmethod report-death((creature proto-creature))
  (message-report (msg "~a dies." (word-capitalize (get-name creature)))))

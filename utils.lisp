(in-package :cl-user)

(defun build-class-property(form)
  (destructuring-bind (name initform &rest other) form
    `(,name :initform ,initform :allocation :class ,@other)))

(defun build-instance-property(form)
  (destructuring-bind (name initform &rest other) form
    `(,name :initform ,initform ,@other)))

(defmacro simple-defclass(name supers (&body instance-properties)
					 (&body class-properties))
  `(defclass ,name ,supers
     (,@(mapcar #'build-class-property class-properties)
      ,@(mapcar #'build-instance-property instance-properties))))

(export 'simple-defclass)

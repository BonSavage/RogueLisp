(in-package :rl.game)

(defconstant +build-info+ "Debug version")
(defun build-info() +build-info+)

(defun add-item(pos type &rest other-args)
  (add-entity pos (apply #'make-item type other-args)))

(defun add-items(pos count type &rest other-args)
  (add-entity pos (apply #'make-stack type count other-args)))

(defun add-trap(pos type &rest other-args)
  (add-entity pos (apply #'make-instance type other-args)))

(defun add-creature(pos type &rest other-args)
  (add-event (make-turn (apply #'make-creature type :pos pos other-args) 100)))

(defun add-effect(creature type &rest args)
  (rl.entity:invoke-effect creature (apply #'rl.effect:make-effect type args)))

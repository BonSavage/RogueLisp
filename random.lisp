(in-package :rnd)

(setf *random-state* (make-random-state t))

(defstruct dices
  (count 1 :type fixnum)
  (faces 6 :type fixnum)
  (base 0 :type fixnum))

(defun dices(count faces &key (base 0))
  (make-dices :count count :faces faces :base base))

(defun throw-dices(dice)
  (with-slots (count faces base) dice
    (apply #'+ (cons base (mapcar (lambda (n) (1+ (random n))) (make-list count :initial-element faces))))))

(defun dices-string(dices)
  (with-slots (count faces base) dices
    (let ((str (char-buffer "")))
      (if (not (zerop base))
	  (format str "~ad~a + ~a" count faces base)
	  (format str "~ad~a" count faces))
      str)))

(defun bernoulli(&optional (range 2))
  (zerop (random range)))

(defun interval(min max)
  (+ min (random (- max min))))

(defun fbernoulli(range)
  (<= (random 1.0) range))

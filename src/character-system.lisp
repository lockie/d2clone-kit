(in-package :d2clone-kit)


(defclass character-system (system)
  ((name :initform 'character)))

(defcomponent character character
  (target-x nil :type double-float)
  (target-y nil :type double-float))

(defmethod system-load ((system character-system))
  t)

(defmethod make-component ((system character-system) entity &rest parameters)
  (destructuring-bind (&key target-x target-y) parameters
    (with-character entity (x y)
      (setf x target-x)
      (setf y target-y))))

(defparameter *speed* 0.01d0)

(declaim
 (inline approx-equal)
 (ftype (function (double-float double-float &optional double-float) boolean) approx-equal))
(defun approx-equal (a b &optional (epsilon 0.01d0))
  (< (abs (- a b)) epsilon))

(defmethod system-update ((system character-system) dt)
  (with-characters
      (with-coordinate entity ()
        (if (and (approx-equal target-x x) (approx-equal target-y y))
            ;; XXX debug
            (progn
              (incf x (* 10 *speed* (signum (- target-x x))))
              (incf y (* 10 *speed* (signum (- target-y y))))
              (setf target-x (- target-x))
              (setf target-y (- target-y)))
            (progn
              (incf x (* *speed* (signum (- target-x x))))
              (incf y (* *speed* (signum (- target-y y)))))))))

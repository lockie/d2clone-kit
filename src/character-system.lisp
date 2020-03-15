(in-package :d2clone-kit)


(defclass character-system (system)
  ((name :initform 'character))
  (:documentation "Handles characters."))

(defcomponent character character
  (speed nil :type double-float)
  (target-x nil :type double-float)
  (target-y nil :type double-float))

(defmethod make-component ((system character-system) entity &rest parameters)
  (destructuring-bind (&key (speed 0.1d0) target-x target-y) parameters
    (with-character entity (s x y)
      (setf s speed)
      (setf x target-x)
      (setf y target-y))))

(declaim
 (inline approx-equal)
 (ftype (function (double-float double-float &optional double-float) boolean) approx-equal))
(defun approx-equal (a b &optional (epsilon 0.01d0))
  (< (abs (- a b)) epsilon))

(defmethod system-update ((system character-system) dt)
  ;; TODO : if mouse cursor is close to player when moving, movement is still jerky :(
  (with-characters
      (with-coordinate entity ()
        (with-sprite entity ()
          (if (and (approx-equal target-x x speed) (approx-equal target-y y speed))
              (when (eq stance 'walk)
                (switch-stance entity 'idle))
              (let ((a (atan (- target-y y) (- target-x x))))
                (setf angle a)
                (incf x (* speed (cos a)))
                (incf y (* (/ *tile-width* *tile-height*) speed (sin a)))
                (unless (eq stance 'walk)
                  (switch-stance entity 'walk))))))))

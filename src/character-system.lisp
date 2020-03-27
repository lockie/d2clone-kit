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

;; TODO : some sort of generic SoA class/macro with getter/setter functions
(declaim
 (inline set-character-target)
 (ftype (function (fixnum double-float double-float)) set-character-target))
(defun set-character-target (entity new-target-x new-target-y)
  "Sets character ENTITY new movement target to NEW-TARGET-X, NEW-TARGET-Y."
  (with-character entity ()
    (setf target-x new-target-x
          target-y new-target-y))
  (with-sprite entity ()
    (with-coordinate entity ()
      (setf angle (atan (* (- new-target-y y) 0.5d0) (- new-target-x x))))))

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
              (let ((direction-x (* 0.5d0 (cos angle)))
                    (direction-y (* 0.5d0 (/ *tile-width* *tile-height*) (sin angle))))
                (if (multiple-value-call #'collidesp
                      (tile-index (+ x direction-x)
                                  (+ y direction-y)))
                    (progn
                        (setf target-x x
                              target-y y)
                        (switch-stance entity 'idle))
                      (progn
                        (incf x (* speed 2d0 direction-x))
                        (incf y (* speed 2d0 direction-y))
                        (unless (eq stance 'walk)
                          (switch-stance entity 'walk))))))))))

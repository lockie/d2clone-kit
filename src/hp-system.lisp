(in-package :d2clone-kit)


(defsystem hp
  ()
  (:documentation "Handles hit points."))

(defcomponent (hp)
  (maximum-hp nil :type double-float)
  (current-hp nil :type double-float))

(defmethod make-component ((system hp-system) entity &rest parameters)
  (destructuring-bind (&key current maximum) parameters
    (make-hp entity :current-hp current :maximum-hp maximum)))

(declaim
 (inline set-hp)
 (ftype (function (fixnum double-float)) set-hp))
(defun set-hp (entity new-hp)
  "Sets current hit points of ENTITY to NEW-HP."
  (with-hp entity ()
    (cond
      ((> new-hp maximum-hp)
       (setf current-hp maximum-hp))
      ((<= new-hp 0d0)
       (let ((damage-fraction (/ (- current-hp new-hp) maximum-hp)))
         (setf current-hp 0d0)
         (delete-entity-actions entity)
         (issue entity-died :entity entity :damage-fraction damage-fraction)))
      (t
       (setf current-hp new-hp)))))

(declaim
 (inline deadp)
 (ftype (function (fixnum) boolean) deadp))
(defun deadp (entity)
  "Returns T when ENTITY is dead."
  (with-hp entity ()
    (zerop current-hp)))

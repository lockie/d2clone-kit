(in-package :d2clone-kit)


(defclass hp-system (system)
  ((name :initform 'hp))
  (:documentation "Handles hit points."))

(defcomponent hp hp
  (maximum-hp 1d0 :type double-float)
  (current-hp 1d0 :type double-float))

(defmethod make-component ((system hp-system) entity &rest parameters)
  (destructuring-bind (&key current maximum) parameters
    (with-hp entity ()
      (setf current-hp current
            maximum-hp maximum))))

(declaim
 (inline deadp)
 (ftype (function (fixnum) boolean) deadp))
(defun deadp (entity)
  "Returns T when ENTITY is dead."
  (with-hp entity ()
    (zerop current-hp)))

(defmethod system-update ((system hp-system) dt)
  (with-hps
    (when (zerop current-hp)
      (switch-stance entity :death))))

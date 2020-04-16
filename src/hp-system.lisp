(in-package :d2clone-kit)


(defclass hp-system (system)
  ((name :initform 'hp))
  (:documentation "Handles hit points."))

(defcomponent hp hp
  (maximum-hp nil :type double-float)
  (current-hp nil :type double-float))

(defmethod make-component ((system hp-system) entity &rest parameters)
  (destructuring-bind (&key current maximum) parameters
    (with-hp entity ()
      (setf current-hp current
            maximum-hp maximum))))

(declaim
 (inline set-hp)
 (ftype (function (fixnum double-float)) set-hp))
(defun set-hp (entity new-hp)
  "Sets current hit points of ENTITY to NEW-HP."
  (with-hp entity ()
    (cond
      ((<= new-hp 0d0)
       (setf current-hp 0d0)
       (issue entity-died :entity entity))
      (t
       (setf current-hp new-hp)))))

(declaim
 (inline deadp)
 (ftype (function (fixnum) boolean) deadp))
(defun deadp (entity)
  "Returns T when ENTITY is dead."
  (with-hp entity ()
    (zerop current-hp)))

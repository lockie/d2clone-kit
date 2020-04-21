(in-package :d2clone-kit)


(defclass mana-system (system)
  ((name :initform 'mana))
  (:documentation "Handles mana points."))

(defcomponent mana mana
  (maximum-mana nil :type double-float)
  (current-mana nil :type double-float))

(defmethod make-component ((system mana-system) entity &rest parameters)
  (destructuring-bind (&key current maximum) parameters
    (with-mana entity ()
      (setf current-mana current
            maximum-mana maximum))))

(in-package :d2clone-kit)


(defclass mana-system (system)
  ((name :initform 'mana))
  (:documentation "Handles mana points."))

(defcomponent mana mana
  (maximum-mana 1d0 :type double-float)
  (current-mana 1d0 :type double-float))

(defmethod make-component ((system mana-system) entity &rest parameters)
  (destructuring-bind (&key current maximum) parameters
    (with-mana entity ()
      (setf current-mana current
            maximum-mana maximum))))

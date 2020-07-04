(in-package :d2clone-kit)


(defsystem mana
  ()
  (:documentation "Handles mana points."))

(defcomponent (mana)
  (maximum-mana nil :type double-float)
  (current-mana nil :type double-float))

(defmethod make-component ((system mana-system) entity &rest parameters)
  (destructuring-bind (&key current maximum) parameters
    (make-mana entity :current-mana current :maximum-mana maximum)))

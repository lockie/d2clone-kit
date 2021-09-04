(in-package :d2clone-kit)


(defsystem credits
  ((ui-entity +invalid-entity+ :type fixnum))
  (:documentation "Handles credits screen."
   :order 2))

(declaim (inline credits-screen) (ftype (function () fixnum) credits-screen))
(defun credits-screen ()
  "Returns credits screen UI entity."
  (credits-system-ui-entity *credits-system*))

(defmethod system-initialize ((system credits-system))
  (setf (credits-system-ui-entity system)
        (make-object '((:ui :prefab :credits)))))

(defmethod system-finalize ((system credits-system))
  (with-system-slots ((ui-entity) :of credits-system :instance system)
    (when (entity-valid-p ui-entity)
      (delete-entity ui-entity))))

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
  (with-ui (credits-system-ui-entity system) ()
    (when-let (font (gethash :font parameters))
      (nk:allegro-font-del font))))

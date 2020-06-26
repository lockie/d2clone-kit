(in-package :d2clone-kit)


(defsystem menu
  ((ui-entity nil))
  (:documentation "Handles main menu."
   :order 1))

(declaim (inline main-menu) (ftype (function () fixnum) main-menu))
(defun main-menu ()
  "Returns main menu UI entity."
  (menu-system-ui-entity *menu-system*))

(defmethod system-initialize ((system menu-system))
  (setf (menu-system-ui-entity system)
        (make-object '((:ui :prefab :main-menu :on t)))))

(defhandler menu-system allegro-event (event event-type)
  :filter '(eq event-type :key-down)
  (let ((allegro-event (slot-value event 'event)))
    (when (eq (cffi:foreign-slot-value allegro-event '(:struct al:keyboard-event) 'al::keycode)
              :escape)
      (toggle-ui (menu-system-ui-entity system)))))

(in-package :d2clone-kit)

(defclass menu-system (system)
  ((name :initform 'menu)
   (ui-entity :initform nil))
  (:documentation "Handles main menu."))

(declaim (inline main-menu) (ftype (function () fixnum) main-menu))
(defun main-menu ()
  "Returns main menu UI entity."
  (slot-value (system-ref 'menu) 'ui-entity))

(defmethod system-initialize ((system menu-system))
  (with-slots (ui-entity) system
    (setf ui-entity (make-object '((:ui :prefab :main-menu :on t))))))

(defhandler menu-system allegro-event (event event-type)
  :filter '(eq event-type :key-down)
  (let ((allegro-event (slot-value event 'event)))
    (when (eq (cffi:foreign-slot-value allegro-event '(:struct al:keyboard-event) 'al::keycode)
              :escape)
      (with-slots (ui-entity) system
        (toggle-ui ui-entity)))))

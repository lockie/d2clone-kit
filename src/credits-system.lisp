(in-package :d2clone-kit)

(defclass credits-system (system)
  ((name :initform 'credits)
   (ui-entity :initform nil))
  (:documentation "Handles credits screen."))

(declaim (inline credits-screen) (ftype (function () fixnum) credits-screen))
(defun credits-screen ()
  "Returns credits screen UI entity."
  (slot-value (system-ref 'credits) 'ui-entity))

(defmethod system-initialize ((system credits-system))
  (with-slots (ui-entity) system
    (setf ui-entity (make-object '((:ui :prefab :credits))))))

(defhandler credits-system quit (event)
  (with-slots (ui-entity) system
    (with-ui ui-entity ()
      (when-let (font (gethash :font parameters))
        (nk:allegro-font-del font)))))

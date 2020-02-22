(in-package :d2clone-kit)


(defclass player-system (system)
  ((name :initform 'player)
   (entity :initform -1)
   (debug-entity :initform -1))
  (:documentation "Handles player character."))

(defcomponent player player)

(defmethod system-load ((system player-system))
  t)

(defmethod make-component ((system player-system) entity &rest parameters)
  (declare (ignore parameters))
  (setf (slot-value system 'entity) entity)
  (with-system-config-options ((debug-cursor))
    (when debug-cursor
      (let ((debug-entity (make-entity)))
        (setf (slot-value system 'debug-entity) debug-entity)
        (make-component (system-ref 'debug) debug-entity :order 2000)))))

(declaim (inline player-entity))
(defun player-entity ()
  "Returns current player entity."
  (slot-value (system-ref 'player) 'entity))

(declaim
 (inline mouse-position)
 (ftype (function (&optional cffi:foreign-pointer) (values fixnum fixnum)) mouse-position))
(defun mouse-position (&optional (event nil))
  "Get current mouse cursor coordinates using liballegro mouse event EVENT or by calling [al_get_mouse_state](https://liballeg.org/a5docs/trunk/mouse.html#al_get_mouse_state)."
  (macrolet
      ((mouse-position-values (type struct)
         `(cffi:with-foreign-slots ((al::x al::y) ,struct (:struct ,type))
            (values al::x al::y))))
    (if event
        (mouse-position-values al:mouse-event event)
        (al:with-current-mouse-state state
          (mouse-position-values al:mouse-state state)))))

(defmethod system-event ((system player-system) (event-type (eql :mouse-button-down)) event)
  (multiple-value-bind (x y) (mouse-position event)
    (multiple-value-bind (new-camera-screen-x new-camera-screen-y)
        (viewport->absolute x y)
      (multiple-value-bind (new-camera-x new-camera-y)
          (screen->map new-camera-screen-x new-camera-screen-y)
        (with-character (player-entity) ()
          (setf target-x new-camera-x
                target-y new-camera-y)))))
  t)

(defmethod system-draw ((system player-system) renderer)
  (with-system-config-options ((debug-cursor))
    (when debug-cursor
      (multiple-value-bind (map-x map-y)
          (multiple-value-call #'screen->map
            (multiple-value-call #'viewport->absolute
              (mouse-position)))
        (multiple-value-bind (x y)
            (multiple-value-call #'absolute->viewport
              (map->screen (coerce (floor map-x) 'double-float)
                           (coerce (floor map-y) 'double-float)))
          (add-debug-rectangle
           (slot-value system 'debug-entity)
           x y *tile-width* *tile-height* debug-cursor))))))

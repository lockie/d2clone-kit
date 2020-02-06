(in-package :d2clone-kit)


(defclass player-system (system)
  ((name :initform 'player)
   (entity :initform -1)
   (debug-entity :initform -1)))

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

(declaim
 (inline mouse-position)
 (ftype (function (&optional cffi:foreign-pointer) (values fixnum fixnum)) mouse-position))
(defun mouse-position (&optional (event nil))
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
    (multiple-value-bind (camera-screen-x camera-screen-y) (viewport->absolute x y)
      (multiple-value-bind (new-camera-x new-camera-y)
          (screen->map* camera-screen-x camera-screen-y)
        (incf new-camera-x (/ (mod (floor new-camera-y) 2) 2)) ;; XXX wut
        (with-coordinate (slot-value system 'entity) (player-x player-y)
          (setf player-x new-camera-x
                player-y new-camera-y))
        (with-camera (camera-x camera-y)
          (setf camera-x new-camera-x
                camera-y new-camera-y)))))
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

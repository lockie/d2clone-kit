(in-package :d2clone-kit)


(defclass player-system (system)
  ((name :initform 'player)
   (entity :initform nil)))

(defcomponent player player)

(defmethod system-load ((system player-system))
  t)

(defmethod make-component ((system player-system) entity &rest parameters)
  (declare (ignore parameters))
  (setf (slot-value system 'entity) entity)
  nil)

(defmethod system-event ((system player-system) (event-type (eql :mouse-button-down)) event)
  (cffi:with-foreign-slots ((al::x al::y) event (:struct al:mouse-event))
    (let ((x al::x);;(- al::x (truncate *tile-width* 2)))
          (y al::y))
      ;; (decf x (ceiling *tile-width* 2))  ;; XXX account for sprite offset
      ;; TODO : вроде смутно похоже на правду, но надо перепроверить
      (log-info "mouse click at ~a ~a" x y)
      (with-system-config-options ((display-width display-height))
        (with-camera (camera-x camera-y)
          ;; (setf x 399 y 301)
          (multiple-value-bind (camera-screen-x camera-screen-y)
              ;; (multiple-value-call #'viewport->absolute (map->screen camera-x camera-y))
              (viewport->absolute x y)
            (log-info "camera screen at ~a ~a" camera-screen-x camera-screen-y)
            (multiple-value-bind (xx yy)
                (screen->map camera-screen-x camera-screen-y)
              (log-info "which is world ~a ~a" xx yy))

            (with-coordinate (slot-value system 'entity) (player-x player-y)
              (setf (values camera-x camera-y) (screen->map camera-screen-x camera-screen-y))
              (setf (values player-x player-y) (screen->map camera-screen-x camera-screen-y))))))))
  t)

(defun mouse-position ()
  ;; TODO : optional struct arg; etypecase for it (mouse-state vs mouse-event)
  (al:with-current-mouse-state state
    (cffi:with-foreign-slots
        ((al::x al::y) state (:struct al:mouse-state))
      (values al::x al::y))))

(defmethod system-draw ((system player-system) renderer)
  (with-system-config-options ((debug-cursor))
    (when debug-cursor
      (render
       renderer 2000
       (multiple-value-bind (map-x map-y)
           (multiple-value-call #'screen->map
             (multiple-value-call #'viewport->absolute
               (mouse-position)))
         (multiple-value-bind (x y)
             (multiple-value-call #'absolute->viewport
               (map->screen (coerce (floor map-x) 'double-float)
                            (coerce (floor map-y) 'double-float)))
           #'(lambda ()
               (al:draw-filled-rectangle
                x y (+ x *tile-width*) (+ y *tile-height*)
                (al:map-rgba
                 (first debug-cursor)
                 (second debug-cursor)
                 (third debug-cursor)
                 (or (fourth debug-cursor) 0))))))))))

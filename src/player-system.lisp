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
    (let ((x (- al::x (truncate *tile-width* 2))) ;; account for sprite offset
          (y al:y))
      (log-info "mouse click at ~a ~a" x y)
      (with-system-config-options ((display-width display-height))
        (with-camera (camera-x camera-y)
          (incf camera-x (- x (ceiling display-width 2)))
          (incf camera-y (- y (ceiling display-height 2)))
          (with-point (slot-value system 'entity) (player-x player-y)
            (log-info "setting pos to ~a, ~a" camera-x camera-y)
            (setf player-x camera-x)
            (setf player-y camera-y)))))
    t))

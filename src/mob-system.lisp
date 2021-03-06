(in-package :d2clone-kit)


(defclass mob-system (system)
  ((name :initform 'mob))
  (:documentation "Handles mobs."))


(defcomponent mob mob
  (name nil :type string)
  (vision-range 10d0 :type double-float))

(defmethod make-component ((system mob-system) entity &rest parameters)
  (destructuring-bind (&key name (vision-range 10d0)) parameters
    (with-mob entity (mob-name mob-vision-range)
      (setf mob-name name
            mob-vision-range vision-range))))

(defmethod system-update ((system mob-system) dt)
  (let ((player-entity (player-entity)))
    (multiple-value-bind (player-x player-y)
        (with-coordinate player-entity (x y)
          (values x y))
      (with-mobs
          (unless (or (deadp entity)
                      (deadp player-entity))
            (with-coordinate entity ()
              (with-character entity ()
                (when (or (zerop (length path))
                          (destructuring-bind (dest-x . dest-y)
                              (aref path 0)
                            (or (not (= dest-x player-x))
                                (not (= dest-y player-y)))))
                  (when (< (euclidean-distance x y player-x player-y) vision-range)
                    (attack entity player-entity))))))))))

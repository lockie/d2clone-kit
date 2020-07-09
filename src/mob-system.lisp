(in-package :d2clone-kit)


(defsystem mob
  ()
  (:documentation "Handles mobs."))

(defcomponent (mob)
  (name nil :type string)
  (vision-range nil :type double-float))

(defmethod make-component ((system mob-system) entity &rest parameters)
  (destructuring-bind (&key name (vision-range 10d0)) parameters
    (make-mob entity :name name :vision-range vision-range)))

(defmethod system-update ((system mob-system) dt)
  (let ((player-entity (player-entity)))
    (when (and (entity-valid-p player-entity) (not (deadp player-entity)))
      (with-coordinate player-entity (player-x player-y)
        (with-mobs
            (unless (deadp entity)
              (with-coordinate entity ()
                (with-character entity ()
                  (when (or (zerop (length path))
                            (destructuring-bind (dest-x . dest-y)
                                (aref path 0)
                              (or (not (= (the double-float dest-x) player-x))
                                  (not (= (the double-float dest-y) player-y)))))
                    (when (< (euclidean-distance x y player-x player-y) vision-range)
                      (attack entity player-entity)))))))))))

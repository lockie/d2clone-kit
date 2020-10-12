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

(defmethod system-update ((system mob-system))
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

(defun draw-mob-health-bar (entity renderer)
  "Draws mob's health bar for the mob ENTITY with RENDERER."
  (render
   renderer
   10000d0
   (let ((entity entity))
     #'(lambda ()
         (with-system-config-options ((display-width))
           (with-mob entity ()
             (with-hp entity ()
               (let* ((text-width (al:get-text-width (ui-font-large) name))
                      (bar-width (truncate (* (+ text-width 40)
                                              (/ current-hp maximum-hp))))
                      (name-offset (truncate (- display-width text-width) 2)))
                 (al:draw-filled-rectangle
                  (- name-offset 20) 24
                  (+ name-offset text-width 20) 52
                  (al:map-rgba 0 0 0 220))
                 (al:draw-filled-rectangle
                  (- name-offset 20) 24
                  (+ name-offset bar-width -20) 52
                  (al:map-rgba 40 0 0 0))
                 (al:draw-text
                  (ui-font-large) (al:map-rgba 255 255 255 10) name-offset 26 0 name)))))))))

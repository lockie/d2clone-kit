(in-package :d2clone-kit)


(defsystem combat
  ()
  (:documentation "Handles close combat."
   :order 1))

(defcomponent combat combat
  (target +invalid-entity+ :type fixnum)
  (attack-range 2d0 :type double-float)
  (min-damage 1d0 :type double-float)  ;; TODO : use rl-pcg dice rolls here?..
  (max-damage nil :type double-float))

(defmethod make-component ((system combat-system) entity &rest parameters)
  (declare (ignore system entity parameters))
  (destructuring-bind (&key (attack-range 2d0) (min-damage 1d0) max-damage) parameters
    (with-combat entity (target entity-attack-range entity-min-damage entity-max-damage)
      (setf entity-attack-range attack-range
            entity-min-damage min-damage
            entity-max-damage max-damage))))

(defun attack (attacker-entity target-entity)
  "Initiates a close combat attack of TARGET-ENTITY by ATTACKER-ENTITY."
  (unless (= attacker-entity target-entity)  ;; prevent self-harm lol
    (with-sprite attacker-entity ()
      (unless (eq stance :swing)
        (with-combat attacker-entity ()
          (setf target target-entity))))))

(defconstant +stun-threshold+ 0.08d0)

(defmethod system-update ((system combat-system) dt)
  (with-combats
      (unless (or (not (entity-valid-p target)) (deadp entity))
        (with-sprite entity ()
          (with-coordinate entity (current-x current-y)
            (with-coordinate target (attack-target-x attack-target-y)
              (with-character entity ()
                (cond
                  ;; track target
                  ((> (euclidean-distance attack-target-x attack-target-y current-x current-y)
                      attack-range)
                   (destructuring-bind (final-target-x . final-target-y)
                       (if (length= 0 path)
                           (cons current-x current-y)
                           (simple-vector-peek path))
                     (unless (and (= final-target-x attack-target-x)
                                  (= final-target-y attack-target-y))
                       (set-character-target entity attack-target-x attack-target-y)
                       (when (length= 0 path)
                         (setf target +invalid-entity+)))))
                  ;; start the blow
                  (t
                   (stop-entity entity)
                   (setf angle (face-target current-x current-y attack-target-x attack-target-y))
                   (switch-stance entity :swing)))
                ;; land the blow
                (when (and (entity-valid-p target)  ;; zero-length path case
                           (eq stance :swing)
                           (= frame (car (last (gethash :swing stances)))))
                  (when (<= (euclidean-distance target-x target-y current-x current-y)
                            attack-range)
                    (with-hp target (target-max-hp target-current-hp)
                      (let ((damage (+ min-damage (random (- max-damage min-damage)))))
                        (set-hp target (- target-current-hp damage))
                        (when (> damage (* target-max-hp +stun-threshold+))
                          (unless (zerop target-current-hp)
                            (switch-stance target :hit))
                          (with-combat target (targets-target)
                            (setf targets-target +invalid-entity+))))))
                  (setf target +invalid-entity+)))))))))

(in-package :d2clone-kit)


(defclass combat-system (system)
  ((name :initform 'combat)
   (order :initform 1))
  (:documentation "Handles close combat."))

(defcomponent combat combat
  (target -1 :type fixnum)
  (attack-range 1.5d0 :type double-float))

(defmethod make-component ((system combat-system) entity &rest parameters)
  (declare (ignore system entity parameters))
  nil)

(defun attack (attacker-entity target-entity)
  "Initiates a close combat attack of TARGET-ENTITY by ATTACKER-ENTITY."
  (unless (= attacker-entity target-entity)  ;; prevent self-harm lol
    (with-sprite attacker-entity ()
      (unless (eq stance :swing)
        (with-combat attacker-entity ()
          (setf target target-entity))))))

(defmethod system-update ((system combat-system) dt)
  (with-combats
      (unless (or (minusp target) (deadp entity))
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
                       (set-character-target entity attack-target-x attack-target-y))))
                  ;; start the blow
                  (t
                   (stop-entity entity)
                   (setf angle (face-target current-x current-y attack-target-x attack-target-y))
                   (switch-stance entity :swing)))
                ;; land the blow
                (when (and (eq stance :swing) (= frame (car (last (gethash :swing stances)))))
                  (when (<= (euclidean-distance target-x target-y current-x current-y)
                            attack-range)
                    (switch-stance target :hit)
                    (with-combat target (targets-target)
                      (setf targets-target -1))
                    (with-hp target (target-max-hp target-current-hp)
                      (let ((damage (random (* target-max-hp 0.2d0))))
                        (set-hp target (- target-current-hp damage)))))
                  (setf target -1)))))))))

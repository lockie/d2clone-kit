(in-package :d2clone-kit)


(defsystem combat
  ()
  (:documentation "Handles close combat."
   :order 1))

(defcomponent (combat)
  (attack-range nil :type double-float)
  (min-damage nil :type double-float)  ;; TODO : use rl-pcg dice rolls here?..
  (max-damage nil :type double-float))

(defaction swing
    ((target +invalid-entity+ :type fixnum))
    (:documentation "Close combat attack action.")

  (defmethod initialize-action ((type (eql :swing)) action)
    (let ((entity (action-entity action)))
      (with-swing-action action ()
        (with-combat entity ()
          (make-track-action entity
                             :parent action
                             :target target
                             :target-distance attack-range)))))

  (defmethod finalize-action ((type (eql :swing)) action)
    (switch-stance (action-entity action) :idle)))

(defconstant +stun-threshold+ 0.08d0)

(declaim
 (inline equipped-weapon-class)
 (ftype (function (fixnum) keyword) equipped-weapon-class))
(defun equipped-weapon-class (entity)
  (with-sprite entity ()
    (loop :for layer :being :the :hash-key :of layers-toggled
          :using (hash-value on)
          :when on
          :do (when-let (weapon-class (layer-property entity :weapon-class))
                (return weapon-class))
          :finally (return :fists))))

(defperformer swing (action target)
  (let* ((entity (action-entity action)))
    (with-combat entity ()
      (with-coordinate entity (current-x current-y)
        (with-coordinate target (attack-target-x attack-target-y)
          (if (> (euclidean-distance attack-target-x attack-target-y
                                     current-x current-y)
                 attack-range)
              (when (and (not (index-valid-p (action-child action)))
                         (or (not (eq (current-stance entity) :swing))
                             (stance-finished-p entity)))
                (delete-action action))
              (with-sprite entity ()
                (unless (eq (current-stance entity) :swing)
                  (setf angle (face-target current-x current-y
                                           attack-target-x attack-target-y)))
                (switch-stance entity :swing)
                (when (and (eq (current-stance entity) :swing)
                           (stance-finished-p entity))
                  (with-hp target (target-max-hp target-current-hp)
                    ;; TODO : refactor out damage / stuns
                    (with-combat entity ()
                      (let ((damage (+ min-damage
                                       (random (- max-damage min-damage)))))
                        ;; TODO (#49): add impact sound component on entity
                        ;; being attacked. Get it from a table (weapon class,
                        ;; armor class) -> sound prefab. Get armor class from
                        ;; call to layer-property?..
                        (make-component
                         *sound-system* entity
                         :prefab (table-value-ref
                                  :impact-sounds
                                  `(:armor-class
                                    ,(layer-property target :armor-class)
                                    :weapon-class
                                    ,(equipped-weapon-class entity))))
                        (set-hp target (- target-current-hp damage))
                        (when (> damage (* target-max-hp +stun-threshold+))
                          (unless (zerop target-current-hp)
                            (switch-stance target :hit))
                          (delete-entity-actions target))))
                    (delete-action action))))))))))

(defmethod make-component ((system combat-system) entity &rest parameters)
  (destructuring-bind (&key (attack-range 2d0) (min-damage 1d0) max-damage)
      parameters
    (make-combat entity
                 :attack-range attack-range
                 :min-damage min-damage
                 :max-damage max-damage)))

(declaim (ftype (function (fixnum fixnum)) attack))
(defun attack (attacker-entity target-entity)
  "Initiates a close combat attack of TARGET-ENTITY by ATTACKER-ENTITY."
  (if-let (swing-action (has-action-p attacker-entity :swing))
    (with-swing-action swing-action ()
      (setf target target-entity))
    (make-swing-action attacker-entity :target target-entity)))

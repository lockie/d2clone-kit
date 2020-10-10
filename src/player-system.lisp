(in-package :d2clone-kit)


(defsystem player
  ((mouse-pressed-p nil :type boolean)
   (entity +invalid-entity+  :type fixnum)
   (last-target +invalid-entity+ :type fixnum)
   (orb (cffi:null-pointer) :type cffi:foreign-pointer)
   (orb-fill (cffi:null-pointer) :type cffi:foreign-pointer)
   (orb-flare (cffi:null-pointer) :type cffi:foreign-pointer)
   (orb-tmp (cffi:null-pointer) :type cffi:foreign-pointer)
   (debug-entity +invalid-entity+ :type fixnum))
  (:documentation "Handles player character."
   :order 1))

(defcomponent (player))

(defmethod make-component ((system player-system) entity &rest parameters)
  (declare (ignore parameters))
    (setf (camera-target) entity
          (player-system-entity system) entity))

(declaim
 (inline player-entity)
 (ftype (function () fixnum) player-entity))
(defun player-entity ()
  "Returns current player entity."
  (player-system-entity *player-system*))

(declaim
 (inline mouse-position)
 (ftype (function (&optional (or cffi:foreign-pointer null)) (values fixnum fixnum)) mouse-position))
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

(defun character-under-cursor (cursor-map-x cursor-map-y)
  ;; XXX HACK. Need to use some sort of bounding box for sprites; see #21
  (with-mobs
    (with-coordinate entity ()
      (when (and (< (euclidean-distance x y cursor-map-x cursor-map-y) 1.4d0)
                 (not (deadp entity)))
        (return entity)))))

(defun target-player (&optional (mouse-event nil))
  "Set new player character target according to MOUSE-EVENT or current mouse cursor position."
  (let ((player-entity (player-entity)))
    (when (and (entity-valid-p player-entity) (not (deadp player-entity)))
      (with-system-slots ((mouse-pressed-p last-target) player-system nil :read-only nil)
        (if (entity-valid-p last-target)
            (attack player-entity last-target)
            (multiple-value-bind (new-x new-y)
                (multiple-value-call #'screen->orthogonal*
                  (multiple-value-call #'viewport->absolute
                    (mouse-position mouse-event)))
              (if-let (target (and
                               (not mouse-pressed-p)
                               (character-under-cursor new-x new-y)))
                (attack player-entity (setf last-target target))
                (with-combat player-entity ()
                  (setf target -1)
                  (set-character-target player-entity new-x new-y)))))))))

(defhandler (player-system allegro-event)
  (let ((struct (allegro-event-struct event)))
    (case (allegro-event-type event)
      (:mouse-button-down
       (when (= 1 (cffi:foreign-slot-value struct '(:struct al:mouse-event) 'al::button))
         (target-player struct)
         (setf (player-system-mouse-pressed-p system) t)))
      (:mouse-button-up
       (when (= 1 (cffi:foreign-slot-value struct '(:struct al:mouse-event) 'al::button))
         (setf (player-system-mouse-pressed-p system) nil
               (player-system-last-target system) +invalid-entity+))))))

(defmethod system-initialize ((system player-system))
  (with-system-config-options ((debug-cursor))
    (with-system-slots ((orb orb-fill orb-flare orb-tmp debug-entity)
                        player-system system :read-only nil)
      (setf orb (ensure-loaded #'al:load-bitmap "images/orb.png")
            orb-fill (ensure-loaded #'al:load-bitmap "images/orb-fill.png")
            orb-flare (ensure-loaded #'al:load-bitmap "images/orb-flare.png")
            orb-tmp (al:create-bitmap (al:get-bitmap-width orb-fill)
                                      (al:get-bitmap-height orb-fill))
            debug-entity (if debug-cursor
                             (make-object '((:debug :order 2000d0)))
                             +invalid-entity+)))))

(defmethod system-finalize ((system player-system))
  (with-system-slots ((orb orb-fill orb-flare orb-tmp debug-entity) player-system system)
    (al:destroy-bitmap orb)
    (al:destroy-bitmap orb-fill)
    (al:destroy-bitmap orb-flare)
    (al:destroy-bitmap orb-tmp)
    (when (entity-valid-p debug-entity)
      (delete-entity debug-entity))))

(defmethod system-update ((system player-system) dt)
  (with-system-slots ((mouse-pressed-p last-target) player-system system)
    (unless (and (entity-valid-p last-target)
                 (deadp last-target))
      (when mouse-pressed-p
        (target-player)))))

(defmethod system-draw ((system player-system) renderer)
  (block nil
    (with-system-config-options ((display-width display-height))
      (with-system-slots ((entity orb orb-fill orb-flare orb-tmp) player-system system)
        (unless (entity-valid-p entity) (return))
        (with-hp entity ()
          (with-mana entity ()
            (render
             renderer
             10000d0
             #'(lambda ()
                 (let* ((fill-width (al:get-bitmap-width orb-fill))
                        (fill-height (al:get-bitmap-height orb-fill))
                        (orb-width (al:get-bitmap-width orb))
                        (orb-height (al:get-bitmap-height orb))
                        (fill-shift (+ fill-height (ceiling (- orb-height fill-height) 2))))
                   ;; TODO : draw those in hp-system and mana-system?..
                   (flet ((fill-orb (color)
                            (al:set-target-bitmap orb-tmp)
                            (al:draw-bitmap orb-fill 0 0 0)
                            (al:set-blender
                             (cffi:foreign-enum-value 'al::blend-operations :dest-minus-src)
                             (cffi:foreign-enum-value 'al::blend-mode :one)
                             (cffi:foreign-enum-value 'al::blend-mode :inverse-alpha))
                            (al:draw-filled-rectangle 0 0 fill-width fill-height color)
                            (al:set-target-backbuffer (al:get-current-display))
                            (al:set-blender
                             (cffi:foreign-enum-value 'al::blend-operations :add)
                             (cffi:foreign-enum-value 'al::blend-mode :one)
                             (cffi:foreign-enum-value 'al::blend-mode :inverse-alpha)))
                          (draw-filling (fraction dx)
                            (let ((shift (floor (* fill-shift (- 1d0 fraction)))))
                              (al:draw-bitmap-region
                               orb-tmp
                               (floor (- fill-width orb-width) 2)
                               (+ shift (floor (- fill-height orb-height) 2))
                               orb-width orb-height
                               dx (+ shift (- display-height orb-height))
                               0))))
                     (al:hold-bitmap-drawing t)
                     (al:draw-bitmap orb 0 (- display-height orb-height) 0)
                     (al:draw-bitmap orb (- display-width orb-width) (- display-height orb-height) 0)
                     (al:hold-bitmap-drawing nil)
                     ;; hp orb
                     (fill-orb (al:map-rgba 30 255 255 30))
                     (draw-filling (/ current-hp maximum-hp) 0)
                     ;; mana orb
                     (fill-orb (al:map-rgba 255 255 30 30))
                     (draw-filling (/ current-mana maximum-mana) (- display-width orb-width))
                     (al:hold-bitmap-drawing t)
                     (al:draw-bitmap orb-flare
                                     0 (- display-height orb-height) 0)
                     (al:draw-bitmap orb-flare
                                     (- display-width orb-width)
                                     (- display-height orb-height) 0)
                     (al:hold-bitmap-drawing nil))))))))

      (multiple-value-bind (cursor-map-x cursor-map-y)
          (multiple-value-call #'screen->orthogonal*
            (multiple-value-call #'viewport->absolute
              (mouse-position)))

        (flet ((draw-mob-health-bar (entity)
                 (unless (= entity (player-entity))
                   (render
                    renderer
                    10000d0
                    (let ((entity entity))
                      #'(lambda ()
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
                                 (ui-font-large)
                                 (al:map-rgba 255 255 255 10)
                                 name-offset 26
                                 0 name))))))))))
          (with-system-slots ((mouse-pressed-p last-target) player-system system)
            (if (entity-valid-p last-target)
                (unless (deadp last-target)
                  (draw-mob-health-bar last-target))
                (unless mouse-pressed-p
                  (when-let (target (character-under-cursor cursor-map-x cursor-map-y))
                    (draw-mob-health-bar target))))))

        (with-system-config-options ((debug-cursor))
          (when debug-cursor
            (multiple-value-bind (x y)
                (multiple-value-call #'absolute->viewport
                  (orthogonal->screen (coerce (truncate cursor-map-x) 'double-float)
                                      (coerce (truncate cursor-map-y) 'double-float)))
              (add-debug-rectangle
               (player-system-debug-entity system)
               x y *tile-width* *tile-height* debug-cursor))))))))

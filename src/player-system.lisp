(in-package :d2clone-kit)


(defclass player-system (system)
  ((name :initform 'player)
   (mouse-pressed :initform nil)
   (entity :initform -1)
   (orb :initform nil)
   (orb-fill :initform nil)
   (orb-flare :initform nil)
   (orb-tmp :initform nil)
   (debug-entity :initform -1))
  (:documentation "Handles player character."))

(defcomponent player player)

(defmethod make-component ((system player-system) player-entity &rest parameters)
  (declare (ignore parameters))
  (with-slots (entity orb orb-fill orb-flare orb-tmp) system
    (setf entity player-entity
          orb (al:load-bitmap "images/orb.png")
          orb-fill (al:load-bitmap "images/orb-fill.png")
          orb-flare (al:load-bitmap "images/orb-flare.png")
          orb-tmp (al:create-bitmap (al:get-bitmap-width orb-fill)
                                    (al:get-bitmap-height orb-fill))))
  (with-system-config-options ((debug-cursor))
    (when debug-cursor
      (let ((debug-entity (make-entity)))
        (setf (slot-value system 'debug-entity) debug-entity)
        (make-component (system-ref 'debug) debug-entity :order 2000d0)))))

(declaim (inline player-entity))
(defun player-entity ()
  "Returns current player entity."
  (slot-value (system-ref 'player) 'entity))

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

(defun target-player (&optional (mouse-event nil))
  "Set new player character target according to MOUSE-EVENT or current mouse cursor position."
  (multiple-value-bind (x y) (mouse-position mouse-event)
    (multiple-value-bind (new-screen-x new-screen-y)
        (viewport->absolute x y)
      (multiple-value-bind (new-x new-y)
          (screen->map new-screen-x new-screen-y)
        (set-character-target (player-entity) new-x new-y)))))

(defhandler player-system allegro-event (event event-type)
  :filter '(eq event-type :mouse-button-down)
  (let ((allegro-event (slot-value event 'event)))
    (when (= 1 (cffi:foreign-slot-value allegro-event '(:struct al:mouse-event) 'al::button))
      (target-player allegro-event)
      (setf (slot-value system 'mouse-pressed) t))))

(defhandler player-system allegro-event (event event-type)
  :filter '(eq event-type :mouse-button-up)
  (let ((allegro-event (slot-value event 'event)))
    (when (= 1 (cffi:foreign-slot-value allegro-event '(:struct al:mouse-event) 'al::button))
      (setf (slot-value system 'mouse-pressed) nil))))

(defmethod system-update ((system player-system) dt)
  (when (slot-value system 'mouse-pressed)
    (target-player)))

(defmethod system-draw ((system player-system) renderer)
  (with-system-config-options ((display-width display-height))
    (with-slots (entity orb orb-fill orb-flare orb-tmp) system
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
                 (flet ((do-fill (color)
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
                           (cffi:foreign-enum-value 'al::blend-mode :inverse-alpha))))
                   (al:hold-bitmap-drawing t)
                   (al:draw-bitmap orb 0 (- display-height orb-height) 0)
                   (al:draw-bitmap orb (- display-width orb-width) (- display-height orb-height) 0)
                   (al:hold-bitmap-drawing nil)
                   ;; fill hp orb
                   (do-fill (al:map-rgba 30 255 255 30))
                   (let ((shift (floor (* fill-shift (- 1d0 (/ current-hp maximum-hp))))))
                     (al:draw-bitmap-region
                      orb-tmp
                      (floor (- fill-width orb-width) 2)
                      (+ shift (floor (- fill-height orb-height) 2))
                      orb-width orb-height
                      0 (+ shift (- display-height orb-height))
                      0))
                   ;; fill mana orb
                   (do-fill (al:map-rgba 255 255 30 30))
                   (let ((shift (floor (* fill-shift (- 1d0 (/ current-mana maximum-mana))))))
                     (al:draw-bitmap-region
                      orb-tmp
                      (floor (- fill-width orb-width) 2)
                      (+ shift (floor (- fill-height orb-height) 2))
                      orb-width orb-height
                      (- display-width orb-width)
                      (+ shift (- display-height orb-height))
                      0))
                   (al:hold-bitmap-drawing t)
                   (al:draw-bitmap orb-flare
                                   0 (- display-height orb-height) 0)
                   (al:draw-bitmap orb-flare
                                   (- display-width orb-width)
                                   (- display-height orb-height) 0)
                   (al:hold-bitmap-drawing nil)))))))))
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

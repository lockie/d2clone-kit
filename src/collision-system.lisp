(in-package :d2clone-kit)


(defclass collision-system (system)
  ((name :initform 'collision)
   (collision-map :initform nil))
  (:documentation "Handles object collisions."))

;; TODO : take into account characters?.. character movement event?..

;; TODO : delete component event?..

(defhandler collision-system component-created (event entity system-name)
  :filter '(eq system-name 'map)
  (with-slots (collision-map) system
    (unless collision-map
      (setf collision-map (make-sparse-matrix :element-type 'boolean)))
    (with-coordinate entity ()
      (let ((start-x (truncate x))
            (start-y (truncate y)))
        (with-map-chunk entity ()
          (loop for layer across (tiled-map-layers tiled-map)
                do (loop with data = (tiled-layer-data layer)
                         for y from 0 below (tiled-layer-height layer)
                         do (loop for x from 0 below (tiled-layer-width layer)
                                  for tile = (aref data y x)
                                  do (when (tile-property tiles-properties tile 'collides)
                                       (setf
                                        (sparse-matrix-ref collision-map
                                                           (list (+ x start-x) (+ y start-y)))
                                        t))))))))))

(defmethod collides ((sytem collision-system) x y)
  "Returns whether tile located at integer map coordinates X, Y does collide with other objects
using collision system SYSTEM."
  (sparse-matrix-ref (slot-value system 'collision-map) (list x y)))

(declaim
 (inline collidesp)
 (ftype (function (fixnum fixnum) boolean) collidesp))
(defun collidesp (x y)
  "Returns whether tile located at integer map coordinates X, Y does collide with other objects."
  (sparse-matrix-ref (slot-value (system-ref 'collision) 'collision-map) (list x y)))

(defhandler collision-system quit (event)
  (setf (slot-value system 'collision-map) nil))

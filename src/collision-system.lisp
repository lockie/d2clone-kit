(in-package :d2clone-kit)


(defclass collision-system (system)
  ((name :initform 'collision)
   (collision-map :initform nil)
   (characters-collision-map :initform nil)
   (debug-entity :initform nil))
  (:documentation "Handles object collisions.

To make tile collide (e.g. be non-walkable by characters), set custom
boolean property *collides* to *true* in Tiled tileset."))

;; TODO : delete component event?..

;; TODO : optimize this by packing coordinates into single fixnum (31 bits ought to be enough for anyone)

(defhandler collision-system component-created (event entity system-name)
  :filter '(eq system-name 'map)
  (with-slots (collision-map) system
    (unless collision-map
      (setf collision-map (make-sparse-matrix)))
    (with-coordinate entity ()
      (let ((start-x (truncate x))
            (start-y (truncate y)))
        (with-map-chunk entity ()
          (loop :for layer :across (tiled-map-layers tiled-map)
                :do (loop :with data := (tiled-layer-data layer)
                          :for y :from 0 :below (tiled-layer-height layer)
                          :do (loop :for x :from 0 :below (tiled-layer-width layer)
                                    :for tile := (aref data y x)
                                    :do (when (tile-property tiles-properties tile 'collides)
                                          (multiple-value-bind (ortho-x ortho-y)
                                              (isometric->orthogonal*
                                               (coerce x 'double-float)
                                               (coerce y 'double-float))
                                            (setf
                                             (sparse-matrix-ref
                                              collision-map
                                              (cons (+ (truncate ortho-x) start-x)
                                                    (+ (truncate ortho-y) start-y)))
                                             t)))))))))))

(defhandler collision-system component-created (event entity system-name)
  :filter '(eq system-name 'character)
  (with-slots (characters-collision-map) system
    (unless characters-collision-map
      (setf characters-collision-map (make-sparse-matrix)))
    (with-coordinate entity ()
      ;; TODO : consider character size (#21)
      (setf (sparse-matrix-ref characters-collision-map (cons (round x) (round y))) entity))))

(defhandler collision-system character-moved (event entity old-x old-y new-x new-y)
  ;; TODO : consider character size (#21)
  (let ((old-int-x (round old-x))
        (old-int-y (round old-y))
        (new-int-x (round new-x))
        (new-int-y (round new-y)))
    (unless (and (= old-int-x new-int-x) (= old-int-y new-int-y))
      (with-slots (characters-collision-map) system
        (sparse-matrix-remove characters-collision-map (cons old-int-x old-int-y))
        (setf (sparse-matrix-ref characters-collision-map (cons new-int-x new-int-y)) entity)))))

(defhandler collision-system entity-died (event entity)
  (with-coordinate entity ()
    (with-slots (characters-collision-map) system
      (sparse-matrix-remove characters-collision-map (cons (round x) (round y))))))

(defmethod character-at ((system collision-system) x y)
  "Returns character entity at ingeter map coordinates X, Y or NIL if there's no character there."
  (sparse-matrix-ref (slot-value system 'characters-collision-map) (cons x y)))

(defmethod collides ((system collision-system) x y &key (character nil))
  "Returns whether tile located at integer map coordinates X, Y does collide with other objects
using collision system SYSTEM.
CHARACTER, when non-NIL, specifies character entity to check for collisions with other characters."
  (with-slots (collision-map characters-collision-map) system
    (let ((point (cons x y)))
      (or
       (sparse-matrix-ref collision-map point)
       (if character
           (let ((entity (sparse-matrix-ref characters-collision-map point)))
             (if entity (not (= character entity)) nil))
           nil)))))

(declaim
 (inline collidesp)
 (ftype (function (fixnum fixnum &key (:character (or fixnum null))) boolean) collidesp))
(defun collidesp (x y &key (character nil))
  "Returns whether tile located at integer map coordinates X, Y does collide with other objects.
CHARACTER, when non-NIL, specifies character entity to check for collisions with other characters."
  (with-slots (collision-map characters-collision-map) (system-ref 'collision)
    (let ((point (cons x y)))
      (or
       (sparse-matrix-ref collision-map point)
       (if character
           (let ((entity (sparse-matrix-ref characters-collision-map point)))
             (if entity (not (= character entity)) nil))
           nil)))))

(defmethod system-draw ((system collision-system) renderer)
  (with-system-config-options ((debug-collisions))
    (when debug-collisions
      (with-slots (debug-entity collision-map) system
        (unless debug-entity
          (setf debug-entity (make-entity))
          (make-component (system-ref 'debug) debug-entity :order 2000d0))
        (sparse-matrix-traverse
         collision-map
         #'(lambda (position value)
             (declare (ignore value))
             (multiple-value-bind (x y)
                 (multiple-value-call #'absolute->viewport
                   (orthogonal->screen
                    (coerce (car position) 'double-float)
                    (coerce (cdr position) 'double-float)))
               (add-debug-tile-rhomb debug-entity x y debug-collisions t))))))))

(defhandler collision-system quit (event)
  (with-slots (collision-map characters-collision-map) system
    (setf collision-map nil
          characters-collision-map nil)))

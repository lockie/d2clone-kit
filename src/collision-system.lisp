(in-package :d2clone-kit)


(defclass collision-system (system)
  ((name :initform 'collision)
   (collision-map :initform nil)
   (characters-collision-map :initform nil))
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
                                          (setf
                                           (sparse-matrix-ref collision-map
                                                              (cons (+ x start-x) (+ y start-y)))
                                           t))))))))))

(defhandler collision-system component-created (event entity system-name)
  :filter '(eq system-name 'character)
  (with-slots (characters-collision-map) system
    (unless characters-collision-map
      (setf characters-collision-map (make-sparse-matrix)))
    (multiple-value-bind (col row)
        (with-coordinate entity ()
          (tile-index x y))
      (setf (sparse-matrix-ref characters-collision-map (cons col row)) entity))))

(defhandler collision-system character-moved (event entity old-x old-y new-x new-y)
  (multiple-value-bind (old-col old-row)
      (tile-index old-x old-y)
    (multiple-value-bind (new-col new-row)
        (tile-index new-x new-y)
      (unless (and (= old-x new-x) (= old-y new-y))
        (with-slots (characters-collision-map) system
          (sparse-matrix-remove characters-collision-map (cons old-col old-row))
          (setf (sparse-matrix-ref characters-collision-map (cons new-col new-row)) entity))))))

(defmethod collides ((sytem collision-system) x y &key (character nil))
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

(defhandler collision-system quit (event)
  (with-slots (collision-map characters-collision-map) system
    (setf collision-map nil
          characters-collision-map nil)))

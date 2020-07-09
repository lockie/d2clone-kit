(in-package :d2clone-kit)


(defsystem collision
  ((map (make-sparse-matrix) :type sparse-matrix)
   (characters-map (make-sparse-matrix) :type sparse-matrix)
   (debug-entity +invalid-entity+ :type fixnum))
  (:documentation "Handles object collisions.

To make tile collide (e.g. be non-walkable by characters), set custom
boolean property *collides* to *true* in Tiled tileset."))

;; TODO : optimize this by packing coordinates into single fixnum (31 bits ought to be enough for anyone)

(defhandler collision-system component-created (event entity system-name)
  :filter '(eq system-name 'map)
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
                                            (collision-system-map system)
                                            (cons (+ (truncate ortho-x) start-x)
                                                  (+ (truncate ortho-y) start-y)))
                                           entity))))))))))

(defhandler collision-system entity-deleted (event entity)
  :filter '(has-component-p (gethash :map *systems*) entity) ;; HACK: *map-system* does not work here
  (with-system-slots ((map) collision-system system)
    (sparse-matrix-traverse
     map
     #'(lambda (position value)
         (when (= value entity)
           (sparse-matrix-remove map position))))))

(defhandler collision-system component-created (event entity system-name)
  :filter '(eq system-name 'character)
  (with-coordinate entity ()
    ;; TODO : consider character size (#21)
    (setf (sparse-matrix-ref (collision-system-characters-map system) (cons (round x) (round y)))
          entity)))

(defhandler collision-system entity-deleted (event entity)
  :filter '(has-component-p (gethash :character *systems*) entity) ;; HACK
  (with-coordinate entity ()
    (sparse-matrix-remove (collision-system-characters-map system) (cons (round x) (round y)))))

(defhandler collision-system character-moved (event entity old-x old-y new-x new-y)
  ;; TODO : consider character size (#21)
  (let ((old-int-x (round old-x))
        (old-int-y (round old-y))
        (new-int-x (round new-x))
        (new-int-y (round new-y)))
    (unless (and (= old-int-x new-int-x) (= old-int-y new-int-y))
      (with-system-slots ((characters-map) collision-system system)
        (sparse-matrix-remove characters-map (cons old-int-x old-int-y))
        (setf (sparse-matrix-ref characters-map (cons new-int-x new-int-y))
              entity)))))

(defhandler collision-system entity-died (event entity)
  (with-coordinate entity ()
    (sparse-matrix-remove (collision-system-characters-map system) (cons (round x) (round y)))))

(declaim (inline character-at) (ftype (function (fixnum fixnum) fixnum) character-at))
(defun character-at (x y)
  "Returns character entity at ingeter map coordinates X, Y or NIL if there's no character there."
  (values (sparse-matrix-ref (collision-system-characters-map *collision-system*) (cons x y))))

(declaim
 (inline collidesp)
 (ftype (function (fixnum fixnum &key (:character (or fixnum null))) boolean) collidesp))
(defun collidesp (x y &key (character nil))
  "Returns whether tile located at integer map coordinates X, Y does collide with other objects.
CHARACTER, when non-NIL, specifies character entity to check for collisions with other characters."
  (with-system-slots ((map characters-map) collision-system)
    (let ((point (cons x y)))
      (or
       (not (null (sparse-matrix-ref map point)))
       (if character
           (let ((entity (sparse-matrix-ref characters-map point)))
             (if entity (not (= character (the fixnum entity))) nil))
           nil)))))

(defmethod system-initialize ((system collision-system))
  (with-system-config-options ((debug-collisions))
    (when debug-collisions
      (setf (collision-system-debug-entity system)
            (make-object '((:debug :order 2000d0)))))))

(defmethod system-finalize ((system collision-system))
  (with-system-slots ((debug-entity) collision-system system)
    (when (entity-valid-p debug-entity)
      (delete-entity debug-entity))))

(defmethod system-draw ((system collision-system) renderer)
  (with-system-config-options ((debug-collisions))
    (when debug-collisions
      (with-system-slots ((debug-entity map) collision-system system)
        (sparse-matrix-traverse
         map
         #'(lambda (position value)
             (declare (ignore value))
             (multiple-value-bind (x y)
                 (multiple-value-call #'absolute->viewport
                   (orthogonal->screen
                    (coerce (car position) 'double-float)
                    (coerce (cdr position) 'double-float)))
               (add-debug-tile-rhomb debug-entity x y debug-collisions t))))))))

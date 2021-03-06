(in-package :d2clone-kit)

(defclass map-system (system)
  ((name :initform 'map))
  (:documentation "Handles map chunks in Tiled format.

The following format features are unsupported yet:

* maps made in Tiled < v0.15
* tile flipping
* external tileset files
* non-staggered maps
* stagger axis other than Y
* odd tile size

Also only integer map coordinates allowed for map chunks, otherwise the screen <-> map
conversion maths are badly fucked up."))

(defstruct map-tileset
  (first-id 0 :type fixnum)
  (sprite-batch nil :type fixnum))

(defcomponent map map-chunk
  (tiled-map nil :type tiled-map)
  (tiles nil :type simple-vector)  ;; tiles: tile id -> map-tileset
  (tiles-properties nil :type (vector (or hash-table null)))
  (debug-entity -1 :type fixnum))

(defprefab map "tmx"
  (tiled-map nil :type tiled-map)
  (tiles nil :type simple-vector)  ;; tiles: tile id -> map-tileset
  (tiles-properties nil :type (vector (or hash-table null))))

(declaim (type (integer 0 255) *tile-width* *tile-height*))
(defparameter *tile-width* 0)
(defparameter *tile-height* 0)

(deftype float-coordinate () `(double-float
                               ,(/ most-negative-fixnum 2d0)
                               ,(/ most-positive-fixnum 2d0)))

(declaim
 (inline tile-index)
 (ftype (function (double-float double-float) (values fixnum fixnum)) tile-index))
(defun tile-index (x y)
  "Returns index of tile containing point with world coordinates X, Y."
  (let ((tx (floor (the float-coordinate (+ (* y 0.5d0) x -0.5d0))))
        (ty (floor (the float-coordinate (- (* y 0.5d0) x -1.5d0)))))
    (values
     (1+ (floor (- tx ty) 2))
     (the fixnum (+ tx ty)))))

(declaim
 (inline tile-pos)
 (ftype (function (float-coordinate float-coordinate) (values double-float double-float))
        tile-pos))
(defun tile-pos (col row)
  (let ((x (floor col))
        (y (floor row)))
    (values
     (if (oddp y)
         (+ 0.5d0 x)
         (coerce x 'double-float))
     (coerce y 'double-float))))

(declaim
 (inline map->screen)
 (ftype (function (double-float double-float) (values fixnum fixnum)) map->screen))
(defun map->screen (x y)
  "Converts map coordinates to screen pixel coordinates.

See SCREEN->MAP
See SCREEN->MAP*"
  (values
   (floor (+ (* x *tile-width*) (* (rem (abs (floor y)) 2) (floor *tile-width* 2))))
   (floor (* y *tile-height*) 2)))

(declaim
 (inline screen->map)
 (ftype (function (fixnum fixnum) (values double-float double-float)) screen->map))
(defun screen->map (x y)
  "Converts screen pixel coordinates to map coordinates of nearest tile.

See SCREEN->MAP*
See MAP->SCREEN"
  (let ((tx (floor (- x (* -2 y) (floor *tile-width* 2) (* 2 *tile-height*)) *tile-width*))
        (ty (floor (+ y (/ x -2) (floor *tile-width* 2) (floor *tile-height* 2)) *tile-height*)))
    (values
     (coerce (1+ (/ (- tx ty) 2)) 'double-float)
     (coerce (+ tx ty) 'double-float))))

(declaim
 (inline screen->map*)
 (ftype (function (fixnum fixnum) (values double-float double-float)) screen->map*))
(defun screen->map* (x y)
  "Converts screen pixel coordinates to exact map coordinates.

See SCREEN->MAP
See MAP->SCREEN"
  (multiple-value-bind (int-map-x int-map-y) (screen->map x y)
    (multiple-value-bind (int-x int-y) (map->screen int-map-x int-map-y)
      (let ((diff-x (- x int-x))
            (diff-y (- y int-y)))
        (values
         (+ int-map-x (/ diff-x (coerce *tile-width* 'double-float)))
         (+ int-map-y (/ diff-y (* 2 (coerce *tile-height* 'double-float)))))))))

(defun load-tiles (tiled-map)
  (let ((tilesets (tiled-map-tilesets tiled-map)))
    (if-let (last-tileset
             (loop :for tileset :across tilesets
                   :for first-id := (tiled-tileset-first-id tileset)
                   :with max-id := 0 :and argmax := nil
                   :when (> first-id max-id) :do
                     (setf max-id first-id)
                     (setf argmax tileset)
                   :finally (return argmax)))
      (let* ((tile-count (+ (tiled-tileset-first-id last-tileset)
                            (tiled-tileset-tile-count last-tileset)))
             (tiles (make-array tile-count :initial-element nil))
             (tiles-properties (make-array tile-count :initial-element nil)))
        (loop
          :for tileset :across tilesets
          :do (if-let ((first-id (tiled-tileset-first-id tileset))
                       (tile-count (tiled-tileset-tile-count tileset))
                       (columns (tiled-tileset-columns tileset))
                       (tileset-tiles-properties (tiled-tileset-tiles-properties tileset))
                       (bitmap (al:load-bitmap
                                (format
                                 nil "maps/~a"
                                 (tiled-tileset-image-source tileset)))))
                (let* ((entity (make-entity))
                       (map-tileset (make-map-tileset
                                     :first-id first-id
                                     :sprite-batch entity)))
                  (make-component
                   (system-ref 'sprite-batch)
                   entity
                   :bitmap bitmap
                   :sprite-width *tile-width*
                   :sprite-height *tile-height*)
                  (loop
                    :for i :from first-id :below (+ first-id tile-count)
                    :for index := (- i first-id)
                    :for (q r) := (multiple-value-list (floor index columns))
                    :do (setf
                         (aref tiles i) map-tileset
                         (aref tiles-properties i) (aref tileset-tiles-properties index))))))
        (values tiles tiles-properties))
      nil)))

(defmethod make-prefab ((system map-system) prefab-name)
  (let ((tiled-map (load-tiled-map
                    (make-instance 'character-stream
                                   :path (prefab-path system prefab-name)))))
    (unless (eq (tiled-map-orientation tiled-map) 'staggered)
      (error "only staggered maps supported"))
    (unless (eq (tiled-map-stagger-axis tiled-map) 'y)
      (error "only Y stagger axis supported"))
    (loop :for tileset :across (tiled-map-tilesets tiled-map)
          :for tile-width := (tiled-tileset-tile-width tileset)
          :for tile-height := (tiled-tileset-tile-height tileset)
          :do (if (zerop *tile-width*)
                  (progn
                    (unless (and (evenp tile-width) (evenp tile-height))
                      (error "~s: wrong tileset size ~dx~d (expected to be even)"
                             (tiled-tileset-name tileset)
                             tile-width tile-height))
                    (setf *tile-width* tile-width)
                    (setf *tile-height* tile-height))
                  (unless (and (= *tile-width* tile-width)
                               (= *tile-height* tile-height))
                    (error "~s: wrong tileset size ~dx~d (expected ~dx~d)"
                           (tiled-tileset-name tileset)
                           tile-width tile-height
                           *tile-width* *tile-height*))))
    (multiple-value-bind (tiles tiles-properties)
        (load-tiles tiled-map)
      (make-map-prefab
       :tiled-map tiled-map
       :tiles tiles
       :tiles-properties tiles-properties))))

(defmethod make-prefab-component ((system map-system) entity prefab parameters)
  (with-system-config-options ((debug-grid))
    (with-map-chunk entity ()
      (setf tiled-map (map-prefab-tiled-map prefab))
      (setf tiles (map-prefab-tiles prefab))
      (setf tiles-properties (map-prefab-tiles-properties prefab))
      (when debug-grid
        (setf debug-entity (make-entity))
        (make-component (system-ref 'debug) debug-entity)))))

(defmethod make-component ((system map-system) entity &rest parameters)
  (declare (ignore system entity parameters))
  nil)

(declaim (inline ground-layer-p))
(defun ground-layer-p (layer)
  "Returns T if map chunk layer LAYER is ground layer (i.e. has \"ground\" property)."
  (if-let (properties (tiled-layer-properties layer))
    (gethash 'ground properties nil)
    nil))

(declaim
 (inline tile-property)
 (ftype (function ((vector (or hash-table null)) array-index symbol &optional t)) tile-property))
(defun tile-property (tile-properties index property &optional (default nil))
  "Returns property denoted by symbol PROPERTY of tile with index INDEX. Returns DEFAULT if there's no such property."
  (if-let (properties (aref tile-properties index))
    (gethash property properties default)
    default))

(defmethod system-update ((system map-system) dt)
  (flet
      ((intp (n) (zerop (mod n 1))))
    (with-map-chunks
      (with-coordinate entity ()
        (unless (and (intp x)
                     (intp y))
          ;; TODO : restart for rounding coordinates
          (error "Only integer map coordinates allowed for map chunks"))))))

(defmethod system-draw ((system map-system) renderer)
  (with-system-config-options ((display-width display-height debug-grid))
    (multiple-value-bind (start-x start-y)
        (multiple-value-call #'screen->map (viewport->absolute 0 0))
      (multiple-value-bind (end-x end-y)
          (multiple-value-call #'screen->map (viewport->absolute display-width display-height))
        (with-map-chunks
            (with-coordinate entity (chunk-x chunk-y)
              (multiple-value-bind (chunk-viewport-x chunk-viewport-y)
                  (multiple-value-call #'absolute->viewport
                    (map->screen chunk-x chunk-y))
                (when (range-visible-p
                       chunk-viewport-x chunk-viewport-y
                       (+ (* (tiled-map-width tiled-map) *tile-width*) (truncate *tile-width* 2))
                       (* (1+ (tiled-map-height tiled-map)) (truncate *tile-height* 2)))
                  (loop
                    :with layer-count := (length (tiled-map-layers tiled-map))
                    :with from-col := (max 0 (ceiling (- start-x chunk-x 2)))
                    :with from-row := (max 0 (ceiling (- start-y chunk-y 2)))
                    :for layer :across (tiled-map-layers tiled-map)
                    :for to-col := (min (ceiling (- end-x chunk-x -1))
                                       (1- (tiled-layer-width layer)))
                    :for to-row := (min (ceiling (- end-y chunk-y -1))
                                       (1- (tiled-layer-height layer)))
                    :do (loop
                         :with layer-order := (tiled-layer-order layer)
                         :with ground-layer-p := (ground-layer-p layer)
                         :with data := (tiled-layer-data layer)
                         :for row :from from-row :upto to-row
                         :do (loop :for col :from from-col :upto to-col
                                   :do (let ((tile-index (aref data row col)))
                                         (unless (zerop tile-index)
                                           (multiple-value-bind (tile-x tile-y)
                                               (map->screen
                                                (coerce col 'double-float)
                                                (coerce row 'double-float))
                                             ;; TODO : translucent if obscures player!
                                             (when-let (tileset (aref tiles tile-index))
                                               (add-sprite-index-to-batch
                                                (map-tileset-sprite-batch tileset)
                                                (coerce
                                                 (+ tile-y chunk-viewport-y
                                                    (tile-property tiles-properties tile-index 'z 0)
                                                    (* display-width
                                                       (- layer-order
                                                          (* layer-count
                                                             (if ground-layer-p 2 1)))))
                                                 'double-float)
                                                (- tile-index (map-tileset-first-id tileset))
                                                (+ tile-x chunk-viewport-x)
                                                (+ tile-y chunk-viewport-y))))))))
                    :finally
                       (when debug-grid
                         (loop :for row :from from-row :upto to-row
                               :do (loop :for col :from from-col :upto to-col
                                         :do (multiple-value-bind (tile-x tile-y)
                                                 (map->screen
                                                  (coerce col 'double-float)
                                                  (coerce row 'double-float))
                                               (add-debug-tile-rhomb
                                                debug-entity
                                                (+ tile-x chunk-viewport-x)
                                                (+ tile-y chunk-viewport-y)
                                                debug-grid nil))))))))))))))

(defhandler map-system quit (event)
  (setf *tile-width* 0)
  (setf *tile-height* 0))

(in-package :d2clone-kit)


(defsystem map
  ((last-camera-x most-positive-double-float :type double-float)
   (last-camera-y most-positive-double-float :type double-float))
  (:documentation "Handles map chunks in Tiled format.

The following format features are unsupported yet:

* maps made in Tiled < v0.15
* tile flipping
* external tileset files
* non-staggered maps
* stagger axis other than Y
* odd tile size

Also only integer map coordinates allowed for map chunks, otherwise the screen
<-> map conversion maths are badly fucked up."))

(defstruct map-tileset
  (first-id 0 :type fixnum)
  (sprite-batch nil :type fixnum))

(defcomponent (map map-chunk)
  (tiled-map nil :type tiled-map)
  (tiles nil :type simple-vector :documentation "tile id -> map-tileset")
  (tiles-properties nil :type (vector (or hash-table null)))
  (sprite-batches nil :type list)
  (debug-entity +invalid-entity+ :type fixnum))

(defprefab map "tmx"
  (tiled-map nil :type tiled-map)
  (tiles nil :type simple-vector)  ;; tiles: tile id -> map-tileset
  (tiles-properties nil :type (vector (or hash-table null)))
  (sprite-batches nil :type list))

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
             (tiles-properties (make-array tile-count :initial-element nil))
             (sprite-batches nil))
        (loop
          :for tileset :across tilesets
          :do (if-let ((first-id (tiled-tileset-first-id tileset))
                       (tile-count (tiled-tileset-tile-count tileset))
                       (columns (tiled-tileset-columns tileset))
                       (tileset-tiles-properties (tiled-tileset-tiles-properties
                                                  tileset))
                       (bitmap (ensure-loaded
                                #'al:load-bitmap
                                (format
                                 nil "maps/~a"
                                 (tiled-tileset-image-source tileset)))))
                (let* ((entity (make-object
                                `((:sprite-batch
                                   :bitmap ,bitmap
                                   :sprite-width ,*tile-width*
                                   :sprite-height ,*tile-height*))))
                       (map-tileset (make-map-tileset
                                     :first-id first-id
                                     :sprite-batch entity)))
                  (push entity sprite-batches)
                  (loop
                    :for i :from first-id :below (+ first-id tile-count)
                    :for index := (- i first-id)
                    :for (q r) := (multiple-value-list (floor index columns))
                    :do (setf (aref tiles i)
                              map-tileset
                              (aref tiles-properties i)
                              (aref tileset-tiles-properties index))))))
        (values tiles tiles-properties sprite-batches))
      nil)))

(defmethod make-prefab ((system map-system) prefab-name)
  (let ((tiled-map (load-tiled-map
                    (make-instance 'character-stream
                                   :path (prefab-path system prefab-name)))))
    (unless (eq (tiled-map-orientation tiled-map) :staggered)
      (error "only staggered maps supported"))
    (unless (eq (tiled-map-stagger-axis tiled-map) :y)
      (error "only Y stagger axis supported"))
    (loop :for tileset :across (tiled-map-tilesets tiled-map)
          :for tile-width := (tiled-tileset-tile-width tileset)
          :for tile-height := (tiled-tileset-tile-height tileset)
          :do (if (zerop *tile-width*)
                  (progn
                    (unless (and (evenp tile-width) (evenp tile-height))
                      (error
                       "~s: wrong tileset size ~dx~d (expected to be even)"
                       (tiled-tileset-name tileset)
                       tile-width tile-height))
                    (unless (= tile-width (* 2 tile-height))
                      (error "~s: wrong tileset size ~dx~d 
(expected aspect ratio 2:1)"
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
    (multiple-value-bind (tiles tiles-properties sprite-batches)
        (load-tiles tiled-map)
      (make-map-prefab
       :tiled-map tiled-map
       :tiles tiles
       :tiles-properties tiles-properties
       :sprite-batches sprite-batches))))

(defmethod make-prefab-component ((system map-system) entity prefab parameters)
  (with-system-config-options ((debug-grid))
    (make-map-chunk entity
                    :tiled-map (map-prefab-tiled-map prefab)
                    :tiles (map-prefab-tiles prefab)
                    :tiles-properties (map-prefab-tiles-properties prefab)
                    :sprite-batches (map-prefab-sprite-batches prefab)
                    :debug-entity (if debug-grid
                                      (make-object '((:debug)) entity)
                                      +invalid-entity+))))

(declaim (inline ground-layer-p))
(defun ground-layer-p (layer)
  "Returns T if map chunk layer LAYER is ground layer (i.e. has \"ground\"
  property)."
  (if-let (properties (tiled-layer-properties layer))
    (gethash :ground properties nil)
    nil))

(declaim
 (inline tile-property)
 (ftype
  (function ((vector (or hash-table null)) array-index symbol &optional t))
  tile-property))
(defun tile-property (tile-properties index property &optional (default nil))
  "Returns property denoted by symbol PROPERTY of tile with index
INDEX. Returns DEFAULT if there's no such property."
  ;; TODO : replace with keyword?..
  (if-let (properties (aref tile-properties index))
    (gethash property properties default)
    default))

(defmethod system-finalize ((system map-system))
  (setf *tile-width* 0)
  (setf *tile-height* 0))

(defmethod system-update ((system map-system))
  (flet
      ((intp (n) (zerop (mod n 1))))
    (with-map-chunks
      (with-coordinate entity ()
        (unless (and (intp x)
                     (intp y))
          ;; TODO : restart for rounding coordinates
          (error
           "Only integer map coordinates allowed for map chunks, got (~s, ~s)"
           x y))))))

;; TODO : test on kenney assets (different size)!

(defmethod system-draw ((system map-system) renderer)
  (with-system-config-options ((display-width display-height debug-grid))
    (with-system-slots ((last-camera-x last-camera-y) map-system system
                        :read-only nil)
      (with-camera (camera-x camera-y)
        (when (or debug-grid
                  (not (= camera-x last-camera-x))
                  (not (= camera-y last-camera-y)))
          (setf last-camera-x camera-x
                last-camera-y camera-y)
          (mvlet ((start-x start-y (multiple-value-call #'screen->isometric*
                                     (viewport->absolute 0 0)))
                  (end-x end-y (multiple-value-call #'screen->isometric*
                                 (viewport->absolute
                                  display-width display-height))))
            (setf start-x (/ start-x 2)
                  end-x (/ end-x 2)
                  start-y (* start-y 2)
                  end-y (* end-y 2))
            (with-map-chunks
                (dolist (sprite-batch sprite-batches)
                  (clear-sprite-batch sprite-batch))
              (with-coordinate entity (chunk-ortho-x chunk-ortho-y)
                (mvlet* ((chunk-x chunk-y (orthogonal->isometric
                                           chunk-ortho-x chunk-ortho-y))
                         (chunk-viewport-x chunk-viewport-y
                                           (multiple-value-call
                                               #'absolute->viewport
                                             (isometric->screen
                                              chunk-x chunk-y))))
                  (when (range-visible-p
                         chunk-viewport-x chunk-viewport-y
                         (+ (* (tiled-map-width tiled-map) *tile-width*)
                            (truncate *tile-width* 2))
                         (* (1+ (tiled-map-height tiled-map))
                            (truncate *tile-height* 2)))
                    (loop
                      :with layer-count := (length (tiled-map-layers tiled-map))
                      :with from-col := (max 0 (ceiling (- start-x chunk-x 2)))
                      :with from-row := (max 0 (ceiling (- start-y chunk-y 2)))
                      :for layer :across (tiled-map-layers tiled-map)
                      :for to-col := (min (ceiling (- end-x chunk-x))
                                          (1- (tiled-layer-width layer)))
                      :for to-row := (min (ceiling (- end-y chunk-y))
                                          (1- (tiled-layer-height layer)))
                      :do (loop
                            :with layer-order := (tiled-layer-order layer)
                            :with ground-layer-p := (ground-layer-p layer)
                            :with data := (tiled-layer-data layer)
                            :for row :from from-row :upto to-row
                            :do (loop :for col :from from-col :upto to-col
                                      :do (let ((tile-index
                                                  (aref data row col)))
                                            (unless (zerop tile-index)
                                              (mvlet
                                                  ((tile-x tile-y
                                                           (isometric->screen*
                                                            (coerce
                                                             col
                                                             'double-float)
                                                            (coerce
                                                             row
                                                             'double-float))))
                                                ;; TODO : translucent if
                                                ;; obscures player!
                                                (when-let
                                                    (tileset
                                                     (aref tiles tile-index))
                                                  (add-sprite-index-to-batch
                                                   (map-tileset-sprite-batch
                                                    tileset)
                                                   (coerce
                                                    (+ (/ tile-y 2)
                                                       chunk-viewport-y
                                                       (tile-property
                                                        tiles-properties
                                                        tile-index :z 0)
                                                       (*
                                                        display-width
                                                        (- layer-order
                                                           (* layer-count
                                                              (if ground-layer-p
                                                                  2 1)))))
                                                    'double-float)
                                                   (- tile-index
                                                      (map-tileset-first-id
                                                       tileset))
                                                   (+ (floor tile-x 0.5d0)
                                                      chunk-viewport-x)
                                                   (+ (floor tile-y 2d0)
                                                      chunk-viewport-y))))))))
                      :finally
                         (when debug-grid
                           (loop :for row :from from-row :upto to-row
                                 :do (loop :for col :from from-col :upto to-col
                                           :do (mvlet
                                                   ((tile-x tile-y
                                                            (isometric->screen*
                                                             (coerce
                                                              col
                                                              'double-float)
                                                             (coerce
                                                              row
                                                              'double-float))))
                                                 (add-debug-tile-rhomb
                                                  debug-entity
                                                  (+ (floor tile-x 0.5d0)
                                                     chunk-viewport-x)
                                                  (+ (floor tile-y 2d0)
                                                     chunk-viewport-y)
                                                  debug-grid nil))))))))))))))))

(in-package :d2clone-kit)

(defclass map-system (system)
  ((name :initform 'map)))

(defcomponent map map-chunk
  (tiled-map nil :type tiled-map)
  (tiles nil :type simple-vector)
  (debug-entity -1 :type fixnum))

(defprefab map "tmx"
  (tiled-map nil :type tiled-map)
  (tiles nil :type simple-vector))

(declaim (type (integer 0 255) *tile-width* *tile-height*))
(defparameter *tile-width* 0)
(defparameter *tile-height* 0)

(declaim
 (inline map->screen)
 (ftype (function (double-float double-float) (values fixnum fixnum)) map->screen))
(defun map->screen (x y)
  (values
   (floor (+ (* x *tile-width*) (* (rem (abs (floor y)) 2) (floor *tile-width* 2))))
   (floor (* y *tile-height*) 2)))

(declaim
 (inline screen->map)
 (ftype (function (fixnum fixnum) (values double-float double-float)) screen->map*))
(defun screen->map (x y)
  (let ((tx (floor (- x (* -2 y) (floor *tile-width* 2) (* 2 *tile-height*)) *tile-width*))
        (ty (floor (+ y (/ x -2) (floor *tile-width* 2) (floor *tile-height* 2)) *tile-height*)))
    (values
     (coerce (1+ (/ (- tx ty) 2)) 'double-float)
     (coerce (+ tx ty) 'double-float))))

(declaim
 (inline screen->map*)
 (ftype (function (fixnum fixnum) (values double-float double-float)) screen->map))
(defun screen->map* (x y)
  (multiple-value-bind (int-map-x int-map-y) (screen->map* x y)
    (multiple-value-bind (int-x int-y) (map->screen int-map-x int-map-y)
      (let ((diff-x (- x int-x))
            (diff-y (- y int-y)))
        (values
         (+ int-map-x (/ diff-x (coerce *tile-width* 'double-float)))
         (+ int-map-y (/ diff-y (coerce *tile-height* 'double-float))))))))

(defun load-tiles (tiled-map)
  (let ((tilesets (tiled-map-tilesets tiled-map)))
    (if-let (last-tileset
             (loop for tileset across tilesets
                   for first-id = (tiled-tileset-first-id tileset)
                   with max-id = 0 and argmax = nil
                   when (> first-id max-id) do
                     (setf max-id first-id)
                     (setf argmax tileset)
                   finally (return argmax)))
      (let ((tiles (make-array
                    (+ (tiled-tileset-first-id last-tileset)
                       (tiled-tileset-tile-count last-tileset))
                    :initial-element nil)))
        (loop
          for tileset across tilesets
          do
             (if-let ((first-id (tiled-tileset-first-id tileset))
                      (tile-count (tiled-tileset-tile-count tileset))
                      (columns (tiled-tileset-columns tileset))
                      (bitmap (al:load-bitmap
                               (format
                                nil "maps/~a"
                                (tiled-tileset-image-source tileset)))))
               (loop
                 for i from first-id below (+ first-id tile-count)
                 for index = (- i first-id)
                 for (q r) = (multiple-value-list (floor index columns))
                 do
                    (setf (aref tiles i)
                          (al:create-sub-bitmap
                           bitmap
                           (* r *tile-width*) (* q *tile-height*)
                           *tile-width* *tile-height*)))))
        tiles)
      nil)))

(defmethod make-prefab ((system map-system) prefab-name)
  (let ((tiled-map (load-tiled-map
                    (make-instance 'character-stream
                                   :path (prefab-path system prefab-name)))))
    (unless (eq (tiled-map-orientation tiled-map) 'staggered)
      (error "only staggered maps supported"))
    (unless (eq (tiled-map-stagger-axis tiled-map) 'y)
      (error "only Y stagger axis supported"))
    (loop for tileset across (tiled-map-tilesets tiled-map)
          for tile-width = (tiled-tileset-tile-width tileset)
          for tile-height = (tiled-tileset-tile-height tileset)
          do
             (if (zerop *tile-width*)
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
    (make-map-prefab
     :tiled-map tiled-map
     :tiles (load-tiles tiled-map))))

(defmethod make-prefab-component ((system map-system) entity prefab)
  (with-system-config-options ((debug-grid))
    (with-map-chunk entity ()
      (setf tiled-map (map-prefab-tiled-map prefab))
      (setf tiles (map-prefab-tiles prefab))
      (when debug-grid
        (setf debug-entity (make-entity))
        (make-component (system-ref 'debug) debug-entity)))))

(defmethod make-component ((system map-system) entity &rest parameters)
  (declare (ignore system entity parameters))
  nil)

(defmethod system-load ((system map-system))
  t)

(defun ground-layer-p (layer)
  (if-let ((properties (tiled-layer-properties layer)))
    (gethash 'ground? properties nil)
    nil))

;; XXX only allow integer map chunk coords?..

;; NOTE : it is not advisable performance-wise to use more than one tileset in each layer
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
                    with from-col = (max 0 (ceiling (- start-x chunk-x 2)))
                    with from-row = (max 0 (ceiling (- start-y chunk-y 2)))
                    for layer across (tiled-map-layers tiled-map)
                    for to-col = (min (ceiling (- end-x chunk-x)) (1- (tiled-layer-width layer)))
                    for to-row = (min (ceiling (- end-y chunk-y)) (1- (tiled-layer-height layer)))
                    do (render
                        renderer
                        (+ (tiled-layer-order layer) (if (ground-layer-p layer) 0 100))
                        (let ((layer layer)
                              (tiles tiles))
                          #'(lambda ()
                              (loop
                                with data = (tiled-layer-data layer)
                                for row from from-row upto to-row
                                do (loop for col from from-col upto to-col
                                         do (let ((tile-index (aref data row col)))
                                              (unless (zerop tile-index)
                                                (multiple-value-bind (tile-x tile-y)
                                                    (map->screen
                                                     (coerce col 'double-float)
                                                     (coerce row 'double-float))
                                                  ;; TODO : translucent if obscures player!
                                                  (al:draw-bitmap (aref tiles tile-index)
                                                                  (+ tile-x chunk-viewport-x)
                                                                  (+ tile-y chunk-viewport-y)
                                                                  0)))))))))
                    finally
                       (when debug-grid
                         (loop for row from from-row upto to-row
                               do (loop for col from from-col upto to-col
                                        do (multiple-value-bind (tile-x tile-y)
                                               (map->screen
                                                (coerce col 'double-float)
                                                (coerce row 'double-float))
                                             (add-debug-tile-rhomb
                                              debug-entity
                                              (+ tile-x chunk-viewport-x)
                                              (+ tile-y chunk-viewport-y)
                                              debug-grid nil))))))))))))))

(defmethod system-quit ((system map-system))
  (setf *tile-width* 0)
  (setf *tile-height* 0))

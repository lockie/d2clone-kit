(in-package :d2clone-kit)

(defclass map-system (system)
  ((name :initform 'map)))

(defcomponent map map-chunk
  (tiled-map nil :type tiled-map)
  (tiles nil :type simple-vector))

(defprefab map "tmx"
  (tiled-map nil :type tiled-map)
  (tiles nil :type simple-vector))

(declaim (type (integer 0 255) *tile-width* *tile-height*))
(defparameter *tile-width* 0)
(defparameter *tile-height* 0)

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
  (with-map-chunk entity ()
    (setf tiled-map (map-prefab-tiled-map prefab))
    (setf tiles (map-prefab-tiles prefab))))

(defmethod make-component ((system map-system) entity &rest parameters)
  (declare (ignore system entity parameters))
  nil)

(defmethod system-load ((system map-system))
  t)

(defun ground-layer-p (layer)
  (if-let ((properties (tiled-layer-properties layer)))
    (gethash 'ground? properties nil)
    nil))

(defun map->screen (point-x point-y)
  (declare (type coordinate point-x point-y))
  (labels
      ((half-round (n)
         (ceiling n 2)))
    (declare (inline half-round))
    (values
     (+ (* point-x *tile-width*)
        (half-round (* (rem (abs point-y) 2) *tile-width*)))
     (half-round (* point-y *tile-height*)))))

(defun screen->map (point-x point-y)
  (declare (type fixnum point-x point-y))
  (let* ((row (the fixnum (round point-y (/ *tile-height* 2))))
         (col (the fixnum (round (- (/ (float point-x) *tile-width*)
                                    (/ (rem row 2) 2.0))))))
    (values col row)))

;; TODO : debug setting + drawing of tile under cursor

(declaim
 (inline add-tile-rhomb)
 (ftype (function ((vector single-float) fixnum fixnum list)) add-tile-rhomb))
(defun add-tile-rhomb (buffer x y color)
  (flet
      ((add-point (x y)
         (vector-push-extend (float x 0f0) buffer)
         (vector-push-extend (float y 0f0) buffer)
         (vector-push-extend 0f0 buffer) ;; z
         (vector-push-extend 0f0 buffer) ;; u
         (vector-push-extend 0f0 buffer) ;; v
         (vector-push-extend (float (first color) 0f0) buffer)             ;; r
         (vector-push-extend (float (second color) 0f0) buffer)            ;; g
         (vector-push-extend (float (third color) 0f0) buffer)             ;; b
         (vector-push-extend (float (or (fourth color) 0f0) 0f0) buffer))) ;; a
    (add-point (+ x (ceiling *tile-width* 2)) y)
    (add-point (+ x *tile-width*) (+ y (ceiling *tile-height* 2)))
    (add-point (+ x *tile-width*) (+ y (ceiling *tile-height* 2)))
    (add-point (+ x (ceiling *tile-width* 2)) (+ y *tile-height*))
    (add-point (+ x (ceiling *tile-width* 2)) (+ y *tile-height*))
    (add-point x (+ y (ceiling *tile-height* 2)))
    (add-point x (+ y (ceiling *tile-height* 2)))
    (add-point (+ x (ceiling *tile-width* 2)) y)))

;; NOTE : it is not advisable performance-wise to use  more than one tileset in each layer
(defmethod system-draw ((system map-system) renderer)
  (macrolet
      ((with-layer-tiles (&rest body)
         `(loop for row from (max (1- from-row) 0)
                upto (min (+ 2 from-row (* screen-height/tiles 2)) (1- (tiled-layer-height layer)))
                do (loop for col from (max (1- from-col) 0)
                         upto (min (+ 1 from-col screen-width/tiles)
                                   (1- (tiled-layer-width layer)))
                         do ,@body))))
    (with-system-config-options ((display-width display-height debug-grid))
      (let ((screen-width/tiles (ceiling display-width *tile-width*))
            (screen-height/tiles (ceiling display-height *tile-height*)))
        (with-map-chunks
            (with-point entity (chunk-x chunk-y) ;; XXX require it to be divisible by tile size?
              (when (range-visible-p chunk-x chunk-y
                                     (* (tiled-map-width tiled-map) *tile-width*)
                                     (* (tiled-map-height tiled-map) *tile-height*))
                (multiple-value-bind (chunk-screen-x chunk-screen-y)
                    (absolute->screen chunk-x chunk-y)
                  (multiple-value-bind (from-col from-row)
                      (screen->map (- chunk-screen-x) (- chunk-screen-y))
                    (incf from-row (rem (abs from-row) 2))
                    (let ((tile-offset-x (rem chunk-x *tile-width*))
                          (tile-offset-y (rem chunk-y *tile-height*)))
                      (loop for layer across (tiled-map-layers tiled-map)
                            do (render
                                renderer
                                (+ (tiled-layer-order layer) (if (ground-layer-p layer) 0 100))
                                (let ((layer layer) (tiles tiles))
                                  #'(lambda ()
                                      (let ((data (tiled-layer-data layer)))
                                        (with-layer-tiles
                                            (let ((tile-index (aref data row col)))
                                              (unless (zerop tile-index)
                                                (multiple-value-bind (tile-x tile-y)
                                                    (map->screen (- col from-col) (- row from-row))
                                                  ;; TODO : translucent if obscures player!
                                                  (al:draw-bitmap (aref tiles tile-index)
                                                                  (+ tile-x tile-offset-x)
                                                                  (+ tile-y tile-offset-y) 0))))))))))
                      (when debug-grid
                        (render
                         renderer 1000
                         (let ((layer (aref (tiled-map-layers tiled-map) 0)))
                           #'(lambda ()
                               (let ((vertices (make-array 144 :adjustable t :fill-pointer 0
                                                               :element-type 'single-float)))
                                 (with-layer-tiles
                                     (multiple-value-bind (tile-x tile-y)
                                         (map->screen (- col from-col) (- row from-row))
                                       (add-tile-rhomb vertices
                                                       (+ tile-x tile-offset-x)
                                                       (+ tile-y tile-offset-y) debug-grid)))
                                 (let ((buffer (make-array (length vertices)
                                                           :element-type 'single-float
                                                           :initial-contents vertices)))
                                   (cffi:with-pointer-to-vector-data (ptr buffer)
                                     (al:draw-prim ptr (cffi:null-pointer) (cffi:null-pointer) 0
                                                   (ceiling (length buffer) 9) 0))))))))))))))))))




(defmethod system-quit ((system map-system))
  (setf *tile-width* 0)
  (setf *tile-height* 0))

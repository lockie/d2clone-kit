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

;; (defun map->screen (point-x point-y)
;;   ;; (declare (type coordinate point-x point-y))
;;   (labels
;;       ((half-round (n)
;;          (ceiling n 2)))
;;     (declare (inline half-round))
;;     (values
;;      (+ (* point-x *tile-width*)
;;         (half-round (* (rem (abs point-y) 2) *tile-width*)))
;;      (half-round (* point-y *tile-height*)))))

;; (defun screen->map (point-x point-y)
;;   (declare (type fixnum point-x point-y))
;;   (let* ((row (the fixnum (round point-y (/ *tile-height* 2))))
;;          (col (the fixnum (round (- (/ (float point-x) *tile-width*)
;;                                     (/ (rem row 2) 2.0))))))
;;     (values col row)))

(declaim
 (inline add-tile-rhomb)
 (ftype (function ((vector single-float) fixnum fixnum list boolean)) add-tile-rhomb))
(defun add-tile-rhomb (buffer x y color mark)
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
    (add-point (+ x (ceiling *tile-width* 2)) y)
    (when mark
      (add-point (+ x (ceiling *tile-width* 2)) y)
      (add-point (+ x (ceiling *tile-width* 2)) (+ y *tile-height*))
      (add-point (+ x *tile-width*) (+ y (ceiling *tile-height* 2)))
      (add-point x (+ y (ceiling *tile-height* 2))))))

(defun mouse-position ()
  ;; TODO : optional struct arg; etypecase for it (mouse-state vs mouse-event)
  (al:with-current-mouse-state state
    (cffi:with-foreign-slots
        ((al::x al::y) state (:struct al:mouse-state))
      (values al::x al::y))))

;; NOTE : it is not advisable performance-wise to use more than one tileset in each layer
;; TODO : вроде всё работает, теперь нужен debug grid
(defmethod system-draw ((system map-system) renderer)
  (with-system-config-options ((display-width display-height debug-grid))
    (with-screen-coordinate (slot-value (system-ref 'camera) 'entity)
        (camera-x camera-y)
    (multiple-value-bind (start-x start-y)
        ;; XXX этот паттерн часто повторяется
        (multiple-value-call #'screen->map (viewport->absolute 0 0))
        (multiple-value-bind (end-x end-y)
            (multiple-value-call #'screen->map (+ (ceiling display-width 2) camera-x) (+ (ceiling display-height 2) camera-y))

          ;; (log-info "cam = ~a, ~a ; start = ~a, ~a; end = ~a, ~a" camera-x camera-y start-x start-y end-x end-y)
        (with-map-chunks
            ;; XXX require it to be divisible by tile size?
            (with-coordinate entity (chunk-x chunk-y)
              (multiple-value-bind (chunk-screen-x chunk-screen-y)
                  (multiple-value-call #'absolute->viewport (map->screen chunk-x chunk-y))
                (let ((from-col (max 0 (ceiling (+ start-x (- chunk-x) -2))))
                      (from-row (max 0 (ceiling (+ start-y (- chunk-y) -2))))
                      (tile-offset-x chunk-screen-x);;(rem chunk-screen-x *tile-width*))
                      (tile-offset-y chunk-screen-y);;(rem chunk-screen-y *tile-height*))
                      )
                  (log-info "~a: ~a, ~a - ~a, ~a"
                            entity from-col from-row
                            (min (1- (tiled-layer-width (elt (tiled-map-layers tiled-map) 0))) (ceiling (+ end-x (- chunk-x) 1)))
                            (min (1- (tiled-layer-height (elt (tiled-map-layers tiled-map) 0))) (ceiling (+ end-y (- chunk-y) 1))))
                  (loop for layer across (tiled-map-layers tiled-map)
                        for to-col = (min (1- (tiled-layer-width layer)) (ceiling (+ end-x (- chunk-x) 1)))
                        for to-row = (min (1- (tiled-layer-height layer)) (ceiling (+ end-y (- chunk-y) 1)))
                        do (render
                            renderer
                            (+ (tiled-layer-order layer) (if (ground-layer-p layer) 0 100))
                            (let ((layer layer)
                                  (tiles tiles)
                                  (chunk-x chunk-x) (chunk-y chunk-y)
                                  )
                              #'(lambda ()
                                  (let ((data (tiled-layer-data layer)))
                                    (loop for row from from-row upto to-row
                                          do (loop for col from from-col upto to-col
                                                   do (let ((tile-index (aref data row col)))
                                                        (unless (zerop tile-index)
                                                          (multiple-value-bind (tile-x tile-y)
                                                                (map->screen (coerce col 'double-float) (coerce row 'double-float))
                                                            ;; TODO : translucent if obscures player!
                                                            ;; (when (and (zerop row) (zerop col) (string= (tiled-layer-name layer) "water"))
                                                            ;;   (log-info "first tile pos ~a, ~a + offs= ~a, ~a~%end ~a, ~a + chunk-pos = ~a, ~a" tile-x tile-y (+ tile-x tile-offset-x) (+ tile-y tile-offset-y) end-x end-y (+ end-x chunk-x) (+ end-y chunk-y)))
                                                              (al:draw-bitmap (aref tiles tile-index)
                                                                              (+ tile-x tile-offset-x)
                                                                              (+ tile-y tile-offset-y) 0))))))))))))))))))))

(defmethod system-quit ((system map-system))
  (setf *tile-width* 0)
  (setf *tile-height* 0))

(in-package :d2clone-kit)


(defclass sprite-system (system)
  ((name :initform 'sprite)))

(deftype angle () `(double-float ,(- (* 2 pi)) ,(* 2 pi)))

(defcomponent sprite sprite
  (width 0 :type fixnum)
  (height 0 :type fixnum)
  (layers nil :type hash-table)  ;; layer name (symbol) -> al_bitmap
  (stances nil :type hash-table)  ;; stance name (symbol) -> list of frame #s
  (frame-durations nil :type (simple-array double-float))
  ;; instance state
  (stance nil :type symbol)
  (frame 0 :type fixnum)
  (angle 0d0 :type angle)
  (time-counter 0d0 :type double-float)
  (layers-toggled nil :type hash-table))  ;; layer name (symbol) -> boolean

(defprefab sprite "ase"
  (ase-file nil :type ase-file)
  (layers nil :type hash-table)  ;; layer name (symbol) -> al_bitmap
  (stances nil :type hash-table)  ;; stance name (symbol) -> list of frame #s
  (frame-durations nil :type (simple-array double-float)))

(defun toggle-layer (entity layer &optional (on nil on-supplied-p))
  (with-sprite entity ()
    (multiple-value-bind (old-value layer-exists)
        (gethash layer layers-toggled)
      (unless layer-exists
        ;; TODO : рестарт с выбором существуюещего layer
        (error "no such layer: ~a" layer))
      (let ((value (if on-supplied-p on (not old-value))))
        (setf (gethash layer layers-toggled) value)))))

(cffi:defcfun memcpy :pointer
  (dst :pointer)
  (src :pointer)
  (size :int))

(cffi:defcstruct locked-bitmap-region
  (data :pointer)
  (format :int)
  (pitch :int)
  (pixel-size :int))

(declaim
 (inline seconds)
 (ftype (function (fixnum) double-float) seconds))
(defun seconds (milliseconds)
  (* milliseconds 0.001d0))

(defun load-sprite-frame-durations (ase-file stances)
  (let ((total-stance-length (loop for stance-name being the hash-key of stances
                                   sum (length (gethash stance-name stances)))))
    (delete-if
     #'identity
     (map '(vector double-float)
          #'(lambda (frame)
              (seconds (ase-frame-duration frame)))
          (ase-file-frames ase-file))
     :start total-stance-length)))

(defun load-sprite-stances (ase-file)
  (loop
    with stances = (make-hash :size 4 :test 'eq)
    for frame across (ase-file-frames ase-file)
    do (loop
         for chunk across (ase-frame-chunks frame)
         when (and chunk (ase-tags-chunk-p chunk))
           do (loop
                for tag across (ase-tags-chunk-tags chunk)
                for tag-name = (ase-tag-name tag)
                for stance-name = (format-symbol
                                   'd2clone-kit "~{~:@(~a~)~^-~}"
                                   (butlast
                                    (uiop:split-string tag-name :separator '(#\-))))
                do (if (uiop:string-suffix-p tag-name "-0")
                       (setf (gethash stance-name stances)
                             (loop for i from (ase-tag-from tag) upto (ase-tag-to tag) collect i))
                       (when-let (stance-frames (gethash stance-name stances))
                         (unless (= (1- (length stance-frames))
                                    (- (ase-tag-to tag) (ase-tag-from tag)))
                           (error "length mismatch for tag ~a" tag-name))))))
    finally (return stances)))

(defun load-sprite-layer-names (ase-file)
  (loop
    with layer-names = (make-array 1 :element-type 'symbol :initial-element nil)
    for frame across (ase-file-frames ase-file)
    do (loop
         for chunk across (ase-frame-chunks frame)
         when (and chunk (ase-layer-chunk-p chunk))
           do (let ((id (ase-layer-chunk-id chunk))
                    (layer-name (ase-layer-chunk-name chunk))
                    (layers-length (length layer-names)))
                (when (>= id layers-length)
                  (setf layer-names
                        (adjust-array layer-names (round (* layers-length +array-growth-factor+)))))
                (setf (elt layer-names id) (format-symbol 'd2clone-kit "~:@(~a~)" layer-name))))
    finally (return layer-names)))

(defun load-sprite-layers (ase-file layer-names stances)
  (loop
    with cel-width = (ase-file-width ase-file)
    with cel-height = (ase-file-height ase-file)
    with frames = (ase-file-frames ase-file)
    with frames-count = (length frames)
    with total-stance-length = (loop for stance-name being the hash-key of stances
                                     sum (length (gethash stance-name stances)))
    with directions = (ceiling frames-count total-stance-length)
    with bitmap-width = (* cel-width total-stance-length)
    with bitmap-height = (* cel-height directions)
    with layers-count = (length layer-names)
    with layer-bitmaps = (make-array layers-count :element-type 'cffi:foreign-pointer
                                                  :initial-element (cffi:null-pointer))
    with locked-bitmaps = (loop
                            for i below layers-count
                            for bitmap = (al:create-bitmap bitmap-width bitmap-height)
                            do (setf (elt layer-bitmaps i) bitmap)
                            collect (al:lock-bitmap bitmap :abgr-8888 :writeonly))
    for frame across frames
    for frame-id below frames-count
    do (loop
         for chunk across (ase-frame-chunks frame)
         when (and chunk (ase-cel-chunk-p chunk))
           do (cffi:with-foreign-slots ((data pitch pixel-size)
                                        (elt locked-bitmaps (ase-cel-chunk-layer-id chunk))
                                        (:struct locked-bitmap-region))
                (multiple-value-bind (row col)
                    (truncate frame-id total-stance-length)
                  (loop
                    with start-x = (* cel-width col)
                    with start-y = (* cel-height row)
                    for y from start-y below (+ start-y cel-height)
                    for dst = (cffi:inc-pointer data (+ (* 4 start-x) (* y pitch)))
                    do (cffi:with-pointer-to-vector-data (src (ase-cel-chunk-data chunk))
                         (memcpy dst
                                 (cffi:inc-pointer src (* 4 (- y start-y) cel-width))
                                 (* 4 cel-width)))))))
    finally
       (loop for bitmap across layer-bitmaps do (al:unlock-bitmap bitmap))
       (return layer-bitmaps)))

(defmethod make-prefab ((system sprite-system) prefab-name)
  (let* ((ase-file (load-aseprite
                    (make-instance 'binary-stream
                                   :path (prefab-path system prefab-name))))
         (stances (load-sprite-stances ase-file))
         (frame-durations (load-sprite-frame-durations ase-file stances))
         (layer-names (load-sprite-layer-names ase-file))
         (layer-bitmaps (load-sprite-layers ase-file layer-names stances))
         (layers (make-hash
                  :size (length layer-names) :test 'eq :init-format :keychain
                  :initial-contents layer-names :init-data layer-bitmaps)))
    (make-sprite-prefab
     :ase-file ase-file
     :layers layers
     :stances stances
     :frame-durations frame-durations)))

(defmethod make-prefab-component ((system sprite-system) entity prefab)
  (with-sprite entity ()
    (setf width (ase-file-width (sprite-prefab-ase-file prefab)))
    (setf height (ase-file-height (sprite-prefab-ase-file prefab)))
    (setf stances (sprite-prefab-stances prefab))
    (setf frame-durations (sprite-prefab-frame-durations prefab))
    (setf layers (sprite-prefab-layers prefab))
    (setf layers-toggled (make-hash
                          :size (hash-table-size layers) :init-format :keys
                          :initial-contents (loop for l being the hash-key of layers collect l)
                          :init-default nil))
    (setf stance 'idle)))

(defmethod make-component ((system sprite-system) entity &rest parameters)
  (declare (ignore system entity parameters))
  nil)

(defmethod system-load ((system sprite-system))
  t)

(defmethod system-update ((system sprite-system) dt)
  (declare (double-float dt))
  (with-sprites
    (incf time-counter dt)
    (let ((time-delta (the double-float (elt frame-durations frame))))
      (when (> time-counter time-delta)
        (decf time-counter time-delta)
        (let* ((all-frames (gethash stance stances))
               (remaining-frames (cdr (member frame all-frames :test #'=))))
          (setf frame
                (if remaining-frames
                    (first remaining-frames)
                    (if (eq stance 'death)
                        frame
                        (first all-frames)))))))))

(declaim
 (inline sprite-direction)
 (ftype (function (angle) unsigned-byte) sprite-direction))
(defun sprite-direction (angle)
  "West direction is 0 degree angle; counted clockwise"
  (declare (angle angle))
  (when (minusp angle)
    (setf angle (+ angle (* 2 pi))))
  (nth-value
   ;; TODO : support different # of directions
   0 (truncate (rem (round (* angle (/ 4 pi))) 8))))

(defmethod system-draw ((system sprite-system) renderer)
  (with-sprites
      (with-screen-coordinate entity (sprite-x sprite-y)
        ;; (log-info "player screen ~a, ~a" sprite-x sprite-y)
        (when (visiblep sprite-x sprite-y (max width height))
          (loop
            for layer being the hash-key using (hash-value toggled) of layers-toggled
            when toggled do
              (render
               renderer
               50 ;; TODO : player -> 30, stance=die -> 10, else 50
               (let ((layer layer)
                     (layers layers)
                     (width width)
                     (height height)
                     (frame frame)
                     (angle angle)
                     (sprite-x sprite-x)
                     (sprite-y sprite-y))
                 (multiple-value-bind (x y)
                     (absolute->viewport sprite-x sprite-y)
                   #'(lambda ()
                       (al:draw-bitmap-region
                        (gethash layer layers)
                        (* frame width)
                        (* (sprite-direction angle) height)
                        width height
                        (- x (truncate (- width *tile-width*) 2))
                        (- y (- (* 3 (truncate height 4)) (truncate *tile-height* 2)))
                        0))))))
          (with-system-config-options ((debug-sprite))
            (when debug-sprite
              (render
               renderer 1010
               (let ((width width)
                     (height height))
                 (multiple-value-bind (x y)
                     (absolute->viewport sprite-x sprite-y)
                   #'(lambda ()
                       (let ((x0 (- x (truncate (- width *tile-width*) 2)))
                             (y0 (- y (- (* 3 (truncate height 4)) (truncate *tile-height* 2)))))
                         (al:draw-rectangle
                          x0 y0 (+ x0 width) (+ y0 height)
                          (al:map-rgba
                           (first debug-sprite)
                           (second debug-sprite)
                           (third debug-sprite)
                           (or (fourth debug-sprite) 0))
                          0))))))))))))

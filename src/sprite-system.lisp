(in-package :d2clone-kit)


(defsystem sprite
  ()
  (:documentation "Handles movable sprites in Aseprite format.

The following format features are unsupported yet:

* color modes other than RGBA
* group layers
* layer blend modes
* linked cels"))

(deftype angle ()
  "Angle value in radians."
  `(double-float ,(- (* 2 pi)) ,(* 2 pi)))

(defcomponent sprite sprite
  (width 0 :type fixnum)
  (height 0 :type fixnum)
  (layer-names nil :type list)  ;; layer names to keep the order of layers
  (layer-batches nil :type hash-table)  ;; layer name (keyword) -> sprite batch entity
  (stances nil :type hash-table)  ;; stance name (keyword) -> list of frame #s
  (directions 0 :type fixnum) ;; count of directions
  (frame-durations nil :type (simple-array double-float))
  (frame-data nil :type (simple-array hash-table))
  (prefab-name nil :type keyword)
  ;; instance state
  (stance nil :type keyword)
  (frame 0 :type fixnum)
  (angle 0d0 :type angle)
  (time-counter 0d0 :type double-float)
  (layers-toggled nil :type hash-table)  ;; layer name (keyword) -> boolean
  (debug-entity -1 :type fixnum))

(defprefab sprite "ase"
  (width 0 :type fixnum)
  (height 0 :type fixnum)
  (layer-names nil :type list)  ;; layer names to keep the order of layers
  (layers nil :type hash-table)  ;; layer name (keyword) -> al_bitmap
  (stances nil :type hash-table)  ;; stance name (keyword) -> list of frame #s
  (directions 0 :type fixnum)  ;; count of directions
  (frame-durations nil :type (simple-array double-float))
  (frame-data nil :type (simple-array hash-table)))

(defun toggle-layer (entity layer &optional (on nil on-supplied-p))
  "Toggles layer LAYER on sprite entity ENTITY."
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

(defun load-sprite-frame-durations (ase-file total-stance-length)
  (delete-if
   #'identity
   (map '(vector double-float)
        #'(lambda (frame)
            (seconds (ase-frame-duration frame)))
        (ase-file-frames ase-file))
   :start total-stance-length))

(defun load-sprite-stances (ase-file)
  (loop :with stances := (make-hash :size 4 :test 'eq)
        :for frame :across (ase-file-frames ase-file)
        :do (loop :for chunk :across (ase-frame-chunks frame)
                  :when (and chunk (ase-tags-chunk-p chunk))
                    :do (loop
                          :for tag :across (ase-tags-chunk-tags chunk)
                          :for tag-name := (ase-tag-name tag)
                          :for stance-name := (make-keyword
                                               (format-symbol
                                                nil "~{~:@(~a~)~^-~}"
                                                (butlast
                                                 (uiop:split-string tag-name :separator '(#\-)))))
                          :do (if (uiop:string-suffix-p tag-name "-0")
                                  (setf (gethash stance-name stances)
                                        (loop :for i :from (ase-tag-from tag)
                                              :upto (ase-tag-to tag)
                                              :collect i))
                                  (when-let (stance-frames (gethash stance-name stances))
                                    (unless (= (1- (length stance-frames))
                                               (- (ase-tag-to tag) (ase-tag-from tag)))
                                      (error "length mismatch for tag ~a" tag-name))))))
        :finally (return stances)))

(defun load-sprite-layer-names (ase-file)
  (loop :with layer-names := (make-growable-vector)
        :for frame :across (ase-file-frames ase-file)
        :do (loop :for chunk :across (ase-frame-chunks frame)
                  :when (and chunk (ase-layer-chunk-p chunk))
                    :do (let ((id (ase-layer-chunk-id chunk))
                              (layer-name (ase-layer-chunk-name chunk)))
                          (setf (growable-vector-ref layer-names id)
                                (make-keyword (format-symbol nil "~:@(~a~)" layer-name)))))
        :finally (return (growable-vector-freeze layer-names))))

(defun total-stance-length (stances)
  (loop :for stance-name :being :the :hash-key :of stances
        :sum (length (gethash stance-name stances))))

(defun directions (ase-file total-stance-length)
  (ceiling (length (ase-file-frames ase-file)) total-stance-length))

(defun load-sprite-layers (ase-file layer-names stances total-stance-length directions)
  (loop
    :with cel-width := (ase-file-width ase-file)
    :with cel-height := (ase-file-height ase-file)
    :with frames := (ase-file-frames ase-file)
    :with frames-count := (length frames)
    :with bitmap-width := (* cel-width total-stance-length)
    :with bitmap-height := (* cel-height directions)
    :with layers-count := (length layer-names)
    :with layer-bitmaps := (make-array layers-count :element-type 'cffi:foreign-pointer
                                                    :initial-element (cffi:null-pointer))
    :with locked-bitmaps := (loop
                              :for i :below layers-count
                              :for bitmap := (al:create-bitmap bitmap-width bitmap-height)
                              :do (setf (elt layer-bitmaps i) bitmap)
                              :collect (al:lock-bitmap bitmap :abgr-8888 :writeonly))
    :for frame :across frames
    :for frame-id :below frames-count
    :do (loop
         :for chunk across (ase-frame-chunks frame)
         :when (and chunk (ase-cel-chunk-p chunk))
           :do (cffi:with-foreign-slots ((data pitch pixel-size)
                                        (elt locked-bitmaps (ase-cel-chunk-layer-id chunk))
                                        (:struct locked-bitmap-region))
                (multiple-value-bind (row col)
                    (truncate frame-id total-stance-length)
                  (loop
                    :with start-x := (* cel-width col)
                    :with start-y := (* cel-height row)
                    :for y :from start-y :below (+ start-y cel-height)
                    :for dst := (cffi:inc-pointer data (+ (* 4 start-x) (* y pitch)))
                    :do (cffi:with-pointer-to-vector-data (src (ase-cel-chunk-data chunk))
                          (memcpy dst
                                  (cffi:inc-pointer src (* 4 (- y start-y) cel-width))
                                  (* 4 cel-width)))))))
    :finally
       (loop :for bitmap :across layer-bitmaps :do (al:unlock-bitmap bitmap))
       (return layer-bitmaps)))

(defun load-sprite-frame-data (ase-file total-stance-length)
  (let ((result (make-array total-stance-length
                            :element-type 'hash-table
                            :initial-contents
                            (loop :repeat total-stance-length
                                  :collect (make-hash-table)))))
    (loop
      :for frame :across (ase-file-frames ase-file)
      :when (ase-frame-chunks frame)
        :do (loop
              :for chunk across (ase-frame-chunks frame)
              :when (and chunk (ase-user-data-chunk-p chunk))
                :do (let ((text (ase-user-data-chunk-text chunk))
                          (cel-id (ase-user-data-chunk-cel-id chunk)))
                      (unless (or (> cel-id total-stance-length)
                                  (length= 0 text))
                        (setf (aref result cel-id)
                              (plist-hash-table
                               (with-input-from-string (s text)
                                 (read s))
                               :test 'eq))))))
    result))

(defmethod make-prefab ((system sprite-system) prefab-name)
  (let* ((ase-file (load-aseprite
                    (make-instance 'binary-stream
                                   :path (prefab-path system prefab-name))))
         (stances (load-sprite-stances ase-file))
         (total-stance-length (total-stance-length stances))
         (directions (directions ase-file total-stance-length))
         (frame-durations (load-sprite-frame-durations ase-file total-stance-length))
         (frame-data (load-sprite-frame-data ase-file total-stance-length))
         (layer-names (load-sprite-layer-names ase-file))
         (layer-bitmaps
           (load-sprite-layers ase-file layer-names stances total-stance-length directions))
         (layers (make-hash
                  :size (length layer-names) :test 'eq :init-format :keychain
                  :initial-contents layer-names :init-data layer-bitmaps)))
    (make-sprite-prefab
     :width (ase-file-width ase-file)
     :height (ase-file-height ase-file)
     :layer-names (coerce layer-names 'list)
     :layers layers
     :stances stances
     :directions directions
     :frame-durations frame-durations
     :frame-data frame-data)))

(defmethod make-prefab-component ((system sprite-system) entity prefab parameters)
  (with-system-config-options ((debug-sprite))
    (with-sprite entity ()
      (setf width (sprite-prefab-width prefab))
      (setf height (sprite-prefab-height prefab))
      (setf stances (sprite-prefab-stances prefab))
      (setf directions (sprite-prefab-directions prefab))
      (setf frame-durations (sprite-prefab-frame-durations prefab))
      (setf frame-data (sprite-prefab-frame-data prefab))
      (let* ((layers (sprite-prefab-layers prefab)))
        (setf layer-names (sprite-prefab-layer-names prefab))
        (setf layer-batches (make-hash
                             :test 'eq
                             :initial-contents layers
                             :init-data #'(lambda (k v)
                                            (values
                                             k
                                             (make-object `((:sprite-batch
                                                             :bitmap ,v
                                                             :sprite-width ,width
                                                             :sprite-height ,height)))))))
        (setf layers-toggled (make-hash
                              :test 'eq
                              :init-format :keys
                              :initial-contents layer-names
                              :init-default nil))
        (destructuring-bind (&key (layers-initially-toggled '()) prefab) parameters
          (dolist (layer layers-initially-toggled)
            (setf (gethash layer layers-toggled) t))
          (setf prefab-name prefab)))
      (setf stance :idle)
      (issue sprite-stance-changed :entity entity :stance :idle)
      (setf frame 0
            angle 0d0
            time-counter 0d0)
      (when debug-sprite
        (setf debug-entity (make-object '((:debug :order 1010d0))))))))

(declaim
 (inline stance-interruptible-p)
 (ftype (function (fixnum) boolean)))
(defun stance-interruptible-p (entity)
  "Returns whether current stance can be interrupted for ENTITY."
  (with-sprite entity ()
    (not (gethash :non-interruptible (aref frame-data frame)))))

(declaim
 (inline switch-stance)
 (ftype (function (fixnum keyword)) switch-stance))
(defun switch-stance (entity new-stance)
  "Immediately switches stance of the sprite ENTITY to NEW-STANCE."
  (with-sprite entity ()
    (unless (or (eq stance new-stance)
                (gethash :non-interruptible (aref frame-data frame)))
      (setf stance new-stance)
      (setf frame (first (gethash new-stance stances)))
      (setf time-counter 0d0)
      (issue sprite-stance-changed :entity entity :stance new-stance))))

(defmethod system-update ((system sprite-system) dt)
  (declare (type double-float dt))
  (with-sprites
    (incf time-counter dt)
    (let ((time-delta (the double-float (elt frame-durations frame))))
      (when (> time-counter time-delta)
        (decf time-counter time-delta)
        (let* ((all-frames (gethash stance stances))
               (remaining-frames (cdr (member frame all-frames :test #'=)))
               (data (aref frame-data frame))
               (next-stance (values (gethash :next-stance data)))
               (last-stance (values (gethash :last-stance data))))
          (setf frame
                (cond
                  (remaining-frames
                   (first remaining-frames))
                  (last-stance
                   frame)
                  (next-stance
                   (setf stance next-stance)
                   (issue sprite-stance-changed :entity entity :stance next-stance)
                   (first (gethash next-stance stances)))
                  (t
                   (setf stance :idle)
                   (issue sprite-stance-changed :entity entity :stance :idle)
                   (first (gethash :idle stances))))))))))

(defconstant +isometric-angle+ (* pi (/ 45 180)))

(declaim
 (inline sprite-direction)
 (ftype (function (fixnum double-float) unsigned-byte) sprite-direction))
(defun sprite-direction (directions angle)
  "Calculates sprite direction from angle value ANGLE assuming total sprite direction count DIRECTIONS. East direction is 0 degree angle; counted clockwise."
  ;; (declare (type angle angle))
  (let ((angle (+ angle +isometric-angle+)))
    (when (minusp angle)
      (incf angle (* 2 pi)))
    (when (> angle (* 2 pi))
      (decf angle (* 2 pi)))
    (nth-value
     0 (truncate (rem (round (* angle directions) (* 2 pi)) directions)))))

(defmethod system-draw ((system sprite-system) renderer)
  (with-system-config-options ((debug-sprite))
    (with-sprites
        (loop :for layer :being :the :hash-values :in layer-batches :do
          (clear-sprite-batch layer))
        (with-screen-coordinate entity (sprite-x sprite-y)
          (multiple-value-bind (x y)
              (absolute->viewport sprite-x sprite-y)
            (when (visiblep x y (max width height))
              (let ((x0 (- x (truncate (- width *tile-width*) 2)))
                    (y0 (- y (- (* 3 (truncate height 4)) (truncate *tile-height* 2)))))
                (loop
                  :for layer :being :the :hash-key :using (hash-value toggled) :of layers-toggled
                  :when toggled :do
                    (add-sprite-to-batch
                     (gethash layer layer-batches)
                     (+ y (truncate *tile-height* -2)
                        (coerce
                         (/ (position layer layer-names) (length layer-names))
                         'double-float))
                     (* frame width)
                     (* (sprite-direction directions angle) height)
                     x0 y0))
                (when debug-sprite
                  (add-debug-rectangle debug-entity x0 y0 width height debug-sprite)))))))))

(defhandler sprite-system entity-died (event entity damage-fraction)
  (with-sprite entity ()
    (let ((new-stance
            (if (and (gethash :critdeath stances)
                     (> damage-fraction 0.1d0))
                :critdeath
                :death)))
      (setf stance new-stance)
      (setf frame (first (gethash new-stance stances)))
      (setf time-counter 0d0)
      (issue sprite-stance-changed :entity entity :stance new-stance))))

(defmethod delete-component :before ((system sprite-system) entity)
  (with-sprite entity ()
    (loop :for sprite-batch :being :the :hash-value :in layer-batches
          :do (delete-entity sprite-batch))
    (unless (minusp debug-entity)
      (delete-entity debug-entity))))

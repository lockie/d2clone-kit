(in-package :d2clone-kit)


(defsystem sprite
  ()
  (:documentation "Handles movable sprites in Aseprite format.

The following format features are unsupported yet:

* color modes other than RGBA
* group layers
* layer blend modes
* linked cels"))

;; TODO : document sprite properties.

(deftype angle ()
  "Angle value in radians."
  `(double-float ,(- (* 2 pi)) ,(* 2 pi)))

(defcomponent (sprite)
  (width nil :type fixnum)
  (height nil :type fixnum)
  (layer-names
   nil :type list
       :documentation "layer names to keep the order of layers")
  (layer-batches
   nil :type hash-table
       :documentation "layer name (keyword) -> sprite batch entity")
  ;; TODO : consider renaming stance to sequence
  (stances
   nil :type hash-table
       :documentation "stance name (keyword) -> list of frame #s")
  (directions
   nil :type fixnum
       :documentation "count of directions")
  (frame-durations nil :type (simple-array double-float))
  (frame-data nil :type hash-table)
  (layer-data nil :type hash-table)
  (prefab-name nil :type keyword)
  ;; instance state
  (stance :idle :type keyword)
  (frame
   0 :type fixnum
     :documentation "not an actual frame in ase file, but rather frame no. in
animation stance")
  (angle 0d0 :type angle)
  (time-counter 0d0 :type double-float)
  (layers-toggled
   nil :type hash-table
       :documentation "layer name (keyword) -> boolean")
  (debug-entity +invalid-entity+ :type fixnum))

(defprefab sprite "ase"
  (width nil :type fixnum)
  (height nil :type fixnum)
  (layer-names nil :type list)
  ;; layer name (keyword) -> al_bitmap
  (layers nil :type hash-table)
  (stances nil :type hash-table)
  (directions nil :type fixnum)
  (frame-durations nil :type (simple-array double-float))
  ;; layer name (keyword) -> array of hash-tables with properties
  ;; corresponding to each frame
  (frame-data nil :type hash-table)
  ;; layer name (keyword) -> hash-table with layer properties
  (layer-data nil :type hash-table)
  )

(defun toggle-layer (entity layer &optional (on nil on-supplied-p))
  "Toggles layer LAYER on sprite entity ENTITY."
  (with-sprite entity ()
    (multiple-value-bind (old-value layer-exists)
        (gethash layer layers-toggled)
      (unless layer-exists
        ;; TODO : restart to choose existing layer
        (error "no such layer: ~a" layer))
      (let ((value (if on-supplied-p on (not old-value))))
        (setf (gethash layer layers-toggled) value)))))

(declaim
 #-d2c-debug (inline default-layer)
 (ftype (function (fixnum) keyword) default-layer))
(defun default-layer (entity)
  (with-sprite entity ()
    (first layer-names)))

(declaim
 #-d2c-debug (inline frame-property)
 (ftype (function (fixnum keyword &key (:layer keyword))) frame-property))
(defun frame-property (entity property-name &key layer)
  "Returns current frame's property called PROPERTY-NAME of a sprite component
of ENTITY within the LAYER (or the default layer, if not specified). Retruns
NIL if no such property exists."
  ;; TODO : optimize by having property-name as multiple values + returning
  ;; multiple values
  (with-sprite entity ()
    (gethash
     property-name
     (aref
      (the simple-vector
           (gethash
            (or layer (default-layer entity))
            frame-data))
      frame))))

(defun layer-property (entity property-name &key layer)
  "Returns LAYER's property called PROPERTY-NAME of a sprite component of
ENTITY. Returns NIL of no such property exists."
  ;; TODO : default?.. (in frame-property too)
  ;; TODO : layer parameter logic ???
  (with-sprite entity ()
    (gethash
     property-name
     (gethash
      (or layer (default-layer entity))
      layer-data))))

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
 #-d2c-debug (inline seconds)
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

;; TODO #51 Note that you can query some additional read-only properties such
;; as the maximum allowed bitmap (i.e. texture) size via
;; al_get_display_option.  ALLEGRO_MAX_BITMAP_SIZE When queried this returns
;; the maximum size (width as well as height) a bitmap can have for this
;; display. Calls to al_create_bitmap or al_load_bitmap for bitmaps larger
;; than this size will fail. It does not apply to memory bitmaps which always
;; can have arbitrary size (but are slow for drawing).

(defun load-sprite-stances (ase-file)
  (loop :with stances := (make-hash :size 4 :test #'eq)
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
                                                 (uiop:split-string
                                                  tag-name
                                                  :separator '(#\-)))))
                          :do (if (uiop:string-suffix-p tag-name "-0")
                                  (setf (gethash stance-name stances)
                                        (loop :for i :from (ase-tag-from tag)
                                              :upto (ase-tag-to tag)
                                              :collect i))
                                  (when-let (stance-frames
                                             (gethash stance-name stances))
                                    (unless (= (1- (length stance-frames))
                                               (- (ase-tag-to tag)
                                                  (ase-tag-from tag)))
                                      (error "length mismatch for tag ~a"
                                             tag-name))))))
        :finally (return stances)))

(defun load-sprite-layer-names (ase-file)
  (loop :with layer-names := (make-growable-vector)
        :for frame :across (ase-file-frames ase-file)
        :do (loop :for chunk :across (ase-frame-chunks frame)
                  :when (and chunk (ase-layer-chunk-p chunk))
                    :do (let ((id (ase-layer-chunk-id chunk))
                              (layer-name (ase-layer-chunk-name chunk)))
                          (setf (growable-vector-ref* layer-names id)
                                (make-keyword
                                 (format-symbol nil "~:@(~a~)" layer-name)))))
        :finally (return (growable-vector-freeze layer-names))))

(defun total-stance-length (stances)
  (loop :for stance-name :being :the :hash-key :of stances
        :sum (length (gethash stance-name stances))))

(defun directions (ase-file total-stance-length)
  (ceiling (length (ase-file-frames ase-file)) total-stance-length))

(defun load-sprite-layers (ase-file layer-names total-stance-length directions)
  (loop
    :with cel-width := (ase-file-width ase-file)
    :with cel-height := (ase-file-height ase-file)
    :with frames := (ase-file-frames ase-file)
    :with frames-count := (length frames)
    :with bitmap-width := (* cel-width total-stance-length)
    :with bitmap-height := (* cel-height directions)
    :with layers-count := (length layer-names)
    :with layer-bitmaps := (make-array layers-count
                                       :element-type 'cffi:foreign-pointer
                                       :initial-element (cffi:null-pointer))
    :with locked-bitmaps := (loop
                              :for i :below layers-count
                              :for bitmap :=
                                 (al:create-bitmap bitmap-width bitmap-height)
                              :do (setf (elt layer-bitmaps i) bitmap)
                              :collect
                                 (al:lock-bitmap bitmap :abgr-8888 :writeonly))
    :for frame :across frames
    :for frame-id :below frames-count
    :do (loop
          :for chunk across (ase-frame-chunks frame)
          :when (and chunk (ase-cel-chunk-p chunk))
          :do (cffi:with-foreign-slots
                  ((data pitch pixel-size)
                   (elt locked-bitmaps (ase-cel-chunk-layer-id chunk))
                   (:struct locked-bitmap-region))
                (multiple-value-bind (row col)
                    (truncate frame-id total-stance-length)
                  (loop
                    :with start-x := (* cel-width col)
                    :with start-y := (* cel-height row)
                    :for y :from start-y :below (+ start-y cel-height)
                    :for dst := (cffi:inc-pointer data (+ (* 4 start-x)
                                                          (* y pitch)))
                    :do (cffi:with-pointer-to-vector-data
                            (src (ase-cel-chunk-data chunk))
                          (memcpy dst
                                  (cffi:inc-pointer src (* 4 (- y start-y)
                                                           cel-width))
                                  (* 4 cel-width)))))))
    :finally
       (loop :for bitmap :across layer-bitmaps :do (al:unlock-bitmap bitmap))
       (return layer-bitmaps)))

(defun load-sprite-frame-data (ase-file layer-names total-stance-length)
  (loop
    :with result := (make-hash :test #'eq
                               :init-format :keychain
                               :initial-contents layer-names
                               :init-data (loop :repeat (length layer-names)
                                                :collect
                                                   (make-array
                                                    total-stance-length
                                                    :element-type 'hash-table
                                                    :initial-contents
                                                    (loop
                                                      :repeat
                                                      total-stance-length
                                                      :collect
                                                         (make-hash-table)))))
    :for frame :across (ase-file-frames ase-file)
    :when (ase-frame-chunks frame)
    :do (loop
          :for chunk across (ase-frame-chunks frame)
          :when (and chunk (ase-user-data-chunk-p chunk))
          :do (let ((text (ase-user-data-chunk-text chunk))
                    (layer-id (ase-user-data-chunk-layer-id chunk))
                    (cel-id (ase-user-data-chunk-cel-id chunk)))
                (unless (or (minusp cel-id)
                            (> cel-id total-stance-length)
                            (length= 0 text))
                  (setf (aref (gethash (aref layer-names layer-id) result)
                              cel-id)
                        (plist-hash-table
                         (with-input-from-string (s text)
                           (read s))
                         :test #'eq)))))
    :finally (return result)))

(defun load-sprite-layer-data (ase-file layer-names)
  (loop
    :with result := (make-hash :test #'eq
                               :init-format :keychain
                               :initial-contents layer-names
                               :init-data (loop :repeat (length layer-names)
                                                :collect
                                                   (make-hash :test #'eq)))
    :for frame :across (ase-file-frames ase-file)
    :when (ase-frame-chunks frame)
    :do (loop
          :for chunk across (ase-frame-chunks frame)
          :when (and chunk (ase-user-data-chunk-p chunk))
          :do (let ((text (ase-user-data-chunk-text chunk))
                    (layer-id (ase-user-data-chunk-layer-id chunk))
                    (cel-id (ase-user-data-chunk-cel-id chunk)))
                (when (and (minusp cel-id)
                           (not (length= 0 text)))
                  (setf (gethash (aref layer-names layer-id) result)
                        (plist-hash-table
                         (with-input-from-string (s text)
                           (read s))
                         :test #'eq)))))
    :finally (return result)))

(defmethod make-prefab ((system sprite-system) prefab-name)
  (let* ((ase-file (load-aseprite
                    (make-instance 'binary-stream
                                   :path (prefab-path system prefab-name))))
         (stances (load-sprite-stances ase-file))
         (total-stance-length (total-stance-length stances))
         (directions (directions ase-file total-stance-length))
         (frame-durations
           (load-sprite-frame-durations ase-file total-stance-length))
         (layer-names (load-sprite-layer-names ase-file))
         (frame-data
           (load-sprite-frame-data ase-file layer-names total-stance-length))
         (layer-data (load-sprite-layer-data ase-file layer-names))
         (layer-bitmaps (load-sprite-layers
                         ase-file layer-names total-stance-length directions))
         (layers (make-hash
                  :size (length layer-names) :test #'eq :init-format :keychain
                  :initial-contents layer-names :init-data layer-bitmaps)))
    (make-sprite-prefab
     :width (ase-file-width ase-file)
     :height (ase-file-height ase-file)
     :layer-names (coerce layer-names 'list)
     :layers layers
     :stances stances
     :directions directions
     :frame-durations frame-durations
     :frame-data frame-data
     :layer-data layer-data)))

(declaim
 #-d2c-debug (inline emit-frame-sound)
 (ftype (function (fixnum)) emit-frame-sound))
(defun emit-frame-sound (entity)
  "Emits sound associated with frame through :SOUND property, if any."
  (flet ((emit-layer-sound (layer)
           (when-let (sound (frame-property entity :sound :layer layer))
             (and (make-component *sound-system* entity :prefab sound) t))))
    (with-sprite entity ()
      (loop :with default-layer :of-type keyword := (default-layer entity)
            :with non-default-layer-emitted-sound-p :of-type boolean := nil
            :for layer :of-type keyword
            :being :the :hash-key
            :using (hash-value toggled) :of layers-toggled
            :when (and toggled (not (eq layer default-layer))) :do
               (when (emit-layer-sound layer)
                 (setf non-default-layer-emitted-sound-p t))
            :finally (unless non-default-layer-emitted-sound-p
                       (emit-layer-sound default-layer))))))

(declaim
 #-d2c-debug (inline %switch-stance)
 (ftype (function (fixnum keyword)) %switch-stance))
(defun %switch-stance (entity new-stance)
  "Immediately switches stance of the sprite ENTITY to NEW-STANCE without any
extra checks. Does not affect TIME-COUNTER of sprite."
  (with-sprite entity ()
    (let ((old-stance stance))
      (setf stance new-stance
            frame (first (gethash new-stance stances)))
      (emit-frame-sound entity)
      (issue (sprite-stance-changed)
             :entity entity :old-stance old-stance :new-stance new-stance))))

(defmethod make-prefab-component ((system sprite-system) entity prefab
                                  parameters)
  (with-system-config-options ((debug-sprite))
    (let ((width (sprite-prefab-width prefab))
          (height (sprite-prefab-height prefab)))
      (make-sprite
       entity
       :width width
       :height height
       :stances (sprite-prefab-stances prefab)
       :directions (sprite-prefab-directions prefab)
       :frame-durations (sprite-prefab-frame-durations prefab)
       :frame-data (sprite-prefab-frame-data prefab)
       :layer-data (sprite-prefab-layer-data prefab)
       :layer-names (sprite-prefab-layer-names prefab)
       :layer-batches (make-hash
                       :test #'eq
                       :initial-contents (sprite-prefab-layers prefab)
                       :init-data #'(lambda (k v)
                                      (values
                                       k
                                       (make-object `((:sprite-batch
                                                       :bitmap ,v
                                                       :sprite-width ,width
                                                       :sprite-height ,height))
                                                    entity))))
       :layers-toggled (let ((layers-toggled
                               (make-hash
                                :test #'eq
                                :init-format :keys
                                :initial-contents
                                (sprite-prefab-layer-names prefab)
                                :init-default nil)))
                         (destructuring-bind
                             (&key (layers-initially-toggled nil)
                              &allow-other-keys) parameters
                           (dolist (layer layers-initially-toggled)
                             (setf (gethash layer layers-toggled) t)))
                         layers-toggled)
       :prefab-name (getf parameters :prefab)
       :debug-entity (if debug-sprite
                         (make-object '((:debug :order 1010d0)) entity)
                         +invalid-entity+))))
  (%switch-stance entity :idle))

(declaim
 #-d2c-debug (inline stance-interruptible-p)
 (ftype (function (fixnum) boolean) stance-interruptible-p))
(defun stance-interruptible-p (entity)
  "Returns whether current stance can be interrupted for ENTITY."
    (not (frame-property entity :non-interruptible)))

(declaim
 #-d2c-debug (inline current-stance)
 (ftype (function (fixnum) keyword) current-stance))
(defun current-stance (entity)
  (with-sprite entity ()
    stance))

(declaim
 #-d2c-debug (inline switch-stance)
 (ftype (function (fixnum keyword)) switch-stance))
(defun switch-stance (entity new-stance)
  "Immediately switches stance of the sprite ENTITY to NEW-STANCE unless it is
already at that stance or the current stance is non-interruptible."
  (with-sprite entity ()
    (unless (or (eq stance new-stance)
                (not (stance-interruptible-p entity)))
      (setf time-counter 0d0)
      (%switch-stance entity new-stance))))

(declaim
 #-d2c-debug (inline frame-finished-p)
 (ftype (function (fixnum) (or null double-float)) frame-finished-p))
(defun frame-finished-p (entity)
  "Returns generalized boolean indicating whether current animation frame of
ENTITY is finished and should be switched to the next one. If the latter is
the case, return the double float representing the amount of time during which
it was."
  (with-sprite entity ()
    (let* ((frame-duration (the double-float (elt frame-durations frame)))
           (frame-duration-left (- frame-duration *delta-time*)))
      (if (> time-counter frame-duration-left)
          frame-duration-left
          nil))))

(declaim
 #-d2c-debug (inline stance-finished-p)
 (ftype (function (fixnum) boolean) stance-finished-p))
(defun stance-finished-p (entity)
  "Returns whether current stance of animation of ENTITY is played through."
  (with-sprite entity ()
    (and (frame-finished-p entity)
         (= frame (the fixnum (car (last (gethash stance stances))))))))

(defmethod system-update ((system sprite-system))
  (with-sprites
    (if-let (frame-duration-left (frame-finished-p entity))
      (let* ((all-frames (gethash stance stances))
             (remaining-frames (cdr (member frame all-frames :test #'=)))
             (next-stance (frame-property entity :next-stance))
             (last-stance (frame-property entity :last-stance)))
        (decf time-counter frame-duration-left)
        (cond
          (remaining-frames
           (setf frame (first remaining-frames))
           (emit-frame-sound entity))
          (last-stance)
          (next-stance
           (%switch-stance entity next-stance))
          (t
           (%switch-stance entity :idle))))
      (incf time-counter *delta-time*))))

(defconstant +isometric-angle+ (* pi (/ 45 180)))

(declaim
 #-d2c-debug (inline sprite-direction)
 (ftype (function (fixnum double-float) unsigned-byte) sprite-direction))
(defun sprite-direction (directions angle)
  "Calculates sprite direction from angle value ANGLE assuming total sprite
direction count DIRECTIONS. East direction is 0 degree angle; counted
clockwise."
  ;; (declare (type angle angle))
  (let ((angle (+ angle +isometric-angle+)))
    (when (minusp angle)
      (incf angle (* 2 pi)))
    (when (> angle (* 2 pi))
      (decf angle (* 2 pi)))
    (nth-value
     0 (truncate (rem (round (* angle directions) (* 2 pi)) directions)))))

(defmethod system-draw ((system sprite-system) renderer)
  (with-system-config-options ((debug-sprite display-height))
    (with-sprites
        (loop :for layer :being :the :hash-values :in layer-batches :do
          (clear-sprite-batch layer))
        (with-screen-coordinate entity (sprite-x sprite-y)
          (multiple-value-bind (x y)
              (absolute->viewport sprite-x sprite-y)
            (when (visiblep x y (max width height))
              (let ((x0 (- x (truncate width 2)))
                    (y0 (- y (* 3 (truncate height 4)))))
                (loop
                  :for layer :being :the :hash-key
                  :using (hash-value toggled) :of layers-toggled
                  :when toggled :do
                    (add-sprite-to-batch
                     (gethash layer layer-batches)
                     (+ y (truncate *tile-height* -2)
                        (if (and (%has-component-p *hp-system* entity)
                                 (deadp entity))
                            (- (coerce (/ display-height *tile-height*)
                                       'double-float))
                            0)
                        (coerce
                         (/ (position layer layer-names) (length layer-names))
                         'double-float))
                     (* frame width)
                     (* (sprite-direction directions angle) height)
                     x0 y0))
                (when debug-sprite
                  (add-debug-rectangle debug-entity x0 y0 width height
                                       debug-sprite)))))))))

(defhandler (sprite-system entity-died)
  (let ((entity (entity-died-entity event))
        (damage-fraction (entity-died-damage-fraction event)))
    (with-sprite entity ()
      (let ((new-stance
              (if (and (gethash :critdeath stances)
                       (> damage-fraction 0.1d0))
                  :critdeath
                  :death)))
        (setf time-counter 0d0)
        (%switch-stance entity new-stance)))))

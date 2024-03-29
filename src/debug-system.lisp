(in-package :d2clone-kit)


(defsystem debug
  ((font (al:create-builtin-font) :type cffi:foreign-pointer)
   (texts (make-hash-table :test #'eq) :type hash-table)
   (text-designators nil :type list))
  (:documentation "Handles drawing of various debug information."
   :order 10))

(defcomponent (debug debug-primitive)
  (render-order nil :type double-float)
  (type nil :type keyword)
  (points nil :type growable-vector))

(defconstant +point-size+ 9)

(declaim
 (ftype (function
         (fixnum fixnum fixnum fixnum fixnum fixnum &optional (or fixnum null)))
        add-debug-point))
(defun add-debug-point (entity x y r g b &optional (a nil))
  "Adds point with screen coordinates X, Y and color R, G, B, A to debug
primitive ENTITY. If primitive's type is not :POLYLINE, the behaviour is
undefined."
  (with-debug-primitive entity ()
    (let ((count (growable-vector-length points)))
      (growable-vector-grow points (+ count +point-size+))
      (setf (growable-vector-ref points (+ count 0)) (float x))
      (setf (growable-vector-ref points (+ count 1)) (float y))
      (setf (growable-vector-ref points (+ count 2)) 0f0) ;; z
      (setf (growable-vector-ref points (+ count 3)) 0f0) ;; u
      (setf (growable-vector-ref points (+ count 4)) 0f0) ;; v
      (setf (growable-vector-ref points (+ count 5)) (float r))
      (setf (growable-vector-ref points (+ count 6)) (float g))
      (setf (growable-vector-ref points (+ count 7)) (float b))
      (setf (growable-vector-ref points (+ count 8)) (float (or a 0))))))

(declaim (ftype (function (fixnum fixnum fixnum fixnum fixnum list))
                add-debug-rectangle))
(defun add-debug-rectangle (entity x y w h color)
  "Adds rectangle with screen coordinates X, Y, dimensions W, H and color
COLOR to debug primitive ENTITY. If primitive's type is not :POLYLINE, the
behaviour is undefined."
  (let ((r (first color))
        (g (second color))
        (b (third color))
        (a (fourth color)))
    (add-debug-point entity x y r g b a)
    ;; TODO : fix types (sbcl tries to alloc bignum)
    (add-debug-point entity (+ x w) y r g b a)
    (add-debug-point entity (+ x w) y r g b a)
    (add-debug-point entity (+ x w) (+ y h) r g b a)
    (add-debug-point entity (+ x w) (+ y h) r g b a)
    (add-debug-point entity x (+ y h) r g b a)
    (add-debug-point entity x (+ y h) r g b a)
    (add-debug-point entity x y r g b a)))

(declaim (ftype (function (fixnum fixnum fixnum list boolean))
                add-debug-tile-rhomb))
;; TODO : make color param API consistent with add-debug-point ???
(defun add-debug-tile-rhomb (entity x y color mark)
  "Adds grid rhomb with screen coordinates X, Y and color COLOR to debug
primitive ENTITY. If MARK is T, then rhomb is drawn crossed out. If
primitive's type is not :POLYLINE, the behaviour is undefined."
  (let ((r (first color))
        (g (second color))
        (b (third color))
        (a (fourth color)))
    (add-debug-point entity (+ x (ceiling *tile-width* 2)) y r g b a)
    (add-debug-point entity (+ x *tile-width*) (+ y (ceiling *tile-height* 2))
                     r g b a)
    (add-debug-point entity (+ x *tile-width*) (+ y (ceiling *tile-height* 2))
                     r g b a)
    (add-debug-point entity (+ x (ceiling *tile-width* 2)) (+ y *tile-height*)
                     r g b a)
    (add-debug-point entity (+ x (ceiling *tile-width* 2)) (+ y *tile-height*)
                     r g b a)
    (add-debug-point entity x (+ y (ceiling *tile-height* 2)) r g b a)
    (add-debug-point entity x (+ y (ceiling *tile-height* 2)) r g b a)
    (add-debug-point entity (+ x (ceiling *tile-width* 2)) y r g b a)
    (when mark
      (add-debug-point entity (+ x (ceiling *tile-width* 2)) y r g b a)
      (add-debug-point entity (+ x (ceiling *tile-width* 2)) (+ y *tile-height*)
                       r g b a)
      (add-debug-point entity (+ x *tile-width*) (+ y (ceiling *tile-height* 2))
                       r g b a)
      (add-debug-point entity x (+ y (ceiling *tile-height* 2)) r g b a))))

(declaim (ftype (function (fixnum real real real real list
                                  &optional single-float))
                add-debug-ellipse))
(defun add-debug-ellipse (entity x y rx ry color &optional (thickness 1f0))
  "Adds an ellipse with screen coordinates X, Y, radii RX, RY and color COLOR
to debug primitive ENTITY. Only works if the ellipse is the single item in the
debug primitive. If primitive's type is not :ELLIPSE, the behaviour is
undefined."
  (let ((r (first color))
        (g (second color))
        (b (third color))
        (a (fourth color)))
    (with-debug-primitive entity ()
      (growable-vector-grow points +point-size+)
      (setf (growable-vector-ref points 0) (float x))
      (setf (growable-vector-ref points 1) (float y))
      (setf (growable-vector-ref points 2) thickness)
      (setf (growable-vector-ref points 3) (float rx))
      (setf (growable-vector-ref points 4) (float ry))
      (setf (growable-vector-ref points 5) r)
      (setf (growable-vector-ref points 6) g)
      (setf (growable-vector-ref points 7) b)
      (setf (growable-vector-ref points 8) (or a 0)))))

(declaim (ftype (function (keyword string &rest t)) add-debug-text))
(defun add-debug-text (designator text &rest args)
  "Adds debug text TEXT using FORMAT-like arguments ARGS to display on top
left corner of the screen. To identify same text line each frame, set keyword
DESIGNATOR to same value."
  (with-system-slots ((texts text-designators) :of debug-system :read-only nil)
    (pushnew designator text-designators)
    (setf (gethash designator texts) (apply #'format nil text args))))

;; TODO : helper function to clear the list of debug texts

(defmethod make-component ((system debug-system) entity &rest parameters)
  (destructuring-bind (&key (size 144) (order 1000d0) (type :polyline))
      parameters
    (make-debug-primitive
     entity
     :render-order order
     :type type
     :points (make-growable-vector :initial-element 0d0
                                   :initial-allocated-size size))))

(defmethod system-finalize ((system debug-system))
  (al:destroy-font (debug-system-font system)))

(defmethod system-update ((system debug-system))
  ;; TODO !!! document that the arrays are cleared every frame
  (with-debug-primitives
      (growable-vector-clear points)))

(defmethod system-draw ((system debug-system) renderer)
  (with-debug-primitives
       (render
        renderer render-order
        (let ((points points)
              (type type))
          #'(lambda ()
              (ecase type
                (:ellipse
                 (loop :for i :of-type fixnum :from 0
                       :to (1- (growable-vector-length points)) :by +point-size+
                       :do (al:draw-ellipse
                            (growable-vector-ref points 0)
                            (growable-vector-ref points 1)
                            (growable-vector-ref points 3)
                            (growable-vector-ref points 4)
                            (al:map-rgba
                             (growable-vector-ref points 5)
                             (growable-vector-ref points 6)
                             (growable-vector-ref points 7)
                             (growable-vector-ref points 8))
                            (growable-vector-ref points 2))))
                (:polyline
                 (cffi:with-pointer-to-vector-data
                     (ptr (growable-vector-freeze
                           points
                           :element-type 'single-float))
                   (al:draw-prim
                    ptr (cffi:null-pointer) (cffi:null-pointer) 0
                    (ceiling
                     (growable-vector-length points)
                     +point-size+)
                    0))))))))
  (with-system-slots ((font texts text-designators)
                      :of debug-system :instance system)
    (loop
      :with color := (al:map-rgb 255 255 255)
      :for designator :in (reverse text-designators)
      :for text := (gethash designator texts)
      :for i :from 0
      :unless (emptyp text)
        :do (render
             renderer
             5000d0
             (let ((i i)
                   (text text))
               #'(lambda () (al:draw-text font color 0 (* i 10) 0 text)))))))

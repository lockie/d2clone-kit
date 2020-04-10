(in-package :d2clone-kit)

(defclass debug-system (system)
  ((name :initform 'debug)
   (order :initform 10)
   (font :initform nil)
   (texts :initform (make-hash-table :test 'eq))
   (text-designators :initform nil))
  (:documentation "Handles drawing of various debug information."))

(defcomponent debug debug-buffer
  (render-order 0d0 :type double-float)
  (points nil :type growable-vector))

(declaim
 (ftype (function
         (fixnum fixnum fixnum fixnum fixnum fixnum &optional (or fixnum null)))
        add-debug-point))
(defun add-debug-point (entity x y r g b &optional (a nil))
  "Adds point with screen coordinates X, Y and color R, G, B, A to debug buffer ENTITY."
  (with-debug-buffer entity ()
    (let ((count (growable-vector-length points)))
      (growable-vector-grow points (+ count 9))
      (setf (%growable-vector-ref points (+ count 0)) (float x))
      (setf (%growable-vector-ref points (+ count 1)) (float y))
      (setf (%growable-vector-ref points (+ count 2)) 0f0) ;; z
      (setf (%growable-vector-ref points (+ count 3)) 0f0) ;; u
      (setf (%growable-vector-ref points (+ count 4)) 0f0) ;; v
      (setf (%growable-vector-ref points (+ count 5)) (float r))
      (setf (%growable-vector-ref points (+ count 6)) (float g))
      (setf (%growable-vector-ref points (+ count 7)) (float b))
      (setf (%growable-vector-ref points (+ count 8)) (float (or a 0))))))

(declaim (ftype (function (fixnum fixnum fixnum fixnum fixnum list)) add-debug-rectangle))
(defun add-debug-rectangle (entity x y w h color)
  "Adds rectangle with screen coordinates X, Y, dimensions W, H and color COLOR to debug buffer ENTITY."
  (let ((r (first color))
        (g (second color))
        (b (third color))
        (a (fourth color)))
    (add-debug-point entity x y r g b a)
    (add-debug-point entity (+ x w) y r g b a)
    (add-debug-point entity (+ x w) y r g b a)
    (add-debug-point entity (+ x w) (+ y h) r g b a)
    (add-debug-point entity (+ x w) (+ y h) r g b a)
    (add-debug-point entity x (+ y h) r g b a)
    (add-debug-point entity x (+ y h) r g b a)
    (add-debug-point entity x y r g b a)))

(declaim (ftype (function (fixnum fixnum fixnum list boolean)) add-debug-tile-rhomb))
(defun add-debug-tile-rhomb (entity x y color mark)
  "Adds grid rhomb with screen coordinates X, Y and color COLOR to debug buffer ENTITY. If MARK is T, then rhomb is drawn crossed out."
  (let ((r (first color))
        (g (second color))
        (b (third color))
        (a (fourth color)))
  (add-debug-point entity (+ x (ceiling *tile-width* 2)) y r g b a)
  (add-debug-point entity (+ x *tile-width*) (+ y (ceiling *tile-height* 2)) r g b a)
  (add-debug-point entity (+ x *tile-width*) (+ y (ceiling *tile-height* 2)) r g b a)
  (add-debug-point entity (+ x (ceiling *tile-width* 2)) (+ y *tile-height*) r g b a)
  (add-debug-point entity (+ x (ceiling *tile-width* 2)) (+ y *tile-height*) r g b a)
  (add-debug-point entity x (+ y (ceiling *tile-height* 2)) r g b a)
  (add-debug-point entity x (+ y (ceiling *tile-height* 2)) r g b a)
  (add-debug-point entity (+ x (ceiling *tile-width* 2)) y r g b a)
  (when mark
    (add-debug-point entity (+ x (ceiling *tile-width* 2)) y r g b a)
    (add-debug-point entity (+ x (ceiling *tile-width* 2)) (+ y *tile-height*) r g b a)
    (add-debug-point entity (+ x *tile-width*) (+ y (ceiling *tile-height* 2)) r g b a)
    (add-debug-point entity x (+ y (ceiling *tile-height* 2)) r g b a))))

(declaim (ftype (function (keyword string &rest t)) add-debug-text))
(defun add-debug-text (designator text &rest args)
  "Adds debug text TEXT using FORMAT-like arguments ARGS to display on top left corner of the screen. To identify same text line each frame, set keyword DESIGNATOR to same value."
  (with-slots (texts text-designators)
      (system-ref 'debug)
    (pushnew designator text-designators)
    (setf (gethash designator texts) (apply #'format nil text args))))

(defmethod make-component ((system debug-system) entity &rest parameters)
  (destructuring-bind (&key (size 144) (order 1000d0)) parameters
    (with-debug-buffer entity ()
      (setf render-order order)
      (setf points (make-growable-vector :initial-element 0d0 :initial-allocated-size size)))))

(defmethod system-update ((system debug-system) dt)
  (with-debug-buffers
      (growable-vector-clear points)))

(defmethod system-draw ((system debug-system) renderer)
  (with-debug-buffers
      (render
       renderer render-order
       (let ((points points))
         #'(lambda ()
             (cffi:with-pointer-to-vector-data
                 (ptr (growable-vector-freeze points :element-type 'single-float))
               (al:draw-prim ptr (cffi:null-pointer) (cffi:null-pointer) 0
                             (ceiling (growable-vector-length points) 9) 0))))))
  (with-slots (font texts text-designators) system
    (unless font
      (setf font (al:create-builtin-font)))
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

(defhandler debug-system quit (event)
  (with-slots (font) system
    (al:destroy-font font)
    (setf font nil)))

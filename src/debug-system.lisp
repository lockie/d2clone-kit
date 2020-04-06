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
  (count 0 :type fixnum)
  (points nil :type (simple-array single-float)))

(declaim
 (ftype (function
         (fixnum fixnum fixnum fixnum fixnum fixnum &optional (or fixnum null)))
        add-debug-point))
(defun add-debug-point (entity x y r g b &optional (a nil))
  "Adds point with screen coordinates X, Y and color R, G, B, A to debug buffer ENTITY."
  (with-debug-buffer entity ()
    (let ((current-length (length points)))
      (when (< current-length (+ count 9))
        (setf points
              (adjust-array points
                            (round (* current-length +array-growth-factor+)))))
      (setf (aref points (+ count 0)) (float x))
      (setf (aref points (+ count 1)) (float y))
      (setf (aref points (+ count 2)) 0f0) ;; z
      (setf (aref points (+ count 3)) 0f0) ;; u
      (setf (aref points (+ count 4)) 0f0) ;; v
      (setf (aref points (+ count 5)) (float r))
      (setf (aref points (+ count 6)) (float g))
      (setf (aref points (+ count 7)) (float b))
      (setf (aref points (+ count 8)) (float (or a 0)))
      (incf count 9))))

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
    (when (< size 14)
      (setf size 14))
    (with-debug-buffer entity ()
      (setf render-order order)
      (setf count 0)
      (setf points (make-array size :element-type 'single-float)))))

(defmethod system-update ((system debug-system) dt)
  (with-debug-buffers
    (setf count 0)))

(defmethod system-draw ((system debug-system) renderer)
  (with-debug-buffers
      (render
       renderer render-order
       (let ((points points)
             (count count))
         #'(lambda ()
             (cffi:with-pointer-to-vector-data (ptr points)
               (al:draw-prim ptr (cffi:null-pointer) (cffi:null-pointer) 0
                             (ceiling count 9) 0))))))
  (with-slots (font texts text-designators) system
    (unless font
      (setf font (al:create-builtin-font)))
    (loop
      with color = (al:map-rgb 255 255 255)
      for designator in (reverse text-designators)
      for text = (gethash designator texts)
      for i from 0
      unless (emptyp text)
        do (render
            renderer
            5000d0
            (let ((i i)
                  (text text))
              #'(lambda () (al:draw-text font color 0 (* i 10) 0 text)))))))

(defhandler debug-system quit (event)
  (with-slots (font) system
    (al:destroy-font font)
    (setf font nil)))

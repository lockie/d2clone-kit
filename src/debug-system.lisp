(in-package :d2clone-kit)

(defclass debug-system (system)
  ((name :initform 'debug)
   (order :initform 10)))

(defcomponent debug debug-buffer
  (render-order 0 :type fixnum)
  (count 0 :type fixnum)
  (points nil :type (simple-array single-float)))

(declaim
 (ftype (function
         (fixnum fixnum fixnum fixnum fixnum fixnum &optional (or fixnum null)))
        add-debug-point))
(defun add-debug-point (entity x y r g b &optional (a nil))
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

(declaim (ftype (function (fixnum fixnum fixnum list boolean)) add-debug-tile-rhomb))
(defun add-debug-tile-rhomb (entity x y color mark)
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

(defmethod system-load ((system debug-system))
  t)

(defmethod make-component ((system debug-system) entity &rest parameters)
  (destructuring-bind (&key (size 144) (order 1000)) parameters
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
             (al:hold-bitmap-drawing nil)
             (cffi:with-pointer-to-vector-data (ptr points)
               (al:draw-prim ptr (cffi:null-pointer) (cffi:null-pointer) 0
                             (ceiling count 9) 0)))))))

(in-package :d2clone-kit)


(defclass camera-system (system)
  ((name :initform 'camera)
   (entity :initform nil)))

(defcomponent camera camera)

(defmethod system-load ((system camera-system))
  (declare (ignore system))
  t)

(defmethod system-quit ((system camera-system))
  (setf (slot-value system 'entity) -1)
  t)

(defmethod make-component ((system camera-system) entity &rest parameters)
  (declare (ignore parameters))
  (setf (slot-value system 'entity) entity)
  nil)

(declaim (inline camera-entity))
(defun camera-entity ()
  (slot-value (system-ref 'camera) 'entity))

(defmacro with-camera (bindings &rest body)
  (with-gensyms (camera-entity)
    `(let ((,camera-entity (camera-entity)))
       (with-coordinate ,camera-entity ,bindings
         ,@body))))

(declaim
 (inline absolute->viewport)
 ;; (ftype (function (coordinate coordinate) (values fixnum fixnum)) absolute->viewport)
 )
(defun absolute->viewport (x y)
  (with-system-config-options ((display-width display-height))
    (with-screen-coordinate (camera-entity)
        (camera-x camera-y)
      (values
       (+ (- x camera-x) (ceiling display-width 2))
       (+ (- y camera-y) (ceiling display-height 2))))))

(declaim
 (inline viewport->absolute)
 )
(defun viewport->absolute (x y)
  (with-system-config-options ((display-width display-height))
    (with-screen-coordinate (camera-entity)
        (camera-x camera-y)
      (values
       (+ x camera-x (- (ceiling display-width 2)))
       (+ y camera-y (- (ceiling display-height 2)))))))

(declaim
 (inline visiblep)
 (ftype (function (fixnum fixnum &optional fixnum) boolean) visiblep))
(defun visiblep (x y &optional (delta 0))
  (with-system-config-options ((display-width display-height))
    (and
     (and (> x (- delta))
          (< x (+ delta display-width)))
     (and (> y (- delta))
          (< y (+ delta display-height))))))

(declaim
 (inline range-visible-p)
 (ftype (function (fixnum fixnum fixnum fixnum) boolean) range-visible-p))
(defun range-visible-p (x y width height)
  (with-system-config-options ((display-width display-height))
    (and
     (> x (- width))
     (< x display-width)
     (> y (- height))
     (< y display-height))))

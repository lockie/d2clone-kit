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

(defmacro with-camera (bindings &rest body)
  (with-gensyms (camera-entity)
    `(let ((,camera-entity (slot-value (system-ref 'camera) 'entity)))
       (with-point ,camera-entity ,bindings
         ,@body))))

(declaim
 (inline absolute->screen)
 (ftype (function (coordinate coordinate) (values fixnum fixnum)) absolute->screen))
(defun absolute->screen (x y)
  (with-config-options (display-width display-height)
    (with-camera (camera-x camera-y)
      (values
       (+ (- x camera-x) (ceiling display-width 2))
       (+ (- y camera-y) (ceiling display-height 2))))))

(declaim
 (inline visiblep)
 (ftype (function (fixnum fixnum &optional fixnum) boolean) visiblep))
(defun visiblep (x y &optional (delta 0))
  (with-config-options (display-width display-height)
    (with-camera (camera-x camera-y)
      (and
       (<= (abs (- camera-x x))
           (+ (ceiling display-width 2) delta))
       (<= (abs (- camera-y y))
           (+ (ceiling display-height 2) delta))))))

(declaim
 (inline range-visible-p)
 (ftype (function (fixnum fixnum fixnum fixnum) boolean) range-visible-p))
(defun range-visible-p (x y width height)
  (with-config-options (display-width display-height)
    (with-camera (camera-x camera-y)
      (let ((relative-x (- camera-x x))
            (relative-y (- camera-y y))
            (half-screen-width (ceiling display-width 2))
            (half-screen-height (ceiling display-height 2)))
        (and
         (>= relative-x (- half-screen-width))
         (< relative-x (+ width half-screen-width))
         (>= relative-y (- half-screen-height))
         (< relative-y (+ height half-screen-height)))))))


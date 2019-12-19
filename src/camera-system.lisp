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
 ;; (ftype (function (fixnum fixnum &optional fixnum) boolean) visiblep)
 )
(defun visiblep (x y &optional (delta 0))
  t)
  ;; (with-system-config-options ((display-width display-height))
  ;;   (with-camera (camera-x camera-y)
  ;;     (and
  ;;      (<= (abs (- camera-x x))
  ;;          (+ (ceiling display-width 2) delta))
  ;;      (<= (abs (- camera-y y))
  ;;          (+ (ceiling display-height 2) delta))))))

(declaim
 (inline range-visible-p)
 ;; (ftype (function (fixnum fixnum fixnum fixnum) boolean) range-visible-p)
 )
(defun range-visible-p (x y width height)
  t)


  ;; (with-system-config-options ((display-width display-height))
  ;;   (with-camera (camera-x camera-y)
  ;;     (let ((relative-x (- camera-x x))
  ;;           (relative-y (- camera-y y))
  ;;           (half-screen-width (ceiling display-width 2))
  ;;           (half-screen-height (ceiling display-height 2)))
  ;;       (and
  ;;        (>= relative-x (- half-screen-width))
  ;;        (< relative-x (+ width half-screen-width))
  ;;        (>= relative-y (- half-screen-height))
  ;;        (< relative-y (+ height half-screen-height)))))))


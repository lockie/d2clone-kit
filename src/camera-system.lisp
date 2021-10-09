(in-package :d2clone-kit)


(defsystem camera
  ((entity +invalid-entity+ :type fixnum)
   (target +invalid-entity+ :type fixnum))
  (:documentation "Handles camera entity."))

(defmethod make-component ((system camera-system) entity &rest parameters)
  (declare (ignore parameters))
  (setf (camera-system-entity system) entity)
  nil)

(declaim #-d2c-debug (inline camera-entity))
(defun camera-entity ()
  "Returns current camera entity."
  (camera-system-entity *camera-system*))

(declaim #-d2c-debug (inline camera-target))
(defun camera-target ()
  "Returns current camera target, i.e. the entity camera tracks."
  (camera-system-target *camera-system*))

(declaim #-d2c-debug (inline (setf camera-target)))
(defun (setf camera-target) (target)
  "Sets current camera target, i.e. the entity camera tracks. Set to NIL to
stop camera tracking."
  (setf (camera-system-target *camera-system*) target))

(defmacro with-camera (bindings &body body)
  "Executes BODY with current camera position bound to two symbols in BIDNINGS
list."
  (with-gensyms (camera-entity)
    `(let ((,camera-entity (camera-entity)))
       (when (entity-valid-p ,camera-entity)
         (with-coordinate ,camera-entity ,bindings
           ,@body)))))

(declaim
 #-d2c-debug (inline absolute->viewport)
 ;; (ftype (function (coordinate coordinate) (values fixnum fixnum))
 ;;  absolute->viewport)
 )
(defun absolute->viewport (x y)
  "Converts given integer absolute screen coordinates to viewport coordinates.

See VIEWPORT->ABSOLUTE"
  (with-system-config-options ((display-width display-height))
    (with-screen-coordinate (camera-entity)
        (camera-x camera-y)
      (values
       (+ (- x camera-x) (ceiling display-width 2))
       (+ (- y camera-y) (ceiling display-height 2))))))

(declaim
 #-d2c-debug (inline viewport->absolute)
 )
(defun viewport->absolute (x y)
  "Converts given integer viewport coordinates to absolute screen coordinates.

See ABSOLUTE->VIEWPORT"
  (with-system-config-options ((display-width display-height))
    (with-screen-coordinate (camera-entity)
        (camera-x camera-y)
      (values
       (+ x camera-x (- (ceiling display-width 2)))
       (+ y camera-y (- (ceiling display-height 2)))))))

(declaim
 #-d2c-debug (inline visiblep)
 (ftype (function (fixnum fixnum &optional fixnum) boolean) visiblep))
(defun visiblep (x y &optional (delta 0))
  "Returns T if point with given viewport coordinates is visible on screeen."
  (with-system-config-options ((display-width display-height))
    (and
     (> x (- delta))
     (< x (+ delta display-width))
     (> y (- delta))
     (< y (+ delta display-height)))))

(declaim
 #-d2c-debug (inline range-visible-p)
 (ftype (function (fixnum fixnum fixnum fixnum) boolean) range-visible-p))
(defun range-visible-p (x y width height)
  "Returns T if any part of rectangular range defined by given viewport
coordinates and dimensions is visible on screen."
  (with-system-config-options ((display-width display-height))
    (and
     (> x (- width))
     (< x display-width)
     (> y (- height))
     (< y display-height))))

(defmethod system-update ((system camera-system))
  (let ((target (camera-system-target system)))
    (when (entity-valid-p target)
      (with-camera (camera-x camera-y)
        (with-coordinate target ()
          (setf camera-x x
                camera-y y))))))

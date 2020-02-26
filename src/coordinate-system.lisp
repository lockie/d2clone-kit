(in-package :d2clone-kit)


(defclass coordinate-system (system)
  ((name :initform 'coordinate))
  (:documentation "Stores world coordinates."))

;; TODO : разобраться с типами
;; (deftype coordinate () `(integer ,(truncate most-negative-fixnum 256)
;;                                  ,(truncate most-positive-fixnum 256)))

(defcomponent coordinate coordinate
  (x 0d0 :type double-float)
  (y 0d0 :type double-float))

(defmethod make-component ((system coordinate-system) entity &rest parameters)
  (destructuring-bind (&key x y) parameters
    (with-coordinate entity (point-x point-y)
      (setf point-x x)
      (setf point-y y))))

(declaim
 (inline world->screen)
 (ftype (function (double-float double-float) (values fixnum fixnum)) world->screen))
(defun world->screen (x y)
  "Converts world coordinate units to screen pixel units."
  (values
   (floor (* x *tile-width*))
   (floor (* y *tile-height*) 2)))

(defmacro with-screen-coordinate (entity bindings &body body)
  "Executes BODY with ENTITY's screen pixel coordinates bound to two symbols in BINDINGS list. If BINDINGS are not set, coordinates are bound to symbols X and Y."
  (let* ((bindings (or bindings '(x y)))
         (screen-x (car bindings))
         (screen-y (cadr bindings))
         (map-x (gensym "x"))
         (map-y (gensym "y")))
    `(with-coordinate ,entity (,map-x ,map-y)
       (multiple-value-bind (,screen-x ,screen-y) (world->screen ,map-x ,map-y)
         ,@body))))

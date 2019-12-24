(in-package :d2clone-kit)


(defclass coordinate-system (system)
  ((name :initform 'coordinate)))

;; TODO : разобраться с типами
;; (deftype coordinate () `(integer ,(truncate most-negative-fixnum 256)
;;                                  ,(truncate most-positive-fixnum 256)))

(defcomponent coordinate coordinate
  (x 0 :type double-float)
  (y 0 :type double-float))  ;; XXX размерность x и y - тайлы (не пиксели!)

(defmethod system-load ((system coordinate-system))
  t)

(defmethod make-component ((system coordinate-system) entity &rest parameters)
  (destructuring-bind (&key x y) parameters
    (with-coordinate entity (point-x point-y)
      (setf point-x x)
      (setf point-y y))))

(declaim
 (inline map->screen)
 (ftype (function (double-float double-float) (values fixnum fixnum)) map->screen))
(defun map->screen (x y)
  (values
   (floor (+ (* x *tile-width*) (* (rem (abs (floor y)) 2) (floor *tile-width* 2))))
   (floor (* y *tile-height*) 2)))

(declaim
 (inline screen->map*)
 (ftype (function (fixnum fixnum) (values double-float double-float)) screen->map*))
(defun screen->map* (x y)
  (let ((tx (floor (- x (* -2 y) (floor *tile-width* 2) (* 2 *tile-height*)) *tile-width*))
        (ty (floor (+ y (/ x -2) (floor *tile-width* 2) (floor *tile-height* 2)) *tile-height*)))
    (values
     (coerce (1+ (floor (- tx ty) 2)) 'double-float)
     (coerce (+ tx ty) 'double-float))))

(declaim
 (inline screen->map)
 (ftype (function (fixnum fixnum) (values double-float double-float)) screen->map))
(defun screen->map (x y)
  (multiple-value-bind (int-map-x int-map-y) (screen->map* x y)
    (multiple-value-bind (int-x int-y) (map->screen int-map-x int-map-y)
      (let ((diff-x (- x int-x))
            (diff-y (- y int-y)))
        (values
         (+ int-map-x (/ diff-x (coerce *tile-width* 'double-float)))
         (+ int-map-y (/ diff-y (coerce *tile-height* 'double-float))))))))

(defmacro with-screen-coordinate (entity bindings &body body)
  (let* ((bindings (or bindings '(x y)))
         (screen-x (car bindings))
         (screen-y (cadr bindings))
         (map-x (gensym "x"))
         (map-y (gensym "y")))
    `(with-coordinate ,entity (,map-x ,map-y)
       (multiple-value-bind (,screen-x ,screen-y) (map->screen ,map-x ,map-y)
         ,@body))))

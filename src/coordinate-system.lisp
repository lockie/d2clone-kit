(in-package :d2clone-kit)


(defclass coordinate-system (system)
  ((name :initform 'coordinate)))

;; TODO : разобраться с типами
;; (deftype coordinate () `(integer ,(truncate most-negative-fixnum 256)
;;                                  ,(truncate most-positive-fixnum 256)))

;; TODO : float ??? for movement. or separate component of 2D/3D position
;; TODO : or maybe separate world <-> pixel conversion inline funcs/macroses
;; переобозвать в coordinate?..
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
   (the fixnum
        (+ (ceiling (* x *tile-width*))
           (ceiling (* (rem y 2) *tile-width*) 2)))
   (the fixnum
        (ceiling (* y *tile-height*) 2))))  ;; XXX - ?

(declaim
 (inline screen->map)
 (ftype (function (fixnum fixnum) (values double-float double-float)) screen->map))
(defun screen->map (x y)
  (let* ((ty (- y (ceiling x 2) *tile-height*))
         (tx (+ x ty)))
    (setf ty (ceiling (- ty) (ceiling *tile-width* 2)))
    (setf tx (1+ (ceiling tx (ceiling *tile-height*))))
    (values
     (the double-float
          (* (+ tx ty) 0.5d0))
     (the double-float
          (coerce (- tx ty) 'double-float)))))

(defmacro with-screen-coordinate (entity bindings &body body)
  (let* ((bindings (or bindings '(x y)))
         (screen-x (car bindings))
         (screen-y (cadr bindings))
         (map-x (gensym "x"))
         (map-y (gensym "y")))
    `(with-coordinate ,entity (,map-x ,map-y)
       (multiple-value-bind (,screen-x ,screen-y) (map->screen ,map-x ,map-y)
         ,@body))))

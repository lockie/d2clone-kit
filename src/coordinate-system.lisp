(in-package :d2clone-kit)


(declaim (type (integer 0 255) *tile-width* *tile-height*))
(global-vars:define-global-var *tile-width* 0)
(global-vars:define-global-var *tile-height* 0)

(defsystem coordinate
  ()
  (:documentation "Stores orthogonal world coordinates."
   :order -5))

;; TODO : deal with types
;; (deftype coordinate () `(integer ,(truncate most-negative-fixnum 256)
;;                                  ,(truncate most-positive-fixnum 256)))
;; (deftype float-coordinate () `(double-float
;;                                ,(/ most-negative-fixnum 2d0)
;;                                ,(/ most-positive-fixnum 2d0)))

(defcomponent (coordinate)
  (x nil :type double-float)
  (y nil :type double-float))

(defmethod make-component ((system coordinate-system) entity &rest parameters)
  (apply #'make-coordinate entity parameters))

(declaim
 (inline orthogonal->isometric)
 (ftype (function (double-float double-float) (values double-float double-float))
        orthogonal->isometric))
(defun orthogonal->isometric (x y)
  "Translates orthogonal coordinates X, Y into isometric coordinates."
  (values
   (- x y)
   (* (+ x y) 0.5d0)))

(declaim
 (inline isometric->orthogonal)
 (ftype (function (double-float double-float) (values double-float double-float))
        isometric->orthogonal))
(defun isometric->orthogonal (x y)
  "Translates isometric coordinates X, Y into ortogonal coordinates."
  (values
   (+ y (* 0.5d0 x))
   (- y (* 0.5d0 x))))

(declaim
 (inline isometric->orthogonal*)
 (ftype (function (double-float double-float) (values double-float double-float))
        isometric->orthogonal*))
(defun isometric->orthogonal* (x y)
  "Translates isometric coordinates X, Y into orthogonal coordinates, taking tile staggering into account."
  (let ((stagger (* 0.5d0 (rem (abs (floor y)) 2))))
    (values (+ (* 0.5d0 y) x stagger)
            (- (* 0.5d0 y) x stagger))))

(declaim
 (inline isometric->screen)
 (ftype (function (double-float double-float) (values fixnum fixnum))
        isometric->screen))
(defun isometric->screen (x y)
  "Translates isometric coordinates X, Y into screen coordinates."
  (values
   (floor (* x *tile-width*) 2)
   (floor (* y *tile-height*))))

(declaim
 (inline isometric->screen*)
 (ftype (function (double-float double-float) (values fixnum fixnum))
        isometric->screen*))
(defun isometric->screen* (x y)
  "Translates isometric coordinates X, Y into screen coordinates, taking tile staggering into account."
  (values
   (floor (+ (* x *tile-width*) (* (rem (abs (floor y)) 2) (floor *tile-width* 2))) 2)
   (floor (* y *tile-height*))))

(declaim
 (inline screen->isometric*)
 (ftype (function (fixnum fixnum) (values double-float double-float))
        screen->isometric*))
(defun screen->isometric* (x y)
  "Translates screen coordinates X, Y into isometric coordinates, taking tile staggering into account."
  (let ((iso-y (coerce (/ y *tile-height*) 'double-float)))
    (values
     (coerce
      (/ (- (* x 2) (* (rem (abs (floor iso-y)) 2) (floor *tile-width* 2))) *tile-width*)
      'double-float)
     iso-y)))

(declaim
 (inline orthogonal->screen)
 (ftype (function (double-float double-float) (values fixnum fixnum))
        orthogonal->screen))
(defun orthogonal->screen (x y)
  "Translates orthogonal coordinates X, Y into screen coordinates."
  (multiple-value-call
      #'isometric->screen
    (orthogonal->isometric x y)))

(declaim
 (inline screen->orthogonal*)
 (ftype (function (fixnum fixnum) (values double-float double-float))
        screen->orthogonal*))
(defun screen->orthogonal* (x y)
  "Translates screen coordinates X, Y into orthogonal coordinates, taking tile staggering into account."
  (multiple-value-call
      #'isometric->orthogonal
    (screen->isometric* x y)))

(defmacro with-screen-coordinate (entity bindings &body body)
  "Executes BODY with ENTITY's screen pixel coordinates bound to two symbols in BINDINGS list. If BINDINGS are not set, coordinates are bound to symbols X and Y."
  (let* ((bindings (or bindings '(x y)))
         (screen-x (car bindings))
         (screen-y (cadr bindings))
         (map-x (gensym "x"))
         (map-y (gensym "y")))
    `(with-coordinate ,entity (,map-x ,map-y)
       (multiple-value-bind (,screen-x ,screen-y) (orthogonal->screen ,map-x ,map-y)
         ,@body))))

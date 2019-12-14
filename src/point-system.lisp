(in-package :d2clone-kit)


(defclass point-system (system)
  ((name :initform 'point)))

(deftype coordinate () `(integer ,(truncate most-negative-fixnum 256)
                                 ,(truncate most-positive-fixnum 256)))

(defcomponent point point
  (x 0 :type fixnum)  ;; TODO : float ??? for movement. or separate component of 3D position
  (y 0 :type fixnum))

(defmethod system-load ((system point-system))
  t)

(defmethod make-component ((system point-system) entity &rest parameters)
  (destructuring-bind (&key x y) parameters
    (with-point entity (point-x point-y)
      (setf point-x x)
      (setf point-y y))))

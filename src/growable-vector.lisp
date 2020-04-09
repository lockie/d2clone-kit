(in-package :d2clone-kit)


(defstruct (growable-vector
            (:constructor %make-growable-vector)
            (:conc-name %growable-vector-)
            (:copier nil)
            (:predicate nil))
  "A simple vector of dynamic size."
  (vector nil :type simple-vector)
  (size nil :type array-length)
  (initial-element nil))

(defconstant +array-growth-factor+ (* 0.5d0 (1+ (sqrt 5d0))))

(declaim (inline make-growable-vector))
(defun make-growable-vector (&key (initial-element nil) (initial-allocated-size 1))
  (%make-growable-vector
   :vector (make-array initial-allocated-size :initial-element initial-element)
   :size 0
   :initial-element initial-element))

(declaim
 (inline growable-vector-ref)
 (ftype (function (growable-vector array-index) t) growable-vector-ref))
(defun growable-vector-ref (growable-vector index)
  (aref (%growable-vector-vector growable-vector) index))

(declaim
 (inline growable-vector-grow)
 (ftype (function (growable-vector array-length)) growable-vector-grow))
(defun growable-vector-grow (growable-vector new-allocated-size)
  (let ((vector (%growable-vector-vector growable-vector)))
    (when (> new-allocated-size (length vector))
      (setf (%growable-vector-vector growable-vector)
            (adjust-array
             vector
             new-allocated-size
             :initial-element (%growable-vector-initial-element growable-vector))))))

(declaim
 (inline (setf %growable-vector-ref))
 (ftype (function (t growable-vector array-index) t) (setf %growable-vector-ref)))
(defun (setf %growable-vector-ref) (value growable-vector index)
  (setf (%growable-vector-size growable-vector)
        (max (%growable-vector-size growable-vector) (1+ index))
        (aref (%growable-vector-vector growable-vector) index)
        value))

(declaim
 (inline (setf growable-vector-ref))
 (ftype (function (t growable-vector array-index) t) (setf growable-vector-ref)))
(defun (setf growable-vector-ref) (value growable-vector index)
  (let* ((vector (%growable-vector-vector growable-vector))
         (allocated-size (length vector)))
    (when (>= index allocated-size)
      (growable-vector-grow
       growable-vector
       (the array-index (round (* index +array-growth-factor+)))))
    (setf (%growable-vector-ref growable-vector index) value)))

;; (declaim
;;  (inline growable-vector-append)
;;  (ftype (function (growable-vector t)) growable-vector-append))
;; (defun growable-vector-append (growable-vector value)
;;   (setf (growable-vector-ref growable-vector (1+ (growable-vector-size growable-vector)))
;;         value))

(declaim
 (inline growable-vector-length)
 (ftype (function (growable-vector) array-length) growable-vector-length))
(defun growable-vector-length (growable-vector)
  (%growable-vector-size growable-vector))

(declaim
 (inline growable-vector-clear)
 (ftype (function (growable-vector)) growable-vector-clear))
(defun growable-vector-clear (growable-vector)
  (setf (%growable-vector-size growable-vector) 0))

(declaim
 (inline growable-vector-freeze)
 (ftype (function (growable-vector &key (:element-type symbol)) simple-array)
        growable-vector-freeze))
(defun growable-vector-freeze (growable-vector &key (element-type 't))
  (let* ((size (%growable-vector-size growable-vector))
         (result (make-array size :element-type element-type)))
    (replace
     result
     (%growable-vector-vector growable-vector))
    result))

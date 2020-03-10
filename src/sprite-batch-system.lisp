(in-package :d2clone-kit)


(defclass sprite-batch-system (system)
  ((name :initform 'sprite-batch)
   (order :initform 10))
  (:documentation "Handles ordered sprite batches."))

(defstruct sprite-batch-element
  (bitmap (cffi:null-pointer) :type cffi:foreign-pointer)
  (order 0 :type fixnum :read-only t)
  (image-x 0 :type fixnum :read-only t)
  (image-y 0 :type fixnum :read-only t)
  (screen-x 0 :type fixnum :read-only t)
  (screen-y 0 :type fixnum :read-only t)
  (width 0 :type fixnum :read-only t)
  (height 0 :type fixnum :read-only t))

(declaim
 (ftype (function (sprite-batch-element sprite-batch-element)) sprite-batch-element-less-p))
(defun sprite-batch-element-less-p (e1 e2)
  (< (sprite-batch-element-order e1)
     (sprite-batch-element-order e2)))

(eval-when (:compile-toplevel)
  (defconstant +maximum-sprite-size+ 256))

(defcomponent sprite-batch sprite-batch
  (bitmap (cffi:null-pointer) :type cffi:foreign-pointer)
  (sprite-width 0 :type (integer 0 #.+maximum-sprite-size+))
  (sprite-height 0 :type (integer 0 #.+maximum-sprite-size+))
  (columns 0 :type fixnum)
  (sprites nil :type (vector sprite-batch-element)))

(defmethod make-component ((system sprite-batch-system) entity &rest parameters)
  ;; TODO : check bitmap is not null everywhere! (i.e. file exists)
  (destructuring-bind (&key bitmap sprite-width sprite-height) parameters
    (with-sprite-batch entity (batch-bitmap sprite-w sprite-h columns sprites)
      (setf batch-bitmap bitmap
            sprite-w sprite-width
            sprite-h sprite-height
            columns (floor (al:get-bitmap-width bitmap) sprite-width)
            ;; TODO : try optimizing using simple-vector
            sprites (make-array
                     0 :element-type 'sprite-batch-element :adjustable t :fill-pointer t)))))

(declaim
 (inline add-sprite-to-batch)
 (ftype (function (fixnum fixnum fixnum fixnum fixnum fixnum)) add-sprite-to-batch))
(defun add-sprite-to-batch (entity order image-x image-y screen-x screen-y)
  "Adds sprite to sprite batch ENTITY using order ORDER, sprite coordinates
IMAGE-X, IMAGE-Y and screen coordinates SCREEN-X, SCREEN-Y.

See ADD-SPRITE-INDEX-TO-BATCH"
  (with-sprite-batch entity ()
    (vector-push-extend
     (make-sprite-batch-element
      :bitmap bitmap
      :order order
      :image-x image-x
      :image-y image-y
      :screen-x screen-x
      :screen-y screen-y
      :width sprite-width
      :height sprite-height)
     sprites)))

(declaim
 (inline add-sprite-index-to-batch)
 (ftype (function (fixnum fixnum fixnum fixnum fixnum)) add-sprite-index-to-batch))
(defun add-sprite-index-to-batch (entity order index screen-x screen-y)
  "Adds sprite to sprite batch ENTITY using order ORDER, sprite index INDEX and
screen coordinates SCREEN-X, SCREEN-Y.

See ADD-SPRITE-TO-BATCH"
  (with-sprite-batch entity ()
    (vector-push-extend
     (multiple-value-bind (q r)
         (floor index columns)
       (declare (type (integer 0 #.(truncate most-positive-fixnum +maximum-sprite-size+)) q r))
       (make-sprite-batch-element
        :bitmap bitmap
        :order order
        :image-x (* r sprite-width)
        :image-y (* q sprite-height)
        :screen-x screen-x
        :screen-y screen-y
        :width sprite-width
        :height sprite-height))
     sprites)))

(defmethod system-update ((system sprite-batch-system) dt)
  (with-sprite-batches
      (setf (fill-pointer sprites) 0)))

(defmethod system-draw ((system sprite-batch-system) renderer)
  (let ((elements (make-priority-queue #'sprite-batch-element-order)))
    (with-sprite-batches
        (priority-queue-push-many elements sprites))
    (render
     renderer
     100
     #'(lambda ()
         (al:hold-bitmap-drawing t)
         (priority-queue-traverse
          elements
          #'(lambda (element)
              (al:draw-bitmap-region
               (sprite-batch-element-bitmap element)
               (sprite-batch-element-image-x element)
               (sprite-batch-element-image-y element)
               (sprite-batch-element-width element)
               (sprite-batch-element-height element)
               (sprite-batch-element-screen-x element)
               (sprite-batch-element-screen-y element)
               0)))
         (al:hold-bitmap-drawing nil)))))

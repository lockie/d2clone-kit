(in-package :d2clone-kit)


(defsystem sprite-batch
  ()
  (:documentation "Handles ordered sprite batches."
   :order 10))

(defstruct sprite-batch-element
  (bitmap (cffi:null-pointer) :type cffi:foreign-pointer :read-only t)
  (order 0 :type double-float :read-only t)
  (image-x 0 :type fixnum :read-only t)
  (image-y 0 :type fixnum :read-only t)
  (screen-x 0 :type fixnum :read-only t)
  (screen-y 0 :type fixnum :read-only t)
  (width 0 :type fixnum :read-only t)
  (height 0 :type fixnum :read-only t))

(declaim
 (ftype (function (sprite-batch-element sprite-batch-element))
        sprite-batch-element-less-p))
(defun sprite-batch-element-less-p (e1 e2)
  (< (sprite-batch-element-order e1)
     (sprite-batch-element-order e2)))

(eval-when (:compile-toplevel)
  (defconstant +maximum-sprite-size+ 256))

(defcomponent (sprite-batch)
  (bitmap nil :type cffi:foreign-pointer)
  (sprite-width nil :type (integer 0 #.+maximum-sprite-size+))
  (sprite-height nil :type (integer 0 #.+maximum-sprite-size+))
  (columns nil :type fixnum)
  ;; TODO : try optimizing using simple-vector
  (sprites (make-array 0 :element-type 'sprite-batch-element :adjustable t
                         :fill-pointer t)
           :type (vector sprite-batch-element)))

(defmethod make-component ((system sprite-batch-system) entity &rest parameters)
  (destructuring-bind (&key bitmap sprite-width sprite-height) parameters
    (unless (find :video-bitmap (al:get-bitmap-flags bitmap))
      (log-warn "Bitmap ~a is not video bitmap, it makes no sense to batch it"
                bitmap))
    (make-sprite-batch entity
                       :bitmap bitmap
                       :sprite-width sprite-width
                       :sprite-height sprite-height
                       :columns (floor (al:get-bitmap-width bitmap)
                                       sprite-width))))

(declaim
 (inline add-sprite-to-batch)
 (ftype (function (fixnum double-float fixnum fixnum fixnum fixnum))
        add-sprite-to-batch))
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
 (ftype (function (fixnum double-float fixnum fixnum fixnum))
        add-sprite-index-to-batch))
(defun add-sprite-index-to-batch (entity order index screen-x screen-y)
  "Adds sprite to sprite batch ENTITY using order ORDER, sprite index INDEX and
screen coordinates SCREEN-X, SCREEN-Y.

See ADD-SPRITE-TO-BATCH"
  (with-sprite-batch entity ()
    (vector-push-extend
     (multiple-value-bind (q r)
         (floor index columns)
       (declare
        (type
         (integer 0 #.(truncate most-positive-fixnum +maximum-sprite-size+))
         q r))
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

(declaim
 (inline clear-sprite-batch)
 (ftype (function (fixnum)) clear-sprite-batch))
(defun clear-sprite-batch (entity)
  (with-sprite-batch entity ()
    (setf (fill-pointer sprites) 0)))

(defmethod system-draw ((system sprite-batch-system) renderer)
  (let ((elements (make-array 0)))
    (with-sprite-batches
        (setf elements (concatenate 'vector elements sprites)))
    (sort elements #'sprite-batch-element-less-p)
    (render
     renderer
     100d0
     #'(lambda ()
         (al:hold-bitmap-drawing t)
         (loop :for element :across elements :do
           (al:draw-bitmap-region
            (sprite-batch-element-bitmap element)
            (sprite-batch-element-image-x element)
            (sprite-batch-element-image-y element)
            (sprite-batch-element-width element)
            (sprite-batch-element-height element)
            (sprite-batch-element-screen-x element)
            (sprite-batch-element-screen-y element)
            0))
         (al:hold-bitmap-drawing nil)))))

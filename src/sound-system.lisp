(in-package :d2clone-kit)


(defclass sound-system (system)
  ((name :initform 'sound)
   (distance-factor :initform 0f0)
   (pan-factor :initform 0f0))
  (:documentation "Handles sounds."))

(defcomponent sound sound
  (sample-instance nil :type cffi:foreign-pointer))

(defprefab sound "ogg"
  (sample (cffi:null-pointer) :type cffi:foreign-pointer))

(defmethod make-prefab ((system sound-system) prefab-name)
  (make-sound-prefab :sample (al:load-sample (prefab-path system prefab-name))))

(declaim
 (inline set-sound-position)
 (ftype (function (fixnum)) set-sound-position))
(defun set-sound-position (entity)
  ;; TODO : use al_set_sample_instance_channel_matrix for 5.1 sound?..
  (with-sound entity ()
    (with-coordinate entity ()
      (with-camera (camera-x camera-y)
        (with-slots (distance-factor pan-factor) (system-ref 'sound)
          (al:set-sample-instance-gain
           sample-instance
           (/ 1f0 (exp (* distance-factor
                          (euclidean-distance x y camera-x camera-y)))))
          (al:set-sample-instance-pan
           sample-instance
           (clamp (* pan-factor (- x camera-x)) -1f0 1f0)))))))

(defmethod make-prefab-component ((system sound-system) entity prefab parameters)
  ;; TODO checking that component already exists in entity?..
  (with-sound entity ()
    (setf sample-instance (al::create-sample-instance (sound-prefab-sample prefab)))
    (al:attach-sample-instance-to-mixer sample-instance (al:get-default-mixer))
    (al:set-sample-instance-playmode sample-instance :once)
    (with-slots (distance-factor pan-factor) system
      (when (zerop distance-factor)
        (with-system-config-options ((display-width display-height))
          (setf distance-factor
                (coerce
                 (/ 2d0
                    (euclidean-distance
                     0d0 0d0
                     (coerce (/ display-width 2 *tile-width*) 'double-float)
                     (coerce (/ display-height 2 *tile-height*) 'double-float)))
                 'single-float))
          (setf pan-factor (/ 1f0 (/ display-width *tile-width* 2))))))
    (set-sound-position entity)
    (al:set-sample-instance-playing sample-instance t)))

(defmethod make-component ((system sound-system) entity &rest parameters)
  (declare (ignore system entity parameters))
  nil)

(defmethod system-update ((system sound-system) dt)
  (with-sounds
      (if (al:get-sample-instance-playing sample-instance)
          (set-sound-position entity)
          (delete-component system entity))))

(defhandler sound-system sprite-stance-changed (event entity stance)
  (with-sprite entity ()
    (maphash
     #'(lambda (layer toggled)
         (when toggled
           (let ((sound-name (make-keyword
                              (format-symbol nil "~a-~a-~a" prefab-name layer stance))))
             (when-let (sound-prefab (prefab system sound-name))
               (make-component system entity :prefab sound-name)))))
     layers-toggled)))

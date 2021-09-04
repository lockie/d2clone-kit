(in-package :d2clone-kit)


(defsystem loading-screen
  ((ui-entity +invalid-entity+ :type fixnum))
  (:documentation "Handles loading screen."
   :order 20))

(defun lightweight-game-loop-step ()
  (system-update *ui-system*)
  (al:clear-to-color (al:map-rgb 0 0 0))
  (nk:allegro-render)
  (al:flip-display))

(declaim
 (ftype (function (double-float)) set-loading-screen-progress))
(defun set-loading-screen-progress (progress)
  (with-ui (loading-screen-system-ui-entity *loading-screen-system*) ()
    (setf (gethash :progress parameters) progress))
  (lightweight-game-loop-step))

(declaim
 (ftype (function (string)) set-loading-screen-text))
(defun set-loading-screen-text (message)
  (with-ui (loading-screen-system-ui-entity *loading-screen-system*) ()
    (setf (gethash :message parameters) message))
  (lightweight-game-loop-step))

(defmethod system-create :after ((system loading-screen-system))
  (with-system-slots ((ui-entity)
                      :of loading-screen-system :instance system :read-only nil)
    (setf ui-entity (make-object '((:ui :prefab :loading :on t))))))

(defun toggle-loading-screen (&optional (on t))
  (toggle-ui (loading-screen-system-ui-entity *loading-screen-system*) on))

(defmethod system-finalize ((system loading-screen-system))
  (with-system-slots ((ui-entity) :of loading-screen-system :instance system)
    (when (entity-valid-p ui-entity)
      (delete-entity ui-entity))))

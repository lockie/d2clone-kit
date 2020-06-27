(in-package :d2clone-kit)


(defunl make-entity ()
  "Allocates new entity."
  (if (emptyp *deleted-entities*)
      (let ((res *entities-count*))
        (incf *entities-count*)
        (when (= *entities-count* *entities-allocated*)
          (setf *entities-allocated* (round (* *entities-allocated* +array-growth-factor+)))
          (log-debug "Adjusting component allocated size to ~a" *entities-allocated*)
          (with-systems system
            (system-adjust-components system *entities-allocated*)))
        res)
      (vector-pop *deleted-entities*)))

(defun delete-entity (entity)
  "Deletes entity ENTITY."
  (issue entity-deleted :entity entity)
  (loop :for system :being :the :hash-value :of *systems*
        :when (has-component-p system entity)
        :do (delete-component system entity))
  (vector-push-extend entity *deleted-entities*))

(defconstant +invalid-entity+ -1 "The invalid entity.")

(declaim (inline entity-valid-p) (ftype (function (fixnum) boolean) entity-valid-p))
(defun entity-valid-p (entity)
  "Return T if entity is valid."
  (not (minusp entity)))

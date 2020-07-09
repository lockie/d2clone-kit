(in-package :d2clone-kit)


(declaim (type array-length *entities-count*))
(global-vars:define-global-var *entities-count* 0)

(declaim (type array-length *entities-allocated*))
(global-vars:define-global-var *entities-allocated* 144)

(declaim (type (vector fixnum) *deleted-entities*))
(global-vars:define-global-var *deleted-entities*
    (make-array 0 :element-type 'fixnum :adjustable t :fill-pointer t))

(declaim (type hash-table *entities-children*))
(global-vars:define-global-var *entities-children* (make-hash-table))

(defunl make-entity (&optional parent)
  "Allocates new entity. When PARENT is set, deleting parent entity automatically deletes it.

See DELETE-ENTITY"
  (let ((new-entity
          (if (emptyp *deleted-entities*)
              (let ((res *entities-count*))
                (incf *entities-count*)
                (when (= *entities-count* *entities-allocated*)
                  (setf *entities-allocated* (round (* *entities-allocated* +array-growth-factor+)))
                  (log-debug "Adjusting component allocated size to ~a" *entities-allocated*)
                  (with-systems system
                    (system-adjust-components system *entities-allocated*)))
                res)
              (vector-pop *deleted-entities*))))
    (when parent
      (setf (gethash parent *entities-children*)
            (push new-entity (gethash parent *entities-children* nil))))
    new-entity))

(defun delete-child (parent child)
  "Deletes relationship between PARENT and CHILD.

See MAKE-ENTITY"
  (setf (gethash parent *entities-children*)
        (delete child (the list (gethash parent *entities-children* nil)))))

(defun delete-entity (entity)
  "Deletes entity ENTITY. Do NOT call this when entity has parent (call DELETE-CHILD first).

See MAKE-ENTITY
See DELETE-CHILD"
  (dolist (child (gethash entity *entities-children* nil))
    (delete-entity child))
  (remhash entity *entities-children*)
  (issue entity-deleted :entity entity)
  (with-systems system
    (when (has-component-p system entity)
      (delete-component system entity)))
  (vector-push-extend entity *deleted-entities*))

(defconstant +invalid-entity+ -1 "The invalid entity.")

(declaim (inline entity-valid-p) (ftype (function (fixnum) boolean) entity-valid-p))
(defun entity-valid-p (entity)
  "Return T if entity is valid."
  (not (minusp entity)))

(defun finalize-entities ()
  (setf *entities-count* 0
        *entities-allocated* 144)
  (clrhash *entities-children*)
  (setf (fill-pointer *deleted-entities*) 0))

(declaim (type fixnum *session-entity*))
(global-vars:define-global-var *session-entity* +invalid-entity+)

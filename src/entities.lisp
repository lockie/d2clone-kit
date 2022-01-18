(in-package :d2clone-kit)


;; TODO : add type alias entity -> fixnum

(defconstant +invalid-entity+ -1 "The invalid entity.")

(declaim (type array-length *entities-count*))
(global-vars:define-global-var *entities-count* 0)

(declaim (type array-length *entities-allocated*))
(global-vars:define-global-var *entities-allocated* 144)

(declaim (type growable-vector *deleted-entities*))
(global-vars:define-global-var *deleted-entities*
    (make-growable-vector :initial-element +invalid-entity+))

(declaim (type hash-table *entities-children*))
(global-vars:define-global-var *entities-children* (make-hash-table))

(declaim (type growable-vector *current-action*))
(global-vars:define-global-var* *current-action*
    (make-growable-vector :initial-element +invalid-index+
                          :initial-allocated-size *entities-allocated*))

(defunl make-entity (&optional parent)
  "Allocates new entity. When PARENT is set, deleting parent entity
automatically deletes it.

See DELETE-ENTITY"
  (let ((new-entity
          (if (growable-vector-emptyp *deleted-entities*)
              (let ((res *entities-count*))
                (incf *entities-count*)
                (when (= *entities-count* *entities-allocated*)
                  (setf *entities-allocated*
                        (round (* *entities-allocated* +array-growth-factor+)))
                  (log-debug "Adjusting component allocated size to ~a"
                             *entities-allocated*)
                  (with-systems system
                    (system-adjust-components system *entities-allocated*))
                  ;; TODO : event for storage growth (to reduce tight coupling
                  ;; with ECS)
                  (growable-vector-grow *current-action* *entities-allocated*))
                res)
              (growable-vector-pop *deleted-entities* 0))))
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
  "Deletes entity ENTITY. Do NOT call this when entity has parent (call
DELETE-CHILD first).

See MAKE-ENTITY
See DELETE-CHILD"
  (dolist (child (gethash entity *entities-children* nil))
    (delete-entity child))
  (remhash entity *entities-children*)
  (delete-entity-actions entity)
  (issue (entity-deleted :async nil) :entity entity)
  (with-systems system
    (when (%has-component-p system entity)
      (delete-component system entity)))
  (growable-vector-push *deleted-entities* entity))

(declaim
 #-d2c-debug (inline entity-valid-p)
 (ftype (function (fixnum) boolean) entity-valid-p))
(defun entity-valid-p (entity)
  "Return T if entity is valid."
  (not (minusp entity)))

(defun finalize-entities ()
  (setf *entities-count* 0
        *entities-allocated* 144
        ;; TODO : move to finalize-actions
        *current-action* (make-growable-vector
                          :initial-element +invalid-index+
                          :initial-allocated-size *entities-allocated*))
  (clrhash *entities-children*)
  (growable-vector-clear *deleted-entities*))

(declaim (type fixnum *session-entity*))
(global-vars:define-global-var *session-entity* +invalid-entity+)

(declaim (ftype (function (fixnum &optional stream fixnum)) dump-entities))
(defun dump-entities (root &optional (stream *standard-output*) (offset 0))
  "Dumps entities tree starting from ROOT node to the STREAM."
  (let ((components (loop :for system :being :the :hash-key
                          :using (hash-value system-instance) :of *systems*
                          :when (%has-component-p system-instance root)
                          :collect system)))
    (format stream "~vT~d[~{~a~^  ~}]~%" offset root components)
    (dolist (child (reverse (gethash root *entities-children*)))
      (dump-entities child stream (+ offset 4)))))

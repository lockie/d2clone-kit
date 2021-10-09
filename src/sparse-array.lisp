(in-package :d2clone-kit)


(defconstant +invalid-index+ -2 "Invalid index marker for SPARSE-ARRAY-INDEX.")

(declaim
 #-d2c-debug (inline index-valid-p)
 (ftype (function (fixnum) boolean) index-valid-p))
(defun index-valid-p (index)
  "Return T if index is valid.

See +INVALID-INDEX+"
  (not (minusp index)))

(defstruct (sparse-array-index
            (:constructor %make-sparse-array-index)
            (:conc-name %sparse-array-index-)
            (:copier nil)
            (:predicate nil))
  "A sparse array index shared between several growable vectors."
  (indices nil :type growable-vector)
  (count 0 :type array-length)
  (deleted-indices (make-growable-vector :initial-element +invalid-index+)
   :type growable-vector))

(defun make-sparse-array-index (&key (initial-allocated-size 1))
  "Creates new sparse array index."
  (%make-sparse-array-index
   :indices (make-growable-vector
             :initial-element +invalid-index+
             :initial-allocated-size initial-allocated-size)))

(declaim
 #-d2c-debug (inline sparse-array-index-grow)
 (ftype (function (sparse-array-index array-length)) sparse-array-index-grow))
(defun sparse-array-index-grow (sparse-array-index new-allocated-size)
  "Adjusts SPARSE-ARRAY-INDEX to have allocated size of NEW-ALLOCATED-SIZE."
  (growable-vector-grow (%sparse-array-index-indices sparse-array-index)
                        new-allocated-size))

(declaim
 #-d2c-debug (inline sparse-array-index-ref)
 (ftype (function (sparse-array-index array-index) fixnum)
        sparse-array-index-ref))
(defun sparse-array-index-ref (sparse-array-index subscript)
  "Access SPARSE-ARRAY-INDEX by SUBSCRIPT."
  (growable-vector-ref (%sparse-array-index-indices sparse-array-index)
                       subscript))

(declaim
 #-d2c-debug (inline sparse-array-index-push)
 (ftype (function (sparse-array-index array-index) array-index)
        sparse-array-index-push))
(defun sparse-array-index-push (sparse-array-index subscript)
  "Appends new element denoted by SUBSCRIPT to the end of SPARSE-ARRAY-INDEX."
  (let ((deleted-indices
          (%sparse-array-index-deleted-indices sparse-array-index)))
    (setf (growable-vector-ref* (%sparse-array-index-indices sparse-array-index)
                                subscript)
          (if (zerop (growable-vector-length deleted-indices))
              (prog1 (%sparse-array-index-count sparse-array-index)
                (incf (%sparse-array-index-count sparse-array-index)))
              (growable-vector-pop deleted-indices 0)))))

(declaim
 #-d2c-debug (inline sparse-array-index-delete)
 (ftype (function (sparse-array-index array-index)) sparse-array-index-delete))
(defun sparse-array-index-delete (sparse-array-index subscript)
  "Marks element denoted by SUBSCRIPT as deleted in SPARSE-ARRAY-INDEX."
  (growable-vector-push
   (%sparse-array-index-deleted-indices sparse-array-index)
   (growable-vector-ref (%sparse-array-index-indices sparse-array-index)
                        subscript))
  (setf (growable-vector-ref (%sparse-array-index-indices sparse-array-index)
                             subscript)
        +invalid-index+))

(defmacro do-sparse-array ((subscript index sparse-array-index) &body body)
  "Executes BODY several times with SUBSCRIPT and INDEX variables set to
corresponding values in SPARSE-ARRAY-INDEX."
  (with-gensyms (indices)
    `(let ((,indices (%sparse-array-index-indices ,sparse-array-index)))
       (loop :for ,subscript :from 0 :to (growable-vector-length ,indices)
             :for ,index := (the fixnum
                                 (growable-vector-ref ,indices ,subscript))
             :when (index-valid-p ,index)
               :do ,@body))))

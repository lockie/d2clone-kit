(in-package :d2clone-kit)


(defstruct (sparse-matrix
            (:constructor %make-sparse-matrix)
            (:copier nil)
            (:predicate nil))
  "A high-watermark sparse matrix, implemented as a dictionary of keys."
  (indices nil :type hash-table)
  (array nil :type growable-vector)
  (deleted-indices nil :type #+ccl vector #-ccl (vector array-index)))

(declaim (ftype (function () sparse-matrix)
                make-sparse-matrix))
(defun make-sparse-matrix ()
  "Creates new sparse matrix."
  (let ((initial-size 8))
    (%make-sparse-matrix
     :indices (make-hash-table :test 'equal :size initial-size)
     :array (make-growable-vector :initial-allocated-size initial-size)
     :deleted-indices (make-array 0
                                  :element-type 'array-index
                                  :adjustable t :fill-pointer t))))

(declaim
 (inline sparse-matrix-ref)
 (ftype (function (sparse-matrix cons)) sparse-matrix-ref))
(defun sparse-matrix-ref (sparse-matrix subscripts)
  "Returns SPARSE-MATRIX element identified by list SUBSCRIPTS or NIL if there's no such element."
  (if-let (index (gethash subscripts (sparse-matrix-indices sparse-matrix)))
    (growable-vector-ref (sparse-matrix-array sparse-matrix) index)
    nil))

(declaim
 (inline (setf sparse-matrix-ref))
 (ftype (function (t sparse-matrix cons) t) (setf sparse-matrix-ref)))
(defun (setf sparse-matrix-ref) (value sparse-matrix subscripts)
  "Sets SPARSE-MATRIX element identified by list SUBSCRIPTS to VALUE.
To remove an element from sparse matrix, use SPARSE-MATRIX-REMOVE.

See SPARSE-MATRIX-REMOVE"
  (if-let (index (gethash subscripts (sparse-matrix-indices sparse-matrix)))
    (setf (%growable-vector-ref (sparse-matrix-array sparse-matrix) index) value)
    (let ((array (sparse-matrix-array sparse-matrix)))
      (if (emptyp (sparse-matrix-deleted-indices sparse-matrix))
          (let ((index (growable-vector-length array)))
            (setf (gethash subscripts (sparse-matrix-indices sparse-matrix)) index
                  (growable-vector-ref array index) value))
          ;; TODO : test deletion!
          (let ((index (vector-pop (sparse-matrix-deleted-indices sparse-matrix))))
            (setf (gethash subscripts (sparse-matrix-indices sparse-matrix)) index
                  (%growable-vector-ref array index) value))))))

(declaim
 (inline sparse-matrix-remove)
 (ftype (function (sparse-matrix cons)) sparse-matrix-remove))
(defun sparse-matrix-remove (sparse-matrix subscripts)
  "Removes element identified by list SUBSCRIPTS from SPARSE-MATRIX.
Returns number of deleted elements or NIL if there's no such element in matrix."
  (when-let (index (gethash subscripts (sparse-matrix-indices sparse-matrix)))
    (remhash subscripts (sparse-matrix-indices sparse-matrix))
    (vector-push-extend index (sparse-matrix-deleted-indices sparse-matrix))))

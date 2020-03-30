(in-package :d2clone-kit)


(defstruct (sparse-matrix
            (:constructor %make-sparse-matrix)
            (:copier nil)
            (:predicate nil))
  "A high-watermark sparse matrix, implemented as a dictionary of keys."
  (indices nil :type hash-table)
  (array nil :type simple-vector)
  (length nil :type array-length)
  (deleted-indices nil :type #+ccl vector #-ccl (vector array-index)))

(declaim (ftype (function (&key (:element-type t)) sparse-matrix)
                make-sparse-matrix))
(defun make-sparse-matrix (&key (element-type 't))
  "Creates sparse matrix holding elements of ELEMENT-TYPE."
  (let ((initial-size 8))
    (%make-sparse-matrix
     :indices (make-hash-table :test 'equal :size initial-size)
     :array (make-array initial-size
                        :element-type element-type
                        :initial-element nil)
     :length 0
     :deleted-indices (make-array 0
                                  :element-type 'array-index
                                  :adjustable t :fill-pointer t))))

(declaim
 (inline sparse-matrix-ref)
 (ftype (function (sparse-matrix list)) sparse-matrix-ref))
(defun sparse-matrix-ref (sparse-matrix subscripts)
  "Returns SPARSE-MATRIX element identified by list SUBSCRIPTS or NIL if there's no such element."
  (if-let (index (gethash subscripts (sparse-matrix-indices sparse-matrix)))
    (aref (sparse-matrix-array sparse-matrix) index)
    nil))

(declaim
 (inline (setf sparse-matrix-ref))
 (ftype (function (t sparse-matrix list) t) (setf sparse-matrix-ref)))
(defun (setf sparse-matrix-ref) (value sparse-matrix subscripts)
  "Sets SPARSE-MATRIX element identified by list SUBSCRIPTS to VALUE.
To remove an element from sparse matrix, use SPARSE-MATRIX-REMOVE.

See SPARSE-MATRIX-REMOVE"
  (if-let (index (gethash subscripts (sparse-matrix-indices sparse-matrix)))
    (setf (aref (sparse-matrix-array sparse-matrix) index) value)
    (let* ((array (sparse-matrix-array sparse-matrix))
           (size (length array))
           (length (sparse-matrix-length sparse-matrix))
           (index length))
      (if (< index size)
          (incf (sparse-matrix-length sparse-matrix))
          (if (emptyp (sparse-matrix-deleted-indices sparse-matrix))
              (setf size (the array-length (round (* size +array-growth-factor+)))
                    array (adjust-array array size :initial-element nil)
                    (sparse-matrix-array sparse-matrix) array
                    (sparse-matrix-length sparse-matrix) (1+ length))
              ;; TODO : test deletion!
              (setf index (vector-pop (sparse-matrix-deleted-indices sparse-matrix)))))
      (setf (gethash subscripts (sparse-matrix-indices sparse-matrix)) index)
      (setf (aref array index) value))))

(declaim
 (inline sparse-matrix-remove)
 (ftype (function (sparse-matrix list)) sparse-matrix-remove))
(defun sparse-matrix-remove (sparse-matrix subscripts)
  "Removes element identified by list SUBSCRIPTS from SPARSE-MATRIX.
Returns number of deleted elements or NIL if there's no such element in matrix."
  (when-let (index (gethash subscripts (sparse-matrix-indices sparse-matrix)))
    (remhash subscripts (sparse-matrix-indices sparse-matrix))
    (vector-push-extend index (sparse-matrix-deleted-indices sparse-matrix))))

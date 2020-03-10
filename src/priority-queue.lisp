(in-package :d2clone-kit)

(defstruct (priority-queue
            (:constructor %make-priority-queue)
            (:copier nil)
            (:predicate nil))
  (array nil :type simple-vector)
  (key nil :type (function (t) fixnum) :read-only t))

(defun make-priority-queue (key-fn)
  "Creates priority queue using key extraction function KEY-FN."
  (%make-priority-queue :array (make-array 0) :key key-fn))

(declaim
 (inline binary-search)
 (ftype (function ((function (t) fixnum) fixnum simple-vector) array-index)
        binary-search))
(defun binary-search (key-fn key array)
  (if (zerop (length array))
      0
      (flet
          ((mid (first last)
             (declare (fixnum first last))
             (the array-index (+ first (truncate (- last first) 2)))))
        (declare (inline mid))
        (do* ((l 0)
              (u (1- (length array)))
              (m (mid l u) (mid l u)))
             ((> l u) l)
          (if (< (funcall key-fn (aref array m)) key)
              (setf l (1+ m))
              (setf u (1- m)))))))

(declaim
 (inline priority-queue-push)
 (ftype (function (priority-queue t)) priority-queue-push))
(defun priority-queue-push (queue element)
  "Adds element ELEMENT to priority queue QUEUE."
  (let* ((array (priority-queue-array queue))
         (key-fn (priority-queue-key queue))
         (position (binary-search key-fn (funcall key-fn element) array)))
    (setf (priority-queue-array queue)
          (adjust-array array (1+ (length array))))
    (let ((array (priority-queue-array queue)))
        (replace
         array
         array
         :start1 (1+ position)
         :end1 (length array)
         :start2 position)
      (setf (aref array position) element)))
  nil)

(declaim
 (inline priority-queue-push-many)
 (ftype (function (priority-queue vector)) priority-queue-push-many))
(defun priority-queue-push-many (queue elements)
  "Adds elements from vector ELEMENTS to priority queue QUEUE."
  (let* ((array (priority-queue-array queue))
         (old-length (length array)))
    (setf (priority-queue-array queue)
          (adjust-array array (+ old-length (length elements))))
    (let ((array (priority-queue-array queue)))
      (replace
       array
       elements
       :start1 old-length)
      ;; TODO : try optimizing assuming elements vector is sorted
      (sort
       array
       #'(lambda (a b)
           (declare (fixnum a b))
           (< a b))
       :key (priority-queue-key queue))))
  nil)

(declaim
 (inline priority-queue-traverse)
 (ftype (function (priority-queue (function (t)))) priority-queue-traverse))
(defun priority-queue-traverse (queue fn)
  "Calls one argument function FN on elements of priority queue QUEUE
in appropriate order."
  (loop for element across (priority-queue-array queue)
        do (funcall fn element)))

(declaim
 (inline priority-queue-clear)
 (ftype (function (priority-queue)) priority-queue-clear))
(defun priority-queue-clear (queue)
  "Clears priority queue QUEUE."
  (setf (priority-queue-array queue)
        (make-array 0))
  nil)

(in-package :d2clone-kit)

(defstruct (priority-queue
            (:constructor %make-priority-queue)
            (:copier nil)
            (:predicate nil))
  "A simple priority queue with DOUBLE-FLOAT priorities."
  (array nil :type simple-vector)
  (key nil :type (function (t) double-float) :read-only t))

(defun make-priority-queue (key-fn)
  "Creates priority queue using key extraction function KEY-FN.

Note: keys are expected to be DOUBLE-FLOATs."
  (%make-priority-queue :array (make-array 0) :key key-fn))

(declaim
 (inline binary-search)
 (ftype (function ((function (t) double-float) double-float simple-vector) array-index)
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
          (if (> (funcall key-fn (aref array m)) key)
              (setf l (1+ m))
              (setf u (1- m)))))))

(declaim
 (inline priority-queue-find)
 (ftype (function (priority-queue t)) priority-queue-find))
(defun priority-queue-find (queue element)
  "Finds ELEMENT's position in QUEUE. Returns NIL if there's no such element.
O(log N) complexity."
  (let* ((array (priority-queue-array queue))
         (key-fn (priority-queue-key queue))
         (position (binary-search key-fn (funcall key-fn element) array)))
    (when (and (< position (length array)) (equal element (aref array position)))
      position)))

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
  "Adds elements from vector ELEMENTS to priority queue QUEUE.

A bit more performance-friendly than calling PRIORITY-QUEUE-PUSH many times
(but complexity is still O(N log N))."
  (unless (length= 0 elements)
    ;; TODO : also optimize (length= 1 elements) case?..
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
             (declare (double-float a b))
             (> a b))
         :key (priority-queue-key queue))))
    nil))

(declaim
 (inline priority-queue-traverse)
 (ftype (function (priority-queue (function (t)))) priority-queue-traverse))
(defun priority-queue-traverse (queue fn)
  "Calls one argument function FN on elements of priority queue QUEUE
in appropriate order."
  (let ((array (priority-queue-array queue)))
    (loop
      :for i :from (1- (length array)) :downto 0
      :for element := (aref array i)
      :do (funcall fn element))))

(declaim
 (inline simple-vector-peek)
 (ftype (function (simple-vector) t) simple-vector-peek))
(defun simple-vector-peek (array)
  (aref array (1- (length array))))

(declaim
 (inline simple-vector-pop)
 (ftype (function (simple-vector) (values t simple-vector)) simple-vector-pop))
(defun simple-vector-pop (array)
  (let* ((last-index (1- (length array)))
         (element (aref array last-index))
         (new-array (adjust-array array last-index)))
    (values element new-array)))

(declaim
 (inline priority-queue-pop)
 (ftype (function (priority-queue)) priority-queue-pop))
(defun priority-queue-pop (queue)
  "Removes and returns the first (priority-wise) element in QUEUE."
  (multiple-value-bind (element new-array)
      (simple-vector-pop (priority-queue-array queue))
    (setf (priority-queue-array queue) new-array)
    element))

(declaim
 (inline priority-queue-remove)
 (ftype (function (priority-queue array-index)) priority-queue-remove))
(defun priority-queue-remove (queue index)
  "Removes element from QUEUE denoted by INDEX."
  (let ((array (priority-queue-array queue)))
    (replace (priority-queue-array queue) array :start1 index :start2 (1+ index))
    (setf (priority-queue-array queue)
          (adjust-array (priority-queue-array queue) (1- (length array)))))
  nil)

(declaim
 (inline priority-queue-clear)
 (ftype (function (priority-queue)) priority-queue-clear))
(defun priority-queue-clear (queue)
  "Clears priority queue QUEUE."
  (setf (priority-queue-array queue)
        (make-array 0))
  nil)

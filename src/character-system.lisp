(in-package :d2clone-kit)


(defclass character-system (system)
  ((name :initform 'character))
  (:documentation "Handles sprites that are able to walk and collide with obstacles."))

(defcomponent character character
  (speed nil :type double-float)
  (target-x nil :type double-float)
  (target-y nil :type double-float)
  (path nil :type simple-vector)
  (debug-entity -1 :type fixnum))

(defmethod make-component ((system character-system) entity &rest parameters)
  (destructuring-bind (&key (speed 2d0) target-x target-y) parameters
    (with-character entity (s x y path debug-entity)
      (setf s speed)
      (setf x target-x)
      (setf y target-y)
      (setf path (make-array 0))
      (with-system-config-options ((debug-path))
        (when debug-path
          (setf debug-entity (make-entity))
          (make-component (system-ref 'debug) debug-entity :order 1050d0))))))

(eval-when (:compile-toplevel)
  (defconstant +neighbours+ '((0d0 -2d0)
                              (0.5d0 -1d0)
                              (1d0 0d0)
                              (-0.5d0 -1d0)
                              (0.5d0 1d0)
                              (-1d0 0d0)
                              (-0.5d0 1d0)
                              (0d0 2d0))))

(declaim
 (inline euclidean-distance)
 (ftype (function (double-float double-float double-float double-float) double-float)
        euclidean-distance))
(defun euclidean-distance (x1 y1 x2 y2)
  (flet ((sqr (x) (the double-float (* x x))))
    (let ((ortho-x (- x2 x1))
          (ortho-y (* 0.5d0 (- y2 y1))))
      (sqrt
       (+
        (sqr (+ ortho-y ortho-x))
        (sqr (- ortho-y ortho-x)))))))

(defstruct (path-node
            (:constructor make-path-node (x y))
            (:copier nil)
            (:predicate nil))
  (cost 0d0 :type double-float)
  (x 0d0 :type double-float :read-only t)
  (y 0d0 :type double-float :read-only t)
  (parent nil :type (or path-node null)))

(declaim (inline make-path-node))

(declaim
 (inline path-node-equal)
 (ftype (function (path-node path-node) boolean) path-node-equal))
(defun path-node-equal (node1 node2)
  (and (= (path-node-x node1)
          (path-node-x node2))
       (= (path-node-y node1)
          (path-node-y node2))))

(declaim
 (inline remove-nth)
 (ftype (function (list array-index) list) remove-nth))
(defun remove-nth (list n)
  (if (zerop n)
      (cdr list)
      (loop
        :for i :from 0 :below array-dimension-limit
        :for element := list :then (cdr element)
        :do (when (= i (1- n))
              (setf (cdr element) (cddr element))
              (loop-finish))
        :finally (return list))))

;; TODO : optimize
;; TODO : penalize turns?..
;; TODO : separate thread?..
(declaim (ftype (function (double-float double-float double-float double-float)) a*))
(defun a* (start-x start-y goal-x goal-y)
  "Runs A* algorithm to find path from point START-X, START-Y to GOAL-X, GOAL-Y.
Returns simple array containing conses of x and y path node world coordinates.

Note: if goal point is not walkable, this function will stuck."
  (declare (optimize (speed 3) (safety 0)))
  (multiple-value-bind (goal-col goal-row)
      (tile-index goal-x goal-y)
    (multiple-value-bind (initial-x initial-y)
        (multiple-value-bind (i-x i-y)
            (tile-index start-x start-y)
          (tile-pos (coerce i-x 'double-float) (coerce i-y 'double-float)))
      (let ((open (make-priority-queue #'path-node-cost))
            (closed nil)
            (start-node (make-path-node initial-x initial-y)))
        (priority-queue-push open start-node)
        (let ((goal-node
                (loop :for current := (priority-queue-pop open)
                      :until (multiple-value-bind (current-x current-y)
                                 (tile-index (path-node-x current)
                                             (path-node-y current))
                               (and (= goal-col current-x) (= goal-row current-y)))
                      :do (push current closed)
                          (dolist (neighbour +neighbours+)
                            (multiple-value-bind (neighbour-x neighbour-y)
                                (tile-pos (+ (path-node-x current)
                                             (the double-float (car neighbour)))
                                          (+ (path-node-y current)
                                             (the double-float (cadr neighbour))))
                              (let* ((cost (+ (euclidean-distance
                                               start-x start-y
                                               (path-node-x current) (path-node-y current))
                                              1d0
                                              (if (multiple-value-call #'collidesp
                                                    (tile-index neighbour-x neighbour-y))
                                                  10000d0 0d0)))
                                     (neighbour-cost (euclidean-distance
                                                      start-x start-y
                                                      neighbour-x neighbour-y))
                                     (neighbour-node (make-path-node neighbour-x neighbour-y))
                                     (neighbour-open-index (priority-queue-find open neighbour-node)))
                                (if (and (< cost neighbour-cost) neighbour-open-index)
                                    ;; new path is better
                                    (priority-queue-remove open neighbour-open-index)
                                    (let ((neighbour-closed-index
                                            (position neighbour-node closed :test #'path-node-equal)))
                                      (cond
                                        ((and neighbour-closed-index (< cost neighbour-cost))
                                         ;; XXX this does happen with the chosen metric.
                                         (setf closed (remove-nth closed neighbour-closed-index)))
                                        ((and (not neighbour-open-index)
                                              (not neighbour-closed-index))
                                         (setf (path-node-cost neighbour-node)
                                               (+ cost (euclidean-distance neighbour-x neighbour-y
                                                                           goal-x goal-y))
                                               (path-node-parent neighbour-node)
                                               current)
                                         (priority-queue-push open neighbour-node))))))))
                      :finally (return current))))
          (loop
            :with result := (make-array 0 :element-type 'cons :adjustable t :fill-pointer t)
            :for node := goal-node :then (path-node-parent node)
            :until (eq start-node node)
            :do (vector-push-extend (cons (path-node-x node) (path-node-y node)) result)
            :finally (return (make-array (length result)
                                         :element-type 'cons
                                         :initial-contents result))))))))

(declaim (inline follow-path) (ftype (function (fixnum)) follow-path))
(defun follow-path (character-entity)
  (with-coordinate character-entity ()
    (with-sprite character-entity ()
      (with-character character-entity ()
        (multiple-value-bind (target new-path)
            (simple-vector-pop path)
          (setf path new-path
                target-x (car target)
                target-y (cdr target)
                angle (atan (* (- target-y y) 0.5d0) (- target-x x))))))))

;; TODO : some sort of generic SoA class/macro with getter/setter functions
(declaim
 (ftype (function (fixnum double-float double-float)) set-character-target))
(defun set-character-target (entity new-target-x new-target-y)
  "Sets character ENTITY new movement target to NEW-TARGET-X, NEW-TARGET-Y."
  (with-coordinate entity ()
    (let ((new-target-x new-target-x)
          (new-target-y new-target-y))
      (loop  ;; make sure new target is walkable
             :with dx := (- new-target-x x) :and dy := (* (- new-target-y y) 0.5d0)
             :with a := (atan dy dx) :and r := (sqrt (+ (* dx dx) (* dy dy)))
             :for (col row) := (multiple-value-list (tile-index new-target-x new-target-y))
             :while (collidesp col row)
             :do (setf r (- r 0.5d0)
                       new-target-x (+ x (* r (cos a)))
                       new-target-y (+ y (* 2d0 r (sin a))))
             :finally
                (multiple-value-setq (new-target-x new-target-y)
                  (multiple-value-bind (col row)
                      (tile-index new-target-x new-target-y)
                    (tile-pos (coerce col 'double-float)
                              (coerce row 'double-float)))))
      (with-character entity ()
        (when (or (zerop (length path))
                  (destructuring-bind (current-target-x . current-target-y)
                      (simple-vector-peek path)
                    (> (euclidean-distance
                        new-target-x new-target-y
                        current-target-x current-target-y)
                       1d0)))
          (setf path (a* x y new-target-x new-target-y))
          (unless (zerop (length path))
            (follow-path entity)))))))

(declaim
 (inline approx-equal)
 (ftype (function (double-float double-float &optional double-float) boolean) approx-equal))
(defun approx-equal (a b &optional (epsilon 0.01d0))
  (< (abs (- a b)) epsilon))

(defmethod system-update ((system character-system) dt)
  (with-characters
      (with-coordinate entity ()
        (with-sprite entity ()
          (let ((delta (* dt speed)))
            (if (and (approx-equal target-x x delta) (approx-equal target-y y delta))
                (if (zerop (length path))
                    (when (eq stance :walk)
                      (switch-stance entity :idle))
                    (follow-path entity))
                (let ((direction-x (* 0.5d0 delta (cos angle)))
                      (direction-y (* 0.5d0 delta (sin angle) (/ *tile-width* *tile-height*))))
                  ;; TODO : this check is kinda redundant
                  ;;  (but left here to check dynamic collisions later)
                  ;; also it still sometimes stuck when it shouldnt (on corners)
                  (cond
                    ((multiple-value-call #'collidesp
                       (tile-index (+ x direction-x)
                                   (+ y direction-y)))
                     (setf target-x x
                           target-y y
                           path (make-array 0))
                     (switch-stance entity :idle))
                    (t
                     (incf x (* 2d0 direction-x))
                     (incf y (* 2d0 direction-y))
                     (unless (eq stance :walk)
                       (switch-stance entity :walk)))))))))))

(defmethod system-draw ((system character-system) renderer)
  (flet ((path-node-pos (x y)
           (multiple-value-bind (mx my)
               (multiple-value-bind (xi yi)
                   (tile-index x y)
                 (map->screen
                  (coerce xi 'double-float)
                  (coerce yi 'double-float)))
             (absolute->viewport
              (+ mx (floor *tile-width* 2))
              (+ my (floor *tile-height* 2))))))
    (with-system-config-options ((debug-path))
      (when debug-path
        (with-characters
            (loop
              :with r := (first debug-path)
              :with g := (second debug-path)
              :with b := (third debug-path)
              :with a := (fourth debug-path)
              :for path-node :across path
              :for start := t :then nil
              :for node-x := (car path-node)
              :for node-y := (cdr path-node)
              :for (x y) := (multiple-value-list (path-node-pos node-x node-y))
              :do (unless start
                    (add-debug-point debug-entity x y r g b a))
                  (add-debug-point debug-entity x y r g b a)))))))

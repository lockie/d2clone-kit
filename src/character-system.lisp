(in-package :d2clone-kit)

;; TODO : rename this to movable?..

;; TODO : perhaps turn the character towards the goal while movement to make it more natural


(defsystem character
  ()
  (:documentation "Handles sprites that are able to walk and collide with obstacles."))

(defcomponent (character)
  (speed nil :type double-float)
  (debug-entity +invalid-entity+ :type fixnum))

(defaction move
    ((target-x nil :type double-float)
     (target-y nil :type double-float)
     (path (make-array 0) :type simple-vector))
    (:documentation "The movement action.")

  (defmethod initialize-action ((type (eql :move)) action)
    (let ((entity (action-entity action)))
      (with-move-action action ()
        (with-coordinate entity ()
          (multiple-value-bind (new-target-x new-target-y)
              ;; make sure new target is walkable
              (closest-walkable-point entity x y target-x target-y)
            (when (or (length= 0 path)
                      (destructuring-bind (current-target-x . current-target-y)
                          (simple-vector-peek path)
                        (> (euclidean-distance
                            new-target-x new-target-y
                            current-target-x current-target-y)
                           1d0)))
              (setf path (a* x y new-target-x new-target-y))
              (if (length= 0 path)
                  (setf target-x new-target-x
                        target-y new-target-y)
                  (follow-path action))))))))

  (defmethod finalize-action ((type (eql :move)) action)
    (switch-stance (action-entity action) :idle)))

(defperformer move (action target-x target-y path)
  (let ((entity (action-entity action)))
    (with-coordinate entity ()
      (with-sprite entity ()
        (if (not (approx-equal angle (face-target x y target-x target-y)))
            (if (length= 0 path)
                ;; reach the destination
                (delete-action action)
                (follow-path action))
            (with-character entity ()
              (let* ((delta (* *delta-time* speed))
                     (direction-x (* delta (cos angle)))
                     (direction-y (* delta (sin angle))))
                (cond
                  ((collidesp (round (+ x direction-x))
                              (round (+ y direction-y))
                              :character entity)
                   ;; stumble upon an obstacle
                   (delete-action action))
                  (t
                   (issue character-moved
                          :entity entity
                          :old-x x
                          :old-y y
                          :new-x (incf x direction-x)
                          :new-y (incf y direction-y))
                   (switch-stance entity :walk))))))))))

(defaction track
    ((target +invalid-entity+ :type fixnum)
     (target-distance 2d0 :type double-float))
    (:documentation "The entity tracking action.")

  (defmethod initialize-action ((type (eql :track)) action)
    (let ((entity (action-entity action)))
      (with-track-action action ()
        (with-coordinate entity (current-x current-y)
          (with-coordinate target (target-x target-y)
            (if (<= (euclidean-distance target-x target-y current-x current-y)
                    (+ 0.0d0 target-distance))
                (delete-action action)
                (make-move-action entity :parent action :target-x target-x :target-y target-y))))))))

(defperformer track (action target target-distance)
  (let* ((entity (action-entity action))
         (child-action (action-child action)))
    (with-coordinate entity (current-x current-y)
      (with-coordinate target (target-x target-y)
        (if (> (euclidean-distance target-x target-y current-x current-y)
               target-distance)
            (if (not (index-valid-p (action-child action)))
                (delete-action action) ;; movement action has stuck, stop current action
                (multiple-value-bind (new-target-x new-target-y)
                    (closest-walkable-point entity current-x current-y target-x target-y)
                  (with-move-action child-action (move-target-x move-target-y path)
                    (destructuring-bind (current-target-x . current-target-y)
                        (if (length= 0 path)
                            (cons move-target-x move-target-y)
                            (simple-vector-peek path))
                      (declare (type double-float current-target-x current-target-y))
                      (unless (and (= current-target-x new-target-x)
                                   (= current-target-y new-target-y))
                        (setf move-target-x new-target-x
                              move-target-y new-target-y
                              path (make-array 0))
                        (initialize-action :move child-action))))))
            ;; reach the destination
            (when (or (not (eq (current-stance entity) :walk))
                      (stance-finished-p entity))
              (delete-action action)))))))

(defmethod make-component ((system character-system) entity &rest parameters)
  (destructuring-bind (&key (speed 2d0)) parameters
    (with-system-config-options ((debug-path))
      (make-character entity
                      :speed speed
                      :debug-entity (if debug-path
                                        (make-object '((:debug :order 1050d0)) entity)
                                        +invalid-entity+)))))

(declaim
 (inline euclidean-distance)
 (ftype (function (double-float double-float double-float double-float) double-float)
        euclidean-distance))
(defun euclidean-distance (x1 y1 x2 y2)
  (flet ((sqr (x) (the double-float (* x x))))
    ;; TODO : use Carmack's fast sqrt here
    (sqrt
     (+
      (sqr (- x1 x2))
      (sqr (- y1 y2))))))

(defstruct (path-node
            (:constructor make-path-node (x y))
            (:copier nil)
            (:predicate nil))
  (cost 0d0 :type double-float)
  (x 0 :type fixnum :read-only t)
  (y 0 :type fixnum :read-only t)
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

(define-constant +neighbours+ '((1  . 0)
                                (1  . -1)
                                (0  . -1)
                                (-1 . -1)
                                (-1 . 0)
                                (-1 . 1)
                                (0  . 1)
                                (1  . 1))
  :test #'equal)

;; TODO : optimize
;; TODO : penalize turns?..
;; TODO : separate thread?..
;; TODO : XXX : the path changes when moving along the way.
;; TODO : XXX : precalculate something?.. like Dijkstra Maps. see
;; https://www.reddit.com/r/roguelikedev/comments/hpxnkd
(declaim (ftype (function (double-float double-float double-float double-float)) a*))
(defun a* (start-x start-y goal-x goal-y)
  "Runs A* algorithm to find path from point START-X, START-Y to GOAL-X, GOAL-Y.
Returns simple array containing conses of x and y path node world coordinates.

Note: if goal point is not walkable, this function will stuck."
  (declare (optimize (speed 3)))
  (let* ((goal-col (floor goal-x))
         (goal-row (floor goal-y))
         (initial-x (round start-x))
         (initial-y (round start-y))
         (open (make-priority-queue #'path-node-cost))
         (closed nil)
         (start-node (make-path-node initial-x initial-y)))
    (priority-queue-push open start-node)
    (let ((goal-node
            (loop :for current := (priority-queue-pop open)
                  :until (and (= goal-col (path-node-x current))
                              (= goal-row (path-node-y current)))
                  :do (push current closed)
                      (dolist (neighbour +neighbours+)
                        (let ((neighbour-x (+ (path-node-x current)
                                              (the fixnum (car neighbour))))
                              (neighbour-y (+ (path-node-y current)
                                              (the fixnum (cdr neighbour)))))
                          (let* ((cost (+ (euclidean-distance
                                           start-x start-y
                                           (coerce (path-node-x current) 'double-float)
                                           (coerce (path-node-y current) 'double-float))
                                          (sqrt (+ (expt (the fixnum (car neighbour)) 2)
                                                   (expt (the fixnum (cdr neighbour)) 2)))
                                          (if (collidesp neighbour-x neighbour-y)
                                              10000d0 0d0)))
                                 (neighbour-cost (euclidean-distance
                                                  start-x start-y
                                                  (coerce neighbour-x 'double-float)
                                                  (coerce neighbour-y 'double-float)))
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
                                           (+ cost (euclidean-distance
                                                    (coerce neighbour-x 'double-float)
                                                    (coerce neighbour-y 'double-float)
                                                    goal-x goal-y))
                                           (path-node-parent neighbour-node) current)
                                     (priority-queue-push open neighbour-node))))))))
                  :finally (return current))))
      (loop
        :with result := (make-array 0 :element-type 'cons :adjustable t :fill-pointer t)
        :for node := goal-node :then (path-node-parent node)
        :until (eq start-node node)
        :do (vector-push-extend (cons
                                 (coerce (path-node-x node) 'double-float)
                                 (coerce (path-node-y node) 'double-float))
                                result)
        :finally (return (make-array (length result)
                                     :element-type 'cons
                                     :initial-contents result))))))

(declaim
 (inline face-target)
 (ftype (function (double-float double-float double-float double-float) double-float)
        face-target))
(defun face-target (character-x character-y target-x target-y)
  "Returns the angle that the character at CHARACTER-X, CHARACTER-Y should be facing to look
at point TARGET-X, TARGET-Y."
  (atan (- target-y character-y)
        (- target-x character-x)))

(declaim (inline follow-path) (ftype (function (fixnum)) follow-path))
(defun follow-path (move-action-index)
  (let ((entity (action-entity move-action-index)))
    (with-coordinate entity ()
      (with-sprite entity ()
        (with-move-action move-action-index ()
          (multiple-value-bind (target new-path)
              (simple-vector-pop path)
            (setf path new-path
                  target-x (car target)
                  target-y (cdr target)
                  angle (face-target x y target-x target-y))))))))

(declaim
 (ftype (function ((or null fixnum) double-float double-float double-float double-float)
                  (values double-float double-float)) closest-walkable-point))
(defun closest-walkable-point (character-entity x y target-x target-y)
  "Returns walkable point closest to target on line from X, Y to TARGET-X, TARGET-Y."
  ;; TODO : when there's some obstacle between, that's a problem
  (let ((new-target-x target-x)
        (new-target-y target-y))
    (loop
      :with dx := (- new-target-x x) :and dy := (* (- new-target-y y) 0.5d0)
      :with a := (atan dy dx) :and r := (sqrt (+ (* dx dx) (* dy dy)))
      :while (collidesp (floor new-target-x) (floor new-target-y) :character character-entity)
      :do (setf r (- r 0.5d0)
                new-target-x (+ x (* r (cos a)))
                new-target-y (+ y (* 2d0 r (sin a))))
      :finally
         (return (values new-target-x new-target-y)))))

(declaim
 (ftype (function (fixnum double-float double-float)) move))
(defun move (entity new-target-x new-target-y)
  "Sets character ENTITY new movement target to NEW-TARGET-X, NEW-TARGET-Y."
  (let ((move-action (has-action-p entity :move)))
    (if (and move-action
             (not (index-valid-p (action-parent move-action))))
        (with-move-action move-action ()
          (setf target-x new-target-x
                target-y new-target-y)
          ;; TODO when path is 1 node long, movement is slower. has to do with pathfinding
          (initialize-action :move move-action))
        (make-move-action entity :target-x new-target-x :target-y new-target-y))))

(declaim
 (inline approx-equal)
 (ftype (function (double-float double-float &optional double-float) boolean) approx-equal))
(defun approx-equal (a b &optional (epsilon 0.05d0))
  (< (abs (- a b)) epsilon))

(defmethod system-draw ((system character-system) renderer)
  (flet ((path-node-pos (x y)
           (multiple-value-bind (screen-x screen-y)
               (multiple-value-call #'absolute->viewport
                 (orthogonal->screen x y))
             (values
              (+ screen-x (floor *tile-width* 2))
              (+ screen-y (floor *tile-height* 2))))))
    (with-system-config-options ((debug-path))
      (when debug-path
        (with-move-actions (action target-x target-y path)
          (let ((entity (action-entity action)))
            (with-character entity ()
              (unless (= entity (player-entity))
                (multiple-value-bind (x y)
                    (multiple-value-call #'absolute->viewport
                      (orthogonal->screen
                       (coerce target-x 'double-float)
                       (coerce target-y 'double-float)))
                  (add-debug-tile-rhomb debug-entity x y debug-path nil)))
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
                    (add-debug-point debug-entity x y r g b a)))))))))

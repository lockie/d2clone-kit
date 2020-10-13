(in-package :d2clone-kit)


(defsystem item
  ;; TODO : by using this design, I can only have one item per tile. Do something about the coords
  ((map (make-sparse-matrix) :type sparse-matrix))
  (:documentation "Handles items."))

(defcomponent (item)
  (type nil :type keyword))

;; TODO : highlight an item when hovering -> need color changing mechanic
;; TODO : periodically draw star thing (to alert a player there's an item on the ground)

(defconstant +item-pickup-range+ 1d0 "Maximum distance at which character could pick up an item.")

(defaction item-pickup
    ((target +invalid-entity+ :type fixnum :documentation "Item entity."))
    (:documentation "The item pickup action.")

  (defmethod initialize-action ((type (eql :item-pickup)) action)
    (let ((entity (action-entity action)))
      (with-item-pickup-action action ()
        (make-track-action entity
                           :parent action
                           :target target
                           :target-distance +item-pickup-range+)))))

(defperformer item-pickup (action target track-action)
  (let* ((entity (action-entity action)))
    (with-coordinate entity (current-x current-y)
      (with-coordinate target (target-x target-y)
        (if (> (euclidean-distance target-x target-y current-x current-y)
               +item-pickup-range+)
            (unless (index-valid-p (action-child action))
              (delete-action action))
            (progn
              (delete-action action)
              (pickup-item target)))))))

(defmethod make-component ((system item-system) entity &rest parameters)
  (destructuring-bind (&key type) parameters
    (when (has-component-p :coordinate entity)
      (with-system-slots ((map) item-system system)
        (with-coordinate entity ()
          ;; TODO : consider character size (#21)
          (setf (sparse-matrix-ref map (cons (round x) (round y))) entity))))
    (make-item entity :type type)
    (make-component *sprite-system* entity
                    :prefab :loot
                    :layers-initially-toggled (list type))))

(defun draw-item-text (item-entity renderer)
  "Draws a text above an item for ITEM-ENTITY with RENDERER."
  (with-item item-entity ()
    (with-screen-coordinate item-entity (screen-x screen-y)
      (multiple-value-bind (x y)
          (absolute->viewport screen-x screen-y)
        (render
         renderer
         9000d0
         (let ((type type) (x x) (y y))
           #'(lambda ()
               ;; TODO : prevent text overlap
               (let* ((text (string-capitalize (substitute #\Space #\- (string type))))
                      (width (al:get-text-width (ui-font-large) text)))
                 (al:draw-filled-rectangle x y (+ x width) (+ y 20)
                                           (al:map-rgba 10 10 10 160))
                 (al:draw-text (ui-font-large)
                               (al:map-rgba 255 255 255 0) x y 0 text)))))))))

(declaim (inline item-at) (ftype (function (fixnum fixnum) (or fixnum null)) item-at))
(defun item-at (x y)
  "Returns item entity at integer map coordinates X, Y or NIL if there's no character there."
  (sparse-matrix-ref (item-system-map *item-system*) (cons x y)))

(define-constant +item-neighbours+ '((1 . 1)
                                     (1 . 0)
                                     (0 . 1)
                                     (1 . -1)
                                     (-1 . 1))
  :test #'equal)

(defun drop-item (owner-entity item)
  "Makes an OWNER-ENTITY drop an item corresponding to keyword ITEM."
  (flet ((entity-tile (entity)
           (with-coordinate entity ()
             (cons (round x) (round y)))))
    (let ((forbidden-positions '()))
      (with-items
          (push (entity-tile entity) forbidden-positions))
      (push (entity-tile (player-entity)) forbidden-positions)
      (with-coordinate owner-entity ()
        (let* ((owner-x (round x))
               (owner-y (round y))
               (positions
                 (mapcar
                  #'(lambda (delta)
                      (cons (+ owner-x (car delta))
                            (+ owner-y (cdr delta))))
                  +item-neighbours+)))
          (when-let (position
                     (some
                      #'(lambda (p)
                          (unless (or (find p forbidden-positions :test #'equal)
                                      (collidesp (car p) (cdr p)))
                            p))
                      positions))
            (make-object `((:coordinate :x ,(- (coerce (car position) 'double-float) 0.49d0)
                                        :y ,(- (coerce (cdr position) 'double-float) 0.49d0))
                           (:item :type ,item))
                         *session-entity*)))))))

  ;; TODO : unhardcode
(define-constant +weapons+
    '(:dagger (2d0 . 3d0)
      :short-sword (4d0 . 7d0)
      :long-sword (8d0 . 12d0)
      :great-sword (15d0 . 20d0))
  :test #'equal)

(defun pickup-item (item-entity)
  "Does an actual picking up of ITEM-ENTITY by player."
  ;; TODO : add picking up sound
  ;; TODO : unhardcode
  (with-system-slots ((map) item-system)
    (with-coordinate item-entity ()
      ;; TODO : consider character size (#21)
      (setf (sparse-matrix-ref map (cons (round x) (round y))) nil)))
  (with-item item-entity ()
    (if (eq type :health-potion)
        (let ((player-entity (player-entity)))
          (with-coordinate player-entity (player-x player-y)
            (make-object
             `((:coordinate :x ,player-x :y ,player-y)
               (:sound :prefab :gulp)) player-entity))
          (with-hp player-entity ()
            (set-hp player-entity (+ current-hp 20d0)))
          (delete-child *session-entity* item-entity)
          (delete-entity item-entity))
        (let ((player-entity (player-entity)))
          (with-sprite player-entity ()
            (doplist (weapon _ +weapons+)
              (when (gethash weapon layers-toggled)
                (drop-item player-entity weapon)
                (setf (gethash weapon layers-toggled) nil))))
          (toggle-layer player-entity type t)
          (with-combat player-entity ()
            (destructuring-bind (weapon-min-dmg . weapon-max-dmg)
                (getf +weapons+ type)
              (setf min-damage weapon-min-dmg
                    max-damage weapon-max-dmg)))
          (delete-child *session-entity* item-entity)
          (delete-entity item-entity)))))

(eval-when (:compile-toplevel :load-toplevel)
  ;; https://stackoverflow.com/a/29361029/1336774
  (defun biased-generator (values weights)
    (multiple-value-bind (total values)
        (loop :for v :in values
              :for w :in weights
              :nconc (make-list w :initial-element v) :into vs
              :sum w :into total
              :finally (return (values total (coerce vs 'vector))))
      #'(lambda ()
          (aref values (random total))))))

(define-constant +item-generator+
    ;; TODO : unhardcode
    (biased-generator '(nil :health-potion :dagger :short-sword :long-sword)
                      '(5 10 5 2 1))
  :test (constantly t))

(defhandler (item-system entity-died
             :filter (not (= (entity-died-entity event) (player-entity))))
  (when-let (item (funcall +item-generator+))
    (drop-item (entity-died-entity event) item)))

(defhandler (item-system entity-deleted)
  (let ((entity (entity-deleted-entity event)))
    (when (%has-component-p system entity)
      (with-system-slots ((map) item-system system)
        (with-coordinate entity ()
          (sparse-matrix-remove map (cons (the fixnum (round x))
                                          (the fixnum (round y)))))))))

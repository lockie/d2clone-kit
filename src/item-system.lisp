(in-package :d2clone-kit)


(defclass item-system (system)
  ((name :initform 'item))
  (:documentation "Handles items."))

(defcomponent item item
  (type nil :type keyword))

(defmethod make-component ((system item-system) entity &rest parameters)
  (destructuring-bind (&key type) parameters
    (with-item entity (item-type)
      (setf item-type type))
    (make-component (system-ref 'sprite) entity
                    :prefab :loot
                    :layers-initially-toggled (list type))))

(defmethod system-draw ((system item-system) renderer)
  (let ((coordinate-system (system-ref 'coordinate)))
    (with-items
      (when (has-component-p coordinate-system entity)
        (with-screen-coordinate entity (screen-x screen-y)
          (multiple-value-bind (x y)
              (absolute->viewport screen-x screen-y)
              (render
               renderer
               9000d0
               (let ((type type)
                     (x x)
                     (y y))
                 #'(lambda ()
                     ;; TODO : prevent text overlap
                     (let* ((text (string-capitalize (substitute #\Space #\- (string type))))
                            (width (al:get-text-width *large-ui-font* text)))
                       (al:draw-filled-rectangle x y (+ x width) (+ y 20)
                                                 (al:map-rgba 10 10 10 160))
                       (al:draw-text *large-ui-font*
                                     (al:map-rgba 255 255 255 0) x y 0 text)))))))))))

(eval-when (:compile-toplevel)
  (defconstant +angles+
    (mapcar #'(lambda (x) (+ 0.01d0 (/ (* x pi) 180d0)))
            '(90 45 135 0 180))))

(defun drop-item (owner-entity item)
  (flet ((entity-tile-index (entity)
           (with-coordinate entity ()
             (multiple-value-bind (col row)
                 (tile-index x y)
               (cons col row)))))
    (let ((forbidden-positions '()))
      (with-items
          (push (entity-tile-index entity) forbidden-positions))
      (push (entity-tile-index (player-entity)) forbidden-positions)
      (with-coordinate owner-entity (owner-x owner-y)
        (multiple-value-bind (owner-col owner-row)
            (tile-index owner-x owner-y)
          (let ((positions
                  (mapcar
                   #'(lambda (angle)
                       (multiple-value-bind (x y)
                           (next-tile angle owner-col owner-row)
                         (cons x y)))
                   +angles+)))
            (when-let (position
                       (some
                        #'(lambda (p)
                            (unless
                                (or (find p forbidden-positions :test #'equal)
                                    (collidesp (car p) (cdr p)))
                              p))
                        positions))
              (let ((item-entity (make-entity)))
                (multiple-value-bind (x y)
                    (tile-pos (coerce (car position) 'double-float)
                              (coerce (cdr position) 'double-float))
                  (make-component (system-ref 'coordinate) item-entity :x x :y (- y 0.5d0)))
                (make-component (system-ref 'item) item-entity :type item)))))))))

(eval-when (:compile-toplevel)
  (defconstant +gulp-initializer+
    (make-entity-initializer
     '((:coordinate :x x :y y)
       (:sound :prefab :gulp)))))

  ;; TODO : unhardcode
(eval-when (:compile-toplevel)
  (defconstant +weapons+
    '(:dagger (2d0 . 3d0)
      :short-sword (4d0 . 7d0)
      :long-sword (8d0 . 12d0)
      :great-sword (15d0 . 20d0))))

(defun pickup-item (item-entity)
  ;; TODO : unhardcode
  (with-item item-entity ()
    (if (eq type :health-potion)
        (let ((player-entity (player-entity)))
          (with-coordinate player-entity (player-x player-y)
            (let ((x player-x) (y player-y))
              (declare (special x y))
              (funcall +gulp-initializer+)))
          (with-hp player-entity ()
            (set-hp player-entity (+ current-hp 20d0)))
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
          (delete-entity item-entity)))))

(defhandler item-system character-moved (event entity new-x new-y)
  :filter '(= entity (player-entity))
  (let ((coordinate-system (system-ref 'coordinate)))
    (with-coordinate entity (player-x player-y)
      (multiple-value-bind (player-col player-row)
          (tile-index player-x player-y)
        (with-items
            (when (has-component-p coordinate-system entity)
              (with-coordinate entity ()
                (multiple-value-bind (item-col item-row)
                    (tile-index x y)
                  (when (and (= item-col player-col)
                             (= item-row player-row))
                    (pickup-item entity)
                    (return))))))))))

(eval-when (:compile-toplevel)
  ;; https://stackoverflow.com/a/29361029/1336774
  (defun biased-generator (values weights)
    (multiple-value-bind (total values)
        (loop :for v :in values
              :for w :in weights
              :nconc (make-list w :initial-element v) :into vs
              :sum w :into total
              :finally (return (values total (coerce vs 'vector))))
      #'(lambda ()
          (aref values (random total)))))

  (defconstant +item-generator+
    ;; TODO : unhardcode
    (biased-generator '(nil :health-potion :dagger :short-sword :long-sword)
                      '(5 10 5 2 1))))

(defhandler item-system entity-died (event entity)
  :filter '(not (= entity (player-entity)))
  (when-let (item (funcall +item-generator+))
    (drop-item entity item)))

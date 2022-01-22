(in-package :d2clone-kit)


(defgeneric make-component (system entity &rest parameters)
  (:documentation "Creates new component using PARAMETERS within system SYSTEM
  for entity ENTITY.

PARAMETERS could include `:PREFAB` key, in which case component is constructed
using corresponding prefab.

See MAKE-PREFAB-COMPONENT"))

(defgeneric delete-component (system entity)
  (:documentation "Deletes SYSTEM's component from ENTITY."))

(defmethod delete-component ((system system) entity)
  (declare (ignore system entity))
  ;; default implementation for componentless systems
  )

;; TODO : automatically delete entities with no components?..

(declaim
 #-d2c-debug (inline system-adjust-components)
 (ftype (function (system array-length)) system-adjust-components))
(defun system-adjust-components (system new-size)
  (when-let ((components-index (system-components-index system)))
    (sparse-array-index-grow components-index new-size)))

(declaim
 #-d2c-debug (inline %has-component-p)
 (ftype (function (system fixnum) boolean) %has-component-p))
(defun %has-component-p (system entity)
  (when-let ((components-index (system-components-index system)))
    (index-valid-p (sparse-array-index-ref components-index entity))))

(declaim
 #-d2c-debug (inline has-component-p)
 (ftype (function (keyword fixnum) boolean) has-component-p))
(defun has-component-p (system entity)
  "Returns T when ENTITY has the SYSTEM's component in it."
  (if-let (system-instance (gethash system *systems*))
    (%has-component-p system-instance entity)
    nil))

(defmacro defsoa (name &rest slots)
  "Defines structure-of-arrays with NAME and SLOTS and corresponding accessors."
  (let* ((plural-name (string-upcase (plural-of name)))
         (slot-names (mapcar #'car slots))
         (slot-defaults (mapcar #'cadr slots))
         (slot-types (mapcar #'(lambda (s) (getf s :type t)) slots))
         (slot-ro (mapcar #'(lambda (s) (getf s :read-only nil)) slots))
         (slot-docs (mapcar
                     #'(lambda (s)
                         (when-let (doc (getf s :documentation))
                           `(setf (documentation
                                   #',(symbolicate name :- (car s))
                                   'function)
                                  ,doc)))
                     slots))
         (soa-slots (mapcar #'(lambda (name default)
                                `(,name (make-growable-vector
                                         :initial-element ,default)
                                        :type growable-vector :read-only t))
                            slot-names slot-defaults))
         (slot-accessors (mapcar
                          #'(lambda (s) `(,(symbolicate name :- s :-aref*)))
                          slot-names))
         (unsafe-slot-accessors
           (mapcar
            #'(lambda (s) `(,(symbolicate name :- s :-aref)))
            slot-names))
         (array-accessors (mapcar
                           #'(lambda (s) `(,(symbolicate name :- s)))
                           slot-names))
         (getter-decls (mapcan
                        #'(lambda (s a type)
                            `((declaim
                               #-d2c-debug (inline ,@s)
                               (ftype
                                (function (,name array-index) ,type) ,@s))
                              (defun ,@s (objects index)
                                (growable-vector-ref (,@a objects) index))))
                        unsafe-slot-accessors array-accessors slot-types))
         (setter-decls (mapcan
                        #'(lambda (ro s a type)
                            (unless ro
                              `((declaim
                                 #-d2c-debug (inline (setf ,@s))
                                 (ftype
                                  (function
                                   (,type ,name array-index)
                                   ,type)
                                  (setf ,@s)))
                                (defun (setf ,@s) (new-value objects index)
                                  (setf
                                   (growable-vector-ref* (,@a objects) index)
                                   new-value)))))
                        slot-ro slot-accessors array-accessors slot-types))
         (unsafe-setter-decls
           (mapcan
            #'(lambda (ro s a type)
                (unless ro
                  `((declaim
                     #-d2c-debug (inline (setf ,@s))
                     (ftype (function (,type ,name array-index)
                                      ,type)
                            (setf ,@s)))
                    (defun (setf ,@s) (new-value objects index)
                      (setf (growable-vector-ref (,@a objects) index)
                            new-value)))))
            slot-ro unsafe-slot-accessors array-accessors slot-types)))
    `(progn
       (defstruct (,name
                   (:constructor ,(symbolicate :make- plural-name))
                   (:copier nil) (:predicate nil))
         ,@soa-slots)
       (declaim #-d2c-debug (inline ,@(mapcar #'car array-accessors)))
       ,@slot-docs ,@getter-decls ,@setter-decls ,@unsafe-setter-decls)))

(defmacro defcomponent ((system &optional (name system)) &rest slots)
  "Defines component structure with NAME and SLOTS within SYSTEM."
  (let* ((system-type (symbolicate system :-system))
         (system-instance (symbolicate :* system-type :*))
         (plural-name (string-upcase (plural-of name)))
         (slot-names (mapcar #'car slots))
         (slot-defaults (mapcar #'cadr slots))
         (slot-accessors (mapcar
                          #'(lambda (s) `(,(symbolicate name :- s :-aref*)))
                          slot-names))
         (unsafe-slot-accessors
           (mapcar #'(lambda (s) `(,(symbolicate name :- s :-aref)))
                   slot-names)))
    `(progn
       (defsoa ,name ,@slots)
       (defmacro ,(symbolicate :with- name) (entity bindings &body body)
         (with-gensyms (index system components)
           (let ((component-exps (mapcar #'list
                                         (if bindings bindings ',slot-names)
                                         (mapcar
                                          #'(lambda (a)
                                              `(,@a ,components ,index))
                                          ',unsafe-slot-accessors))))
             `(let* ((,system ,',system-instance)
                     (,components (system-components ,system))
                     ;; TODO : check if index is valid in debug mode
                     ;;  also check if it was deleted
                     (,index (sparse-array-index-ref
                              (system-components-index ,system)
                              ,entity)))
                (symbol-macrolet (,@component-exps) ,@body)))))
       (defmacro ,(symbolicate :with- plural-name) (&body body)
         (with-gensyms (index system components)
           (let ((component-exps
                   (mapcar #'(lambda (s a) `(,s (,@a ,components ,index)))
                           ',slot-names ',unsafe-slot-accessors)))
             `(let* ((,system ,',system-instance)
                     (,components (system-components ,system)))
                (declare (ignorable ,components))
                (do-sparse-array (entity ,index
                                         (system-components-index ,system))
                  (symbol-macrolet (,@component-exps)
                    ,@body))))))
       (defmethod system-create ((system ,system-type))
         (setf (system-components-index system)
               (make-sparse-array-index
                :initial-allocated-size *entities-allocated*)
               (system-components system)
               (,(symbolicate :make- plural-name)))
         (preload-prefabs system))
       (defmethod delete-component ((system ,system-type) entity)
         (when-let ((components-index (system-components-index system)))
           (sparse-array-index-delete components-index entity)))
       (defun ,(symbolicate :make- name) (entity &key ,@(mapcar #'list
                                                                slot-names
                                                                slot-defaults))
         (let* ((system ,system-instance)
                (components (system-components system))
                (index (sparse-array-index-push (system-components-index system)
                                                entity)))
           (declare (ignorable components index))
           (setf ,@(mapcan
                    #'(lambda (a s) `((,@a components index) ,s))
                    slot-accessors slot-names)))))))

;; TODO : version of with- `plural` macro which goes over several systems'
;; components?..

(in-package :d2clone-kit)


(defgeneric make-component (system entity &rest parameters)
  (:documentation "Creates new component using PARAMETERS within system SYSTEM for entity ENTITY.

PARAMETERS could include `:PREFAB` key, in which case component is constructed using corresponding prefab.

See MAKE-PREFAB-COMPONENT"))

(defgeneric system-adjust-components (system new-size))

(defmethod system-adjust-components ((system system) new-size)
  ;; default implementation for componentless systems
  )

(defgeneric delete-component (system entity)
  (:documentation "Deletes SYSTEM's component from ENTITY."))

(defmethod delete-component ((system system) entity)
  (declare (ignore system entity))
  ;; default implementation for componentless systems
  )

;; TODO : automatically delete entities with no components?..

(defgeneric has-component-p (system entity)
  (:documentation "Returns T when ENTITY has the SYSTEM's component in it."))

(defmethod has-component-p ((system system) entity)
  (declare (ignore system entity))
  ;; default implementation for componentless systems
  )

(defmacro defcomponent (system name &rest slots)
  "Defines component structure with name NAME and slots SLOTS within system SYSTEM."
  ;; TODO : rewrite components storage using sparse array index based on growable vector to
  ;;  remove unnecessary NIL checks and increase cache friendliness
  (let* ((system-name (symbolicate system '-system))
         (system-instance (symbolicate '* system-name '*))
         (plural-name (string-upcase (plural-of name)))
         (slot-names (mapcar #'car slots))
         (slot-defaults (mapcar #'cadr slots))
         (slot-types (mapcar #'(lambda (s) (getf s :type 't)) slots))
         (slot-ro (mapcar #'(lambda (s) (getf s :read-only nil)) slots))
         (soa-slots (mapcar #'(lambda (name default type ro)
                                `(,name (make-array *entities-allocated*
                                                    :element-type '(or ,type null)
                                                    :initial-element ,default)
                                        :type (simple-array (or ,type null))
                                        :read-only ,ro))
                            slot-names slot-defaults slot-types slot-ro))
         (slot-accessors (mapcar #'(lambda (s) `(,(symbolicate name '- s '-aref))) slot-names))
         (array-accessors (mapcar #'(lambda (s) `(,(symbolicate name '- s))) slot-names))
         (adjust-assignments (mapcar #'(lambda (a d)
                                         (let ((acc `(,@a components)))
                                           `(setf ,acc
                                                  (adjust-array ,acc new-size
                                                                :initial-element ,d))))
                                     array-accessors slot-defaults))
         (initialization-assignments
           (mapcan #'(lambda (a d) (copy-list `((aref (,@a components) entity) ,d)))
                   array-accessors slot-defaults))
         (getter-decls (mapcan
                        #'(lambda (s a type)
                            `((declaim
                               (inline ,@s)
                               (ftype (function (,name (integer 0 ,array-dimension-limit)) ,type)
                                      ,@s))
                              (defun ,@s (objects index) (aref (,@a objects) index))))
                        slot-accessors array-accessors slot-types))
         (setter-decls (mapcan
                        #'(lambda (ro s a type)
                            (unless ro
                              `((declaim
                                 (inline (setf ,@s))
                                 (ftype (function
                                         (,type ,name (integer 0 ,array-dimension-limit)) ,type)
                                        (setf ,@s)))
                                (defun (setf ,@s) (new-value objects index)
                                  (setf (aref (,@a objects) index) new-value)))))
                        slot-ro slot-accessors array-accessors slot-types))
         (delete-exprs (mapcan #'(lambda (a) (copy-list `((aref (,@a components) entity) nil)))
                               array-accessors)))
    `(progn
       (defstruct ,name ,@soa-slots)
       (defmacro ,(symbolicate 'with- name) (entity bindings &rest body)
         (with-gensyms (components)
           (let ((component-exps (mapcar #'list
                                         (if bindings bindings ',slot-names)
                                         (mapcar #'(lambda (a) `(,@a ,components ,entity))
                                                 ',slot-accessors))))
             `(let ((,components (system-components ,',system-instance)))
                (symbol-macrolet (,@component-exps) ,@body)))))
       (defmacro ,(symbolicate 'with- plural-name) (&rest body)
         (with-gensyms (components)
           (let ((slot-names ',slot-names)
                 (loop-clauses (mapcan #'(lambda (s a)
                                           `(:for ,s :across ,`(,@a ,components)))
                                       ',slot-names ',array-accessors))
                 (component-exps (mapcar #'(lambda (s type a)
                                             `(,s (the ,type (elt ,`(,@a ,components) entity))))
                                         ',slot-names ',slot-types ',array-accessors)))
             `(let ((,components (system-components ,',system-instance)))
                (loop :for entity :from 0 :below *entities-count*
                      ,@loop-clauses
                      :when (and ,@slot-names)
                        :do (symbol-macrolet (,@component-exps) ,@body))))))
       (defmethod system-create ((system ,system-name))
         (setf (system-components system) (,(symbolicate 'make- name)))
         (preload-prefabs system))
       (defmethod system-adjust-components ((system ,system-name) new-size)
         (declare (type (integer 0 ,array-dimension-limit) new-size))
         (let ((components (system-components system)))
           ,@adjust-assignments))
       (defmethod delete-component ((system ,system-name) entity)
         (let ((components (system-components system)))
           (setf ,@delete-exprs)))
       (defmethod has-component-p ((system ,system-name) entity)
         (let ((components (system-components system)))
           (and ,@(mapcar
                   #'(lambda (a) `(aref (,@a components) entity))
                   array-accessors))))
       (defmethod make-component :before ((system ,system-name) entity &rest parameters)
         (declare (ignore parameters))
         (let ((components (system-components system)))
           (setf ,@initialization-assignments)))
       (defmethod make-prefab-component :before ((system ,system-name) entity prefab parameters)
         (declare (ignore parameters))
         (let ((components (system-components system)))
           (setf ,@initialization-assignments)))
       ,@getter-decls ,@setter-decls)))

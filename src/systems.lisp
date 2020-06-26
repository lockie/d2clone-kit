(in-package :d2clone-kit)


(defstruct (system
            (:constructor nil)
            (:copier nil)
            (:predicate nil))
  "Base structure for all ECS systems."
  (name nil :type symbol)
  (components nil)
  (order 0 :type fixnum))

(declaim (inline system-name system-components system-order))

(declaim (type hash-table *systems*))
(global-vars:define-global-var *systems* (make-hash-table :test 'eq))
(declaim (type list *system-initializers*))
(global-vars:define-global-var *system-initializers* nil)

(defmacro defsystem (name slots (&key documentation (order 0)))
  (let* ((system-name (symbolicate name '-system))
         (printer-name (symbolicate system-name '-print))
         (variable-name (symbolicate '* system-name '*))
         (ctor-name (symbolicate 'make- system-name))
         (slot-docs (mapcar
                     #'(lambda (slot)
                         (when-let (doc (getf slot :documentation))
                           `(setf (documentation
                                    #',(symbolicate system-name '- (car slot))
                                    'function)
                                   ,doc)))
                     slots))
         (slot-names (mapcar #'(lambda (s) (symbolicate system-name '- (car s))) slots))
         (slot-descriptions (mapcar
                             #'(lambda (slot) (remove-from-plist slot :documentation))
                             slots)))
    `(progn
       (defun ,printer-name (object stream)
         (print-unreadable-object (object stream :type t :identity t)))
       (defstruct (,system-name
                   (:include system (order ,order) (name ',name))
                   (:constructor ,(symbolicate '%make- system-name))
                   (:print-object ,printer-name)
                   (:copier nil)
                   (:predicate nil))
         ,documentation
         ,@slot-descriptions)
       (declaim (inline ,@slot-names))
       ,@slot-docs
       (global-vars:define-global-var ,variable-name nil)
       (declaim (type ,system-name ,variable-name))
       (defun ,ctor-name ()
         (let ((system (,(symbolicate '%make- system-name))))
           (system-create system)
           (setf ,variable-name system)))
       (setf *system-initializers*
             (merge 'list (list (cons ,order #',ctor-name)) *system-initializers*
                    #'(lambda (s1 s2) (> (car s1) (car s2))))))))

(defun initialize-systems ()
  (dolist (system (mapcar
                   #'(lambda (initializer)
                       (let ((system (funcall (cdr initializer))))
                         (setf (gethash (make-keyword (system-name system)) *systems*) system)))
                   *system-initializers*))
    (system-initialize system)))

(defmacro with-system-slots ((slots system-type &optional (system-instance nil)
                              &key (read-only t)) &body body)
  (with-gensyms (system)
    (let* ((instance (if system-instance system-instance (symbolicate '* system-type '*)))
           (accessors (mapcar #'(lambda (s) (symbolicate system-type '- s)) slots))
           (accessor-calls (mapcar #'(lambda (a) (list a system)) accessors))
           (let-clauses (mapcar #'list slots accessor-calls)))
      (dolist (a accessors)
        (unless (find-symbol (string a))
          (error "No such slot ~s in ~s" a system-type)))
      `(let ((,system ,instance))
         (,(if read-only 'let 'symbol-macrolet) (,@let-clauses)
          ,@body)))))

(defgeneric system-create (system)
  (:documentation "Low-level method to properly initialize SYSTEM. Not meant to be redefined."))

(defmethod system-create ((system system))
  (declare (ignore system)))

(defgeneric system-initialize (system)
  (:documentation "Performs early SYSTEM initialization."))

(defmethod system-initialize ((system system))
  (declare (ignore system)))

(defgeneric system-finalize (system)
  (:documentation "Performs SYSTEM finalization."))

(defmethod system-finalize ((system system))
  (declare (ignore system)))

(defgeneric system-update (system dt)
  (:documentation "Updates system SYSTEM for time step DT (usually fixed by liballegro around 1/60 of second)."))

(defmethod system-update ((system system) dt)
  (declare (ignore system) (ignore dt)))

(defgeneric system-draw (system renderer)
  (:documentation "Renders system SYSTEM using functional renderer RENDERER.

See RENDER"))

(defmethod system-draw ((system system) renderer)
  (declare (ignore system) (ignore renderer)))

(declaim (type (integer 0 #.array-dimension-limit) *entities-count*))
(global-vars:define-global-var *entities-count* 0)

(declaim (type (integer 0 #.array-dimension-limit) *entities-allocated*))
(global-vars:define-global-var *entities-allocated* 144)

(declaim (type (vector fixnum) *deleted-entities*))
(global-vars:define-global-var *deleted-entities*
    (make-array 0 :element-type 'fixnum :adjustable t :fill-pointer t))

(defun unregister-all-systems ()
  (setf *entities-count* 0
        *entities-allocated* 144)
  (clrhash *systems*)
  (setf (fill-pointer *deleted-entities*) 0))

(defmacro with-systems (var &body body)
  "Executes BODY in loop for each system, binding system instance to variable VAR."
  `(loop :for ,var :being :the :hash-value :of *systems*
         :do ,@body))

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

(defunl make-entity ()
  "Allocates new entity."
  (if (emptyp *deleted-entities*)
      (let ((res *entities-count*))
        (incf *entities-count*)
        (when (= *entities-count* *entities-allocated*)
          (setf *entities-allocated* (round (* *entities-allocated* +array-growth-factor+)))
          (log-debug "Adjusting component allocated size to ~a" *entities-allocated*)
          (with-systems system
            (system-adjust-components system *entities-allocated*)))
        res)
      (vector-pop *deleted-entities*)))

(defun delete-entity (entity)
  "Deletes entity ENTITY."
  (issue entity-deleted :entity entity)
  (loop :for system :being :the :hash-value :of *systems*
        :when (has-component-p system entity)
        :do (delete-component system entity))
  (vector-push-extend entity *deleted-entities*))

(defun make-object (spec)
  "Creates a new game object following specification SPEC structured as follows:
```
'((:system-name1 :component-parameter1 \"value1\" :component-parameter2 2.0)
  (:system-name2 :prefab :prefab-name)
  ;; ...
  )
```"
  (loop :with entity := (make-entity)
        :for component :in spec
        :for system := (gethash (car component) *systems*)
        :for parameters := (cdr component)
        :unless system
          :do (error "No such system: ~s" (car component))
        :do (apply #'make-component system entity parameters)
        :finally (return entity)))

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

(defgeneric prefab (system prefab-name)
  (:documentation "Returns prefab with name symbol PREFAB-NAME within system SYSTEM."))

(defgeneric (setf prefab) (new-prefab system prefab-name)
  (:documentation "Sets prefab NEW-PREFAB with name symbol PREFAB-NAME within system SYSTEM."))

(defgeneric prefab-path (system prefab-name)
  (:documentation "Returns prefab file path for system SYSTEM and prefab name symbol PREFAB-NAME."))
(defgeneric make-prefab (system prefab-name)
  (:documentation "Loads prefab with name symbol PREFAB-NAME within system SYSTEM."))

(defgeneric preload-prefabs (system)
  (:documentation "Loads all prefabs for SYSTEM to avoid in-game performance degradations."))

(defmethod preload-prefabs ((system system)))

(defmethod make-prefab :around (system prefab-name)
  (setf (prefab system prefab-name) (call-next-method)))

(defgeneric make-prefab-component (system entity prefab parameters)
  (:documentation "Creates new component using prefab instance PREFAB as a template and optional
extra parameters PARAMETERS within system SYSTEM for entity ENTITY."))

(defmethod make-component :around (system entity &rest parameters)
  (destructuring-bind (&rest rest-parameters &key (prefab nil) &allow-other-keys) parameters
    (if prefab
        (make-prefab-component system entity
                               (if-let (prefab-instance (prefab system prefab))
                                 prefab-instance
                                 (make-prefab system prefab))
                               rest-parameters)
        (call-next-method))
    (issue component-created :entity entity :system-name (system-name system))))

(defmacro defprefab (system extension &rest slots)
  "Defines prefab structure with slots SLOTS and file name extension EXTENSION within system SYSTEM."
  (let ((storage-name (symbolicate '* system '- 'prefabs '*))
        (system-name (symbolicate system '- 'system))
        (struct-name (symbolicate system '- 'prefab))
        (path-format (format nil "~(~as/~~(~~a~~).~a~)" system extension))
        (ro-slots (mapcar #'(lambda (s) (append s '(:read-only t))) slots)))
    `(progn
       (global-vars:define-global-var ,storage-name (make-hash-table :test 'eq))
       (defmethod prefab ((system ,system-name) prefab-name)
         (values (gethash prefab-name ,storage-name)))
       (defmethod (setf prefab) (new-prefab (system ,system-name) prefab-name)
         (setf (gethash prefab-name ,storage-name) new-prefab))
       (defmethod prefab-path ((system ,system-name) prefab-name)
         (format nil ,path-format prefab-name))
       (defmethod preload-prefabs ((system ,system-name))
         (log-info "Preloading ~a prefabs" ',system)
         (enumerate-directory ,(format nil "~(~as~)" system)
           (when (uiop:string-suffix-p file ,(concatenate 'string "." extension))
             (make-prefab system (make-keyword (string-upcase (pathname-name file)))))))
       (defmethod make-component ((system ,system-name) entity &rest parameters)
         (declare (ignore system entity parameters))
         nil)
       (defmethod system-finalize :after ((system ,system-name))
         (clrhash ,storage-name))
       (defstruct ,struct-name
         ,@ro-slots))))

(in-package :d2clone-kit)

(defclass system ()
  ((name
    :type symbol
    :reader name
    :documentation "Symbol that denotes system.")
   components
   (order
    :type fixnum
    :initform 0
    :reader order
    :documentation "Fixnum representing system's update order."))
  (:documentation "Base class for all ECS systems."))

;; TODO : defsystem macro with global parameter = system instance?

(defgeneric system-unload (system))

(defgeneric system-update (system dt)
  (:documentation "Updates system SYSTEM for time step DT (usually fixed by liballegro around 1/60 of second)."))

(defgeneric system-draw (system renderer)
  (:documentation "Renders system SYSTEM using functional renderer RENDERER.

See RENDER"))

(defmethod system-update ((system system) dt)
  (declare (ignore system) (ignore dt)))

(defmethod system-draw ((system system) renderer)
  (declare (ignore system) (ignore renderer)))

(defvar *systems* (make-hash-table :test #'eq))

(defmethod initialize-instance :after ((system system) &key)
  (with-slots (name components) system
    (if-let (existing-sys (gethash name *systems*))
      (progn
        (log-warn "System ~a was already registered" name)
        (setf components (slot-value existing-sys 'components)))
      (setf components nil))
    (setf (gethash name *systems*) system)))

(declaim (type (integer 0 #.array-dimension-limit) *entities-count*))
(defvar *entities-count* 0)

(declaim (type (integer 0 #.array-dimension-limit) *entities-allocated*))
(defvar *entities-allocated* 144)

(declaim (type (vector fixnum) *deleted-entities*))
(defvar *deleted-entities* (make-array 0 :element-type 'fixnum :adjustable t :fill-pointer t))

(defun unregister-all-systems ()
  (setf *entities-count* 0
        *entities-allocated* 144)
  (clrhash *systems*)
  (setf (fill-pointer *deleted-entities*) 0))

(declaim (inline system-ref))
(defun system-ref (name)
  "Returns system instance by its name symbol NAME."
  (gethash name *systems*))

(defmacro with-systems (var &body body)
  "Executes BODY in loop for each system, binding system instance to variable VAR."
  (with-gensyms (systems)
    `(let ((,systems (sort (hash-table-values *systems*)
                           (lambda (s1 s2) (< (order s1) (order s2))))))
       (dolist (,var ,systems) ,@body))))

(defgeneric make-component (system entity &rest parameters)
  (:documentation "Creates new component using PARAMETERS within system SYSTEM for entity ENTITY.

PARAMETERS could include `:PREFAB` key, in which case component is constructed using corresponding prefab.

See MAKE-PREFAB-COMPONENT"))

(defgeneric system-adjust-components (system new-size))

(defgeneric delete-component (system entity))

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
  (loop for system being the hash-values of *systems*
        do (delete-component system entity))
  (vector-push-extend entity *deleted-entities*))

(defmacro defcomponent (system name &rest slots)
  "Defines component structure with name NAME and slots SLOTS within system SYSTEM."
  (let* ((system-name (symbolicate system '-system))
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
         (adjust-assignments (mapcar #'(lambda (a)
                                         (let ((acc `(,@a components)))
                                           `(setf ,acc (adjust-array ,acc new-size))))
                                     array-accessors))
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
             `(let ((,components (slot-value (gethash ',',system *systems*) 'components)))
                (symbol-macrolet (,@component-exps) ,@body)))))
       (defmacro ,(symbolicate 'with- plural-name) (&rest body)
         (with-gensyms (components)
           (let ((slot-names ',slot-names)
                 (loop-clauses (mapcan #'(lambda (s a)
                                           `(for ,s across ,`(,@a ,components)))
                                       ',slot-names ',array-accessors))
                 (component-exps (mapcar #'(lambda (s type a)
                                             `(,s (the ,type (elt ,`(,@a ,components) entity))))
                                         ',slot-names ',slot-types ',array-accessors)))
             `(let ((,components (slot-value (gethash ',',system *systems*) 'components)))
                (loop for entity from 0 below *entities-count*
                      ,@loop-clauses
                      when (and ,@slot-names)
                        do (symbol-macrolet (,@component-exps) ,@body))))))
       (defmethod initialize-instance :after ((system ,system-name) &key)
         (with-slots (components) system
           (unless components
             (setf components (,(symbolicate 'make- name))))))
       (defmethod system-adjust-components ((system ,system-name) new-size)
         (declare (type (integer 0 ,array-dimension-limit) new-size))
         (with-slots (components) system
           ,@adjust-assignments))
       (defmethod delete-component ((system ,system-name) entity)
         (with-slots (components) system
           (setf ,@delete-exprs)))
       ,@getter-decls ,@setter-decls)))

(defgeneric prefab (system prefab-name)
  (:documentation "Returns prefab with name symbol PREFAB-NAME within system SYSTEM."))

(defgeneric (setf prefab) (new-prefab system prefab-name)
  (:documentation "Sets prefab NEW-PREFAB with name symbol PREFAB-NAME within system SYSTEM."))

(defgeneric prefab-path (system prefab-name)
  (:documentation "Returns prefab file path for system SYSTEM and prefab name symbol PREFAB-NAME."))
(defgeneric make-prefab (system prefab-name)
  (:documentation "Loads prefab with name symbol PREFAB-NAME within system SYSTEM."))

(defmethod make-prefab :around (system prefab-name)
  (setf (prefab system prefab-name) (call-next-method)))

(defgeneric make-prefab-component (system entity prefab)
  (:documentation "Creates new component using prefab instance PREFAB as a template within system SYSTEM for entity ENTITY."))

(defmethod make-component :around (system entity &rest parameters)
  (destructuring-bind (&key (prefab nil) &allow-other-keys) parameters
    (if prefab
        (make-prefab-component system entity
                               (if-let (prefab-instance (prefab system prefab))
                                 prefab-instance
                                 (make-prefab system prefab)))
        (call-next-method))
    (issue component-created :entity entity :system-name (name system))))

(defmacro defprefab (system extension &rest slots)
  "Defines prefab structure with slots SLOTS and file name extension EXTENSION within system SYSTEM."
  (let ((storage-name (symbolicate '* system '- 'prefabs '*))
        (system-name (symbolicate system '- 'system))
        (struct-name (symbolicate system '- 'prefab))
        (path-format (format nil "~(~as/~~(~~a~~).~a~)" system (eval extension)))
        (ro-slots (mapcar #'(lambda (s) (append s '(:read-only t))) slots)))
    `(progn
       (defparameter ,storage-name (make-hash-table :test 'eq))
       (defmethod prefab ((system ,system-name) prefab-name)
         (gethash prefab-name ,storage-name))
       (defmethod (setf prefab) (new-prefab (system ,system-name) prefab-name)
         (setf (gethash prefab-name ,storage-name) new-prefab))
       (defhandler ,system-name quit (event)
         :after '(:end)
         (clrhash ,storage-name))
       (defmethod prefab-path ((system ,system-name) prefab-name)
         (format nil ,path-format prefab-name))
       (defstruct ,struct-name
         ,@ro-slots))))

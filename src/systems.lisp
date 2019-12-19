(in-package :d2clone-kit)

(defclass system ()
  ((name
    :type symbol
    :reader name)
   components
   (loadedp
    :type boolean
    :initform nil
    :reader loadedp)))

;; TODO : defsystem macro with global parameter = system instance?

(defgeneric system-load (system))

(defgeneric system-unload (system))

(defgeneric system-event (system event-type event))

(defgeneric system-update (system dt))

(defgeneric system-draw (system renderer))

(defgeneric system-quit (system))

(defmethod system-load :around ((system system))
  (setf (slot-value system 'loadedp) (call-next-method)))

(defmethod system-event ((system system) event-type event)
  (declare (ignore system) (ignore event-type) (ignore event))
  t)

(defmethod system-update ((system system) dt)
  (declare (ignore system) (ignore dt)))

(defmethod system-draw ((system system) renderer)
  (declare (ignore system) (ignore renderer)))

(defmethod system-quit ((system system))
  (declare (ignore system))
  t)

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

(defconstant +array-growth-factor+ (* 0.5d0 (1+ (sqrt 5d0))))

(defun unregister-all-systems ()
  (setf *entities-count* 0
        *entities-allocated* 144)
  (clrhash *systems*))

(declaim (inline system-ref))
(defun system-ref (name)
  (gethash name *systems*))

(defmacro with-systems (var &rest body)
  `(loop for ,var being the hash-values of *systems*
         do ,@body))

(defun broadcast-event (event-type event)
  (loop for system being the hash-values of *systems*
        always (system-event system event-type event)))

(defun broadcast-quit ()
  (not (loop for system being the hash-values of *systems*
             always (system-quit system))))

(defgeneric make-component (system entity &rest parameters))

(defgeneric system-adjust-components (system new-size))

;; TODO : удаление (дженерик в defcomponent)
;; TODO : хранить в векторе пул из удалённых entity, возвращать сначала из него
(defunl make-entity ()
  (let ((res *entities-count*))
    (incf *entities-count*)
    (when (= *entities-count* *entities-allocated*)
      (setf *entities-allocated* (round (* *entities-allocated* +array-growth-factor+)))
      (log-debug "Adjusting component allocated size to ~a" *entities-allocated*)
      (with-systems system
        (system-adjust-components system *entities-allocated*)))
    res))

(defmacro defcomponent (system name &rest slots)
  (let* ((system-name (symbolicate system '-system))
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
                        slot-ro slot-accessors array-accessors slot-types)))
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
       (defmacro ,(symbolicate 'with- name 's) (&rest body)
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
       ,@getter-decls ,@setter-decls)))

(defgeneric prefab (system prefab-name))

(defgeneric (setf prefab) (new-prefab system prefab-name))

(defgeneric prefab-path (system prefab-name))

(defgeneric make-prefab (system prefab-name))

(defmethod make-prefab :around (system prefab-name)
  (setf (prefab system prefab-name) (call-next-method)))

(defgeneric make-prefab-component (system entity prefab))

(defmethod make-component :around (system entity &rest parameters)
  (destructuring-bind (&key (prefab nil) &allow-other-keys) parameters
    (if prefab
        (make-prefab-component system entity
                               (if-let (prefab-instance (prefab system prefab))
                                 prefab-instance
                                 (make-prefab system prefab)))
        (call-next-method))))

(defmacro defprefab (system extension &rest slots)
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
       (defmethod system-quit :after ((system ,system-name))
         (declare (ignore system))
         (clrhash ,storage-name))
       (defmethod prefab-path ((system ,system-name) prefab-name)
         (format nil ,path-format prefab-name))
       (defstruct ,struct-name
         ,@ro-slots))))

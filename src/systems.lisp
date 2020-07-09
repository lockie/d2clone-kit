(in-package :d2clone-kit)


(defstruct (system
            (:constructor nil)
            (:copier nil)
            (:predicate nil))
  "Base structure for all ECS systems."
  (name nil :type symbol)
  (components-index nil)
  (components nil)
  (order 0 :type fixnum))

(declaim (inline system-name system-components-index system-components system-order))

(setf (documentation #'system-name 'function)
      "Symbol that denotes the system."
      (documentation #'system-components-index 'function)
      "Sparse index for system's entity -> component relation."
      (documentation #'system-components 'function)
      "Storage for system components."
      (documentation #'system-order 'function)
      "Fixnum representing system's update order.")

(declaim (type hash-table *systems*))
(global-vars:define-global-var *systems* (make-hash-table :test 'eq))
(declaim (type list *system-initializers*))
(global-vars:define-global-var *system-initializers* nil)

(defmacro defsystem (name slots (&key documentation (order 0)))
  "Defines an ECS system structure named NAME with SLOTS and docstring DOCUMENTATION,
along with global system instance variable.
ORDER is fixnum specifying the order for system initialization and WITH-SYTEMS macro.

See INITIALIZE-SYSTEMS
See WITH-SYSTEMS"
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
  "Initializes defined ECS systems in specified order.

See DEFSYSTEM"
  (dolist (system (mapcar
                   #'(lambda (initializer)
                       (let ((system (funcall (cdr initializer))))
                         (setf (gethash (make-keyword (system-name system)) *systems*) system)))
                   *system-initializers*))
    (system-initialize system)))

(defmacro with-system-slots ((slots system-type &optional (system-instance nil)
                              &key (read-only t)) &body body)
  "Executes BODY with bindings for slots of a system specified by SYSTEM-TYPE.
If SYSTEM-INSTANCE is NIL (the default), global system instance of type SYSTEM-TYPE is used.
If READ-ONLY is T (the default), slots are not SETF-able."
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

(defmacro with-systems (var &body body)
  "Executes BODY in loop for each system, binding system instance to variable VAR."
  `(loop :for ,var :being :the :hash-value :of *systems*
         :do ,@body))

(defun finalize-systems ()
  (with-systems system
    (system-finalize system))
  (clrhash *systems*))

(defun make-object (spec &optional parent)
  "Creates a new game object following specification SPEC structured as follows:
```
'((:system-name1 :component-parameter1 \"value1\" :component-parameter2 2.0)
  (:system-name2 :prefab :prefab-name)
  ;; ...
  )
```"
  (loop :with entity := (make-entity parent)
        :for component :in spec
        :for system := (gethash (car component) *systems*)
        :for parameters := (cdr component)
        :unless system
          :do (error "No such system: ~s" (car component))
        :do (apply #'make-component system entity parameters)
        :finally (return entity)))

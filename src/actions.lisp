(in-package :d2clone-kit)


(defsoa actions
  (index +invalid-index+ :type fixnum :documentation "")
  (type nil :type keyword :documentation "Keyword designating action type.")
  (entity +invalid-entity+ :type fixnum
                           :documentation "Entity the action belongs to.")
  (parent +invalid-index+ :type fixnum
                          :documentation "Action's parent action.")
  (child +invalid-index+ :type fixnum :documentation "Action's child action."))

(declaim (type actions *actions*))
(global-vars:define-global-var* *actions* (make-actions)
  "Actions' metadata storage.")

(declaim (type growable-vector *action-deleted-indices*))
(global-vars:define-global-var* *action-deleted-indices* (make-growable-vector)
  "The storage of deleted action indices, for reuse purposes.")

(declaim
 (inline action-type)
 (ftype (function (fixnum) keyword) action-type))
(defun action-type (action)
  "Returns keyword designating ACTION type."
  (actions-type-aref *actions* action))

(declaim
 (inline action-entity)
 (ftype (function (fixnum) fixnum) action-entity))
(defun action-entity (action)
  "Returns the entity ACTION belongs to."
  (actions-entity-aref *actions* action))

(declaim
 (inline action-parent)
 (ftype (function (fixnum) fixnum) action-parent))
(defun action-parent (action)
  "Returns ACTION's parent action."
  (actions-parent-aref *actions* action))

(declaim
 (inline action-child)
 (ftype (function (fixnum) fixnum) action-child))
(defun action-child (action)
  "Returns ACTION's child action."
  (actions-child-aref *actions* action))

(declaim (inline actions-length)
         (ftype (function () array-length) actions-length))
(defun actions-length ()
  "Returns the total current count of actions."
  (growable-vector-length (actions-index *actions*)))

(declaim
 (ftype (function (fixnum &optional (or boolean stream))) action-print))
(defun action-print (action &optional stream)
  "Prints human-readable representation of ACTION to STREAM."
  (if (index-valid-p action)
      (format stream "#<~a action ~d (global ~d), ent ~d, par ~d>"
              (string (action-type action))
              (actions-index-aref *actions* action) action
              (action-entity action) (action-parent action))
      (format stream "#<invalid action>")))

(declaim
 (inline current-action)
 (ftype (function (fixnum) fixnum) current-action))
(defun current-action (entity)
  "Returns current action for ENTITY."
  (growable-vector-ref *current-action* entity))

(declaim
 (inline (setf current-action))
 (ftype (function (fixnum array-index) fixnum) (setf current-action)))
(defun (setf current-action) (action entity)
  "Sets ACTION to be current action for ENTITY."
  (setf (growable-vector-ref *current-action* entity) action))

(declaim
 (inline current-action-of)
 (ftype (function (fixnum &key (:is keyword)) boolean) current-action-of))
(defun current-action-of (entity &key is)
  "Returns whether current action of ENTITY has the type IS."
  (let ((current-action (current-action entity)))
    (and (index-valid-p current-action)
         (eq (action-type current-action) is))))

(declaim
 (inline has-action-p)
 (ftype (function (fixnum keyword) (or null fixnum)) has-action-p))
(defun has-action-p (entity type)
  "Returns generalized boolean indicating whether action chain of ENTITY
contains action with TYPE. If it does, the return value is that action."
  (loop :for current-action := (current-action entity)
        :then (action-parent current-action)
        :while (index-valid-p current-action)
        :thereis (and (eq type (action-type current-action)) current-action)))

(defgeneric initialize-action (type index)
  (:documentation "Initializes action with given TYPE and global INDEX."))

(defmethod initialize-action ((type t) index))

(defgeneric finalize-action (type index)
  (:documentation "Finalizes action with given TYPE and global INDEX."))

(defmethod finalize-action ((type t) index))

(defgeneric execute-action-type (type))

(defgeneric do-delete-action (type index))

(defgeneric finalize-action-type (type))

(declaim (ftype (function (fixnum)) delete-action))
(defun delete-action (action)
  "Deletes ACTION from further processing."
  (do-delete-action (actions-type-aref *actions* action) action)
  (growable-vector-push *action-deleted-indices* action)
  (setf (actions-type-aref *actions* action) :deleted)
  (let ((entity (action-entity action))
        (parent (action-parent action))
        (child (action-child action)))
    (when (index-valid-p child)
      (delete-action child))
    (when (index-valid-p parent)
      (setf (actions-child-aref *actions* parent) +invalid-index+))
    (when (= action (current-action entity))
      (setf (current-action entity) parent))))

(declaim (ftype (function (fixnum)) delete-entity-actions))
(defun delete-entity-actions (entity)
  "Deletes all actions of ENTITY."
  (loop :for current-action := (current-action entity)
        :then (action-parent current-action)
        :while (index-valid-p current-action)
        :do (delete-action current-action)))

(declaim (type list *action-types*))
(global-vars:define-global-var *action-types* nil
  "List of keywords denoting registered actions' types.")

(defmacro defaction (name slots (&key documentation) &body body)
  "Defines an action named NAME with SLOTS and DOCUMENTATION. BODY
should contain action's initializer and finalizer for the optimization
of those to kick in.

See INITIALIZE-ACTION
See FINALIZE-ACTION"
  (declare (ignorable documentation))
  (with-gensyms (index storage)
    (let* ((type (make-keyword name))
           (struct (symbolicate name :-action))
           (storage-variable (symbolicate :* name :-actions*))
           (deleted-indices (symbolicate :* name :-action-deleted-indices*))
           (length-helper (symbolicate struct :s-length))
           (slot-names (mapcar #'car slots))
           (slot-defaults (mapcar #'cadr slots))
           (slot-types (mapcar #'(lambda (s) (getf s :type t)) slots))
           (slot-accessors
             (mapcar
              #'(lambda (s) `(,(symbolicate name :-action- s :-aref*)
                              ,storage ,index))
              slot-names))
           (unsafe-slot-accessors
             (mapcar
              #'(lambda (s) `(,(symbolicate name :-action- s :-aref)
                              ,storage ,index))
              slot-names)))
      `(progn
         (pushnew ,type *action-types*)
         (defsoa ,struct ,@slots)
         (declaim (type ,struct ,storage-variable))
         (global-vars:define-global-var* ,storage-variable
             (,(symbolicate :make- struct :s)))
         (declaim (type growable-vector ,deleted-indices))
         (global-vars:define-global-var* ,deleted-indices
             (make-growable-vector))
         (declaim (inline ,length-helper)
                  (ftype (function () array-length) ,length-helper))
         (defun ,length-helper ()
           (growable-vector-length
            (,(symbolicate struct :- (car slot-names)) ,storage-variable)))
         (declaim
          (ftype (function (fixnum &key (:parent fixnum)
                                   ,@(mapcar #'(lambda (n type)
                                                 (list (make-keyword n) type))
                                             slot-names slot-types)))
                 ,(symbolicate :make- struct)))
         (defmacro ,(symbolicate :with- name :-action) (global-index bindings
                                                        &body body)
           (let ((slot-exps (mapcar #'list
                                    (if bindings bindings ',slot-names)
                                    ',unsafe-slot-accessors)))
             `(let ((,',storage ,',storage-variable)
                    (,',index (actions-index-aref *actions* ,global-index)))
                (symbol-macrolet (,@slot-exps) ,@body))))
         (defmacro ,(symbolicate :with- name :-actions) (bindings &body body)
           (with-gensyms (types)
             (let ((index (first bindings)))
               `(loop
                  :with ,types := (actions-type *actions*)
                  :for ,index :of-type fixnum :from 0 :below (actions-length)
                  :when (eq (growable-vector-ref ,types ,index) ,',type)
                  :do (,',(symbolicate :with- name :-action)
                       ,index ,(rest bindings)
                       ,@body)))))
         (defmethod finalize-action-type ((type (eql ,type)))
           (setf ,storage-variable (,(symbolicate :make- struct :s))
                 ,deleted-indices (make-growable-vector)))
         ,@body
         (defun ,(symbolicate :make- struct) (entity &key
                                                     (parent +invalid-index+)
                                                     ,@(mapcar #'list
                                                               slot-names
                                                               slot-defaults))
           ,(format nil "Creates and returns a new ~a action belonging to ENTITY
 with PARENT action and other keyword arguments corresponding to action slots."
                    (substitute #\Space #\- (string-downcase type)))
           ;; NOTE: side-effect of ctor function is
           ;;  to replace current action chain with the new action
           ;;  (or make one more link to that chain)
           (unless (= (current-action entity) parent)
             (delete-entity-actions entity))
           (let ((,storage ,storage-variable)
                 (,index (if (zerop (growable-vector-length ,deleted-indices))
                             (,length-helper)
                             (growable-vector-pop ,deleted-indices)))
                 (global-index (if (zerop (growable-vector-length
                                           *action-deleted-indices*))
                                   (actions-length)
                                   (growable-vector-pop
                                    *action-deleted-indices*))))
             (setf ,@(mapcan
                      #'(lambda (a s) (list a s))
                      slot-accessors slot-names)
                   (actions-index-aref* *actions* global-index) ,index
                   (actions-type-aref* *actions* global-index) ,type
                   (actions-entity-aref* *actions* global-index) entity
                   (actions-parent-aref* *actions* global-index) parent
                   (actions-child-aref* *actions* global-index) +invalid-index+
                   (current-action entity) global-index)
             (when (index-valid-p parent)
               (setf (actions-child-aref* *actions* parent) global-index))
             ;; TODO : there's possible leak when parent != entity's
             ;;  current-action ???
             (initialize-action ,type global-index)
             global-index))
         (defmethod do-delete-action ((type (eql ,type)) index)
           (declare (type (eql ,type) type))
           (finalize-action type index)
           (growable-vector-push ,deleted-indices
                                 (actions-index-aref *actions* index)))))))

(defmacro defperformer (action bindings &body body)
  "Defines a performer function for ACTION symbol with BODY. BINDINGS is a
list of names of action type slots, except for the first element, which is the
name of global index of action being processed."
  (let ((type (make-keyword action)))
    `(defmethod execute-action-type ((type (eql ,type)))
       (,(symbolicate :with- action :-actions) ,bindings
        ,@body))))

(defun process-actions ()
  "Processes all actions."
  (dolist (type *action-types*)
    (execute-action-type type)))

(defun finalize-actions ()
  "Finalizes actions subsystem."
  (setf *actions* (make-actions))
  (setf *action-deleted-indices* (make-growable-vector))
  (dolist (type *action-types*)
    (finalize-action-type type)))

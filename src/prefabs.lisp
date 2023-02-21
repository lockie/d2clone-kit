(in-package :d2clone-kit)


;; TODO : make prefabs lazy - adding new art should not require restart!

(defgeneric prefab (system prefab-name)
  (:documentation "Returns prefab with name symbol PREFAB-NAME within SYSTEM."))

(defgeneric (setf prefab) (new-prefab system prefab-name)
  (:documentation "Sets prefab NEW-PREFAB with name symbol PREFAB-NAME within
SYSTEM."))

(defgeneric prefab-path (system prefab-name)
  (:documentation "Returns prefab file path for SYSTEM and prefab name symbol
PREFAB-NAME."))
(defgeneric make-prefab (system prefab-name)
  (:documentation "Loads prefab with name symbol PREFAB-NAME within SYSTEM."))

(defgeneric preload-prefabs (system)
  (:documentation "Loads all prefabs for SYSTEM to avoid in-game performance
degradations."))

(defmethod preload-prefabs ((system system)))

(defmethod make-prefab :around (system prefab-name)
  (setf (prefab system prefab-name) (call-next-method)))

(defgeneric make-prefab-component (system entity prefab parameters)
  (:documentation "Creates new component using prefab instance PREFAB as a
template and optional extra parameters PARAMETERS within SYSTEM for ENTITY."))

(defmethod make-component :around (system entity &rest parameters)
  (if-let (prefab (getf parameters :prefab))
    (make-prefab-component system entity
                           (if-let (prefab-instance (prefab system prefab))
                             prefab-instance
                             (make-prefab system prefab))
                           parameters)
    (call-next-method))
  (issue (component-created)
         :entity entity
         :system-name (system-name system)))

;; TODO think on using (asdf:system-relative-pathname :awesome-game) to load
;; resources from
(defmacro defprefab (system extension &rest slots)
  "Defines prefab structure with slots SLOTS and file name EXTENSION within
SYSTEM."
  ;; TODO : add possibility to add field documentation
  (let ((storage-name (symbolicate :* system :-prefabs*))
        (system-name (symbolicate system :-system))
        (struct-name (symbolicate system :-prefab))
        (path-format (format nil "~(~as/~~(~~a~~).~a~)" system extension))
        (ro-slots (mapcar #'(lambda (s) (append s '(:read-only t))) slots)))
    `(progn
       (global-vars:define-global-var ,storage-name
           (make-hash-table :test #'eq))
       (defmethod prefab ((system ,system-name) prefab-name)
         (values (gethash prefab-name ,storage-name)))
       (defmethod (setf prefab) (new-prefab (system ,system-name) prefab-name)
         (setf (gethash prefab-name ,storage-name) new-prefab))
       (defmethod prefab-path ((system ,system-name) prefab-name)
         (format nil ,path-format prefab-name))
       (defmethod preload-prefabs ((system ,system-name))
         (log-info "Preloading ~a prefabs" ',system)
         (enumerate-directory ,(format nil "~(~as~)" system)
           (when (uiop:string-suffix-p file
                                       ,(concatenate 'string "." extension))
             (when (system-ref :loading-screen)
               (set-loading-screen-text file))
             (make-prefab system (make-keyword (string-upcase
                                                (pathname-name file)))))))
       (defmethod make-component ((system ,system-name) entity &rest parameters)
         (declare (ignore system entity parameters))
         nil)
       (defmethod system-finalize :before ((system ,system-name))
         (clrhash ,storage-name))
       (defstruct ,struct-name
         ,@ro-slots))))

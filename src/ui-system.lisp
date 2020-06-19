(in-package :d2clone-kit)

(defclass ui-system (system)
  ((name :initform 'ui)
   (ui-font-small :initform nil)
   (ui-font-medium :initform nil)
   (ui-font-large :initform nil)
   (nuklear-font :initform nil)
   (nuklear-context :initform nil))
  (:documentation "Handles UI windows."))

(defcomponent ui ui
  (on nil :type boolean) ;; XXX setting this to NIL kinda breaks ECS implementation
  (function nil :type function)
  (parameters nil :type hash-table))

(defprefab ui "lisp"
  (function nil :type function)
  (parameters nil :type hash-table))

(declaim (inline ui-font-small) (ftype (function () cffi:foreign-pointer) ui-font-small))
(defun ui-font-small ()
  "Returns small variation of UI font."
  (slot-value (system-ref 'ui) 'ui-font-small))

(declaim (inline ui-font-medium) (ftype (function () cffi:foreign-pointer) ui-font-medium))
(defun ui-font-medium ()
  "Returns medium variation of UI font."
  (slot-value (system-ref 'ui) 'ui-font-medium))

(declaim (inline ui-font-large) (ftype (function () cffi:foreign-pointer) ui-font-large))
(defun ui-font-large ()
  "Returns large variation of UI font."
  (slot-value (system-ref 'ui) 'ui-font-large))

(declaim (inline ui-context) (ftype (function () cffi:foreign-pointer) ui-context))
(defun ui-context ()
  "Returns Nuklear GUI library's context."
  (slot-value (system-ref 'ui) 'nuklear-context))

(declaim (inline toggle-ui) (ftype (function (fixnum &optional boolean)) toggle-ui))
(defun toggle-ui (entity &optional (on nil on-supplied-p))
  "Toogles UI window corresponding to ENTITY ON; flushes internal UI event queue to prevent
undesired side effects like processing the same event by different windows."
  (nk:with-input (ui-context))
  (with-ui entity (currently-on)
    (setf currently-on (if on-supplied-p on (not currently-on)))))

(declaim (inline ui-on-p) (ftype (function () boolean) ui-on-p))
(defun ui-on-p ()
  "Returns boolean indicating whether any UI window is currenty shown."
  ;; XXX maybe use nk:item-is-any-active ?
  (let ((result nil))
    (with-uis
      (when on
        (setf result t)
        (loop-finish)))
    result))

(declaim (inline make-button-press-sound) (ftype (function (fixnum)) make-button-press-sound))
(defun make-button-press-sound (entity)
  "Adds button press sound component to ENTITY."
  (make-component (system-ref 'sound) entity :prefab :button-press))

(defmethod make-prefab ((system ui-system) prefab-name)
  (flet ((spec->defun (spec parameters)
           `(lambda (context entity &key ,@(mapcar
                                            #'(lambda (parameter)
                                                (ensure-symbol parameter :d2clone-kit))
                                            parameters))
              (declare (ignorable entity))
              ,spec)))
    (let* ((parameters (eval
                        (let ((*package* (find-package :d2clone-kit)))
                          (read
                           (make-instance 'character-stream
                                          :path (prefab-path system prefab-name))))))
           (spec (getf parameters :spec)))
      (remf parameters :spec)
      (make-ui-prefab
       :function
       (compile nil
                (spec->defun
                 spec
                 (loop :for (key nil) :on parameters :by #'cddr :collect key)))
       :parameters
       (plist-hash-table parameters)))))

(defmethod make-prefab-component ((system ui-system) entity prefab parameters)
  (destructuring-bind (&key (on nil) &allow-other-keys) parameters
    (with-ui entity (ui-on ui-function ui-parameters)
      (setf ui-on on)
      (setf ui-function (ui-prefab-function prefab))
      (setf ui-parameters (ui-prefab-parameters prefab)))))

(defmethod system-initialize ((system ui-system))
  (with-slots (ui-font-small ui-font-medium ui-font-large
               nuklear-font nuklear-context) system
    (with-system-config-options ((display-font display-width display-height))
      (let ((font-name (format nil "fonts/~a" display-font)))
        (setf
         ;; TODO : unhardcode font sizes
         ui-font-small (ensure-loaded #'al:load-ttf-font font-name -8 0)
         ui-font-medium (ensure-loaded #'al:load-ttf-font font-name -12 0)
         ui-font-large (ensure-loaded #'al:load-ttf-font font-name -20 0)
         nuklear-font (ensure-loaded #'nk:allegro-font-create-from-file font-name 18 0)
         nuklear-context (nk:allegro-init
                          nuklear-font
                          (al:get-current-display)
                          display-width display-height))))))

(defmethod system-update ((system ui-system) dt)
  (with-slots (nuklear-context) system
    (with-uis
        (when on
          (apply function nuklear-context entity (hash-table-plist parameters))))))

(defhandler ui-system quit (event)
  (with-slots (ui-font-small ui-font-medium ui-font-large
               nuklear-font nuklear-context) system
    (nk:allegro-font-del nuklear-font)
    (nk:allegro-shutdown)
    (al:destroy-font ui-font-small)
    (al:destroy-font ui-font-medium)
    (al:destroy-font ui-font-large)))

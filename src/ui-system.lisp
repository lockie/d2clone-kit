(in-package :d2clone-kit)


(defsystem ui
  ((font-small (cffi:null-pointer) :type cffi:foreign-pointer)
   (font-medium (cffi:null-pointer) :type cffi:foreign-pointer)
   (font-large (cffi:null-pointer) :type cffi:foreign-pointer)
   (nuklear-font (cffi:null-pointer) :type cffi:foreign-pointer)
   (nuklear-context (cffi:null-pointer) :type cffi:foreign-pointer))
  (:documentation "Handles UI windows."))

(defcomponent (ui)
  (on nil :type boolean)
  (function nil :type function)
  (parameters nil :type hash-table))

(defprefab ui "lisp"
  (function nil :type function)
  (parameters nil :type hash-table))

(declaim (inline ui-font-small) (ftype (function () cffi:foreign-pointer) ui-font-small))
(defun ui-font-small ()
  "Returns small variation of UI font."
  (ui-system-font-small *ui-system*))

(declaim (inline ui-font-medium) (ftype (function () cffi:foreign-pointer) ui-font-medium))
(defun ui-font-medium ()
  "Returns medium variation of UI font."
  (ui-system-font-medium *ui-system*))

(declaim (inline ui-font-large) (ftype (function () cffi:foreign-pointer) ui-font-large))
(defun ui-font-large ()
  "Returns large variation of UI font."
  (ui-system-font-large *ui-system*))

(declaim (inline ui-context) (ftype (function () cffi:foreign-pointer) ui-context))
(defun ui-context ()
  "Returns Nuklear GUI library's context."
  (ui-system-nuklear-context *ui-system*))

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
  (make-component *sound-system* entity :prefab :button-press))

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
    (make-ui entity
             :on on
             :function (ui-prefab-function prefab)
             :parameters (ui-prefab-parameters prefab))))

(defmethod system-initialize ((system ui-system))
  (with-system-slots ((font-small font-medium font-large
                                  nuklear-font nuklear-context) ui-system system :read-only nil)
      (with-system-config-options ((display-font display-width display-height))
        (let ((font-name (format nil "fonts/~a" display-font)))
          (setf
           ;; TODO : unhardcode font sizes
           font-small (ensure-loaded #'al:load-ttf-font font-name -8 0)
           font-medium (ensure-loaded #'al:load-ttf-font font-name -12 0)
           font-large (ensure-loaded #'al:load-ttf-font font-name -20 0)
           nuklear-font (ensure-loaded #'nk:allegro-font-create-from-file font-name 18 0)
           nuklear-context (nk:allegro-init
                            nuklear-font
                            (al:get-current-display)
                            display-width display-height))))))

(defmethod system-finalize ((system ui-system))
  (with-system-slots ((font-small font-medium font-large nuklear-font) ui-system system)
    (nk:allegro-font-del nuklear-font)
    (nk:allegro-shutdown)
    (al:destroy-font font-small)
    (al:destroy-font font-medium)
    (al:destroy-font font-large)))

(defmethod system-update ((system ui-system) dt)
  (with-system-slots ((nuklear-context) ui-system system)
    (with-uis
        (when on
          (apply function nuklear-context entity (hash-table-plist parameters))))))

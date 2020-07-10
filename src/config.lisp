(in-package :d2clone-kit)


(declaim (type cffi:foreign-pointer *config*))
(global-vars:define-global-var *config* (cffi:null-pointer))

(defun init-config ()
  (setf *config* (al:load-config-file "config.ini"))
  (when (cffi:null-pointer-p *config*)
    (setf *config* (al:create-config))))

(declaim (inline save-config))
(defun save-config ()
  (al:save-config-file "config.ini" *config*))

(declaim
 (ftype (function (t (or keyword null) keyword) t) (setf config)))
(defun (setf config) (value section key)
  (al:set-config-value
   *config* (string (or section :||)) (string key)
   (write-to-string value))
  (save-config)
  value)

(declaim
 (ftype (function ((or keyword null) keyword &optional t) t) config))
(defun config (section key &optional default)
  (if-let (value (al:get-config-value
                  *config* (string (or section :||)) (string key)))
    (read-from-string value)
    (setf (config section key) default)))

(defun close-config ()
  (save-config)
  (al:destroy-config *config*)
  (setf *config* (cffi:null-pointer)))

;; TODO : come up with some sort of caching to global var, calling al_get_config_value is not free
(defmacro defoptions (name &rest options)
  "Defines macro to access given group of options. E.g. when NAME is 'SYSTEM, it defines WITH-SYSTEM-CONFIG-OPTIONS macro. OPTIONS should be list of lists containing option's section name, option's name, and :TYPE and :DEFAULT properties.

See WITH-SYSTEM-CONFIG-OPTIONS"
  (let* ((section-names (mapcar #'car options))
         (key-names (mapcar #'cadr options))
         (option-names (mapcar #'(lambda (s k) (symbolicate s :- k))
                               section-names key-names))
         (option-types (mapcar #'(lambda (o) (getf o :type)) options))
         (option-defaults (mapcar #'(lambda (o) (getf o :default)) options))
         (let-clauses (mapcar
                       #'(lambda (o s k type d)
                           `(,o . ((the ,type
                                        (config ,(make-keyword s) ,(make-keyword k) ,d)))))
                       option-names section-names key-names
                       option-types option-defaults)))
    `(defmacro ,(symbolicate 'with- name '-config-options) ((options &key (read-only t)) &body body)
       "Executes BODY with bindings for config options OPTIONS. If READ-ONLY is T (the default), options are not SETF-able."
       (let ((let-clauses
               (remove-if-not
                #'(lambda (c) (find (car c) options))
                '(,@let-clauses))))
         `(,(if read-only 'let 'symbol-macrolet) (,@let-clauses)
           ,@body)))))

(defoptions system
  (display width :type fixnum :default 800)
  (display height :type fixnum :default 600)
  (display windowed :type boolean :default t)
  (display vsync :type boolean :default nil)
  (display fps :type boolean :default nil)
  (display multisampling :type fixnum :default 0)
  (display font :type string :default "")
  (debug profiling :type boolean :default nil)
  (debug grid :type list :default nil)
  (debug sprite :type list :default nil)
  (debug cursor :type list :default nil)
  (debug path :type list :default nil)
  (debug collisions :type list :default nil)

  )

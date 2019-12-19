(in-package :d2clone-kit)


;; TODO : макрос defsetting (ну или там defconfig), который будет заводить special переменные?
;; ну иди symbol-macrolet . плюс ещё типы дефайнить!

(defvar *config*)

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
  (setf *config* nil))

(defmacro defoptions (name &rest options)
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
  (debug grid :type list :default nil)
  (debug sprite :type list :default nil)

  )

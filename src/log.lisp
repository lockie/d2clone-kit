(in-package :d2clone-kit)

(defun init-log (data-dir &optional (level "info"))
  (let ((log-file
          (merge-pathnames
           (make-pathname :name "log" :type "txt")
           data-dir)))
    (setf (uiop:getenv "ALLEGRO_TRACE")
          (namestring log-file)))
  (al:set-config-value (al:get-system-config) "trace" "level" level))

(cffi:defcfun ("_al_trace_prefix" trace-prefix) :boolean
  (channel :string) (level :int) (file :string) (line :int) (function :string))

(cffi:defcfun ("_al_trace_suffix" trace-suffix) :void
  (msg :string) &rest)

(defvar *function-name* "")

(defmacro defunl (fname lambda-list &rest body)
  `(defun ,fname ,lambda-list
     (let ((*function-name* (quote ,fname)))
       ,@body)))

(declaim (string *last-message*)
         (fixnum *last-message-repetitions))
(defparameter *last-message* "")
(defparameter *last-message-repetitions* 0)

(defun %trace (level message args)
  (flet
      ((do-trace (level function-name message)
         (when (trace-prefix "d2clone-kit" level (string (uiop:argv0)) 0 function-name)
           (trace-suffix (concatenate 'string message (string #\newline))))))
    (let ((full-message (apply #'format (append (list nil message) args))))
      (if (string= *last-message* full-message)
          (incf *last-message-repetitions*)
          (progn
            (unless (zerop *last-message-repetitions*)
              (do-trace 1 "trace" (format nil "[last message repeated ~a times]"
                                          *last-message-repetitions*))
              (setf *last-message-repetitions* 0))
            (do-trace level (string-downcase *function-name*) full-message)
            (setf *last-message* full-message))))))

(defmacro deflog (name level)
  (let
      ((function-name (intern (concatenate 'string "LOG-" (symbol-name name)))))
    `(defun ,function-name (message &rest args)
       (%trace ,level message args))))

(deflog debug 0)

(deflog info 1)

(deflog warn 2)

(deflog error 3)

(defmacro with-condition-reporter (&rest body)
  `(handler-bind
       ((error #'(lambda (e)
                   (log-error "~a"
                              (with-output-to-string (s)
                                (uiop:print-condition-backtrace e :stream s)))
                   (unless *debugger-hook*
                     (al:show-native-message-box
                      (cffi:null-pointer) "Hey guys" "We got a big error here :("
                      (format nil "~a" e) (cffi:null-pointer) :error)))))
     ,@body))

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

(defmacro defunl (fname lambda-list &body body)
  "DEFUN wrapper which sets correct current function name for logging functions."
  (let ((docstring (when (stringp (car body)) (pop body))))
    `(defun ,fname ,lambda-list
       ,@(ensure-list docstring)
       (let ((*function-name* (quote ,fname)))
         ,@body))))

(declaim (string *last-message*)
         (fixnum *last-message-repetitions))
(global-vars:define-global-var *last-message* "")
(global-vars:define-global-var *last-message-repetitions* 0)

(defun %trace (level message args)
  (flet
      ((do-trace (level function-name message)
         (when (trace-prefix
                "d2clone-kit" level (file-namestring (or (uiop:argv0) "")) 0 function-name)
           (loop :with finalized-message := (format nil "~a~%" message)
                 :with length := (length finalized-message)
                 :for i :of-type fixnum :from 0 :to length :by 1024
                 :do (trace-suffix
                      "%s" :string (subseq finalized-message i (min length (+ i 1024))))))))
    (let ((full-message (apply #'format (list* nil message args))))
      (cond
        ((string= *last-message* full-message)
         (incf *last-message-repetitions*))
        (t
         (unless (zerop *last-message-repetitions*)
           (do-trace 1 "trace" (format nil "[last message repeated ~a times]"
                                       *last-message-repetitions*))
           (setf *last-message-repetitions* 0))
         (do-trace level (string-downcase *function-name*) full-message)
         (setf *last-message* full-message))))))

(defmacro deflog (name level docstring)
  (let
      ((function-name (intern (concatenate 'string "LOG-" (symbol-name name)))))
    `(defun ,function-name (message &rest args)
       ,@(ensure-list docstring)
       (%trace ,level message args))))

(deflog debug 0
  "Adds formatted message MESSAGE using placeholder arguments ARGS to liballegro debug channel.")

(deflog info 1
  "Adds formatted message MESSAGE using placeholder arguments ARGS to liballegro info channel.")

(deflog warn 2
  "Adds formatted message MESSAGE using placeholder arguments ARGS to liballegro warn channel.")

(deflog error 3
  "Adds formatted message MESSAGE using placeholder arguments ARGS to liballegro error channel.")

(defmacro with-condition-reporter (&body body)
  "Executes body BODY with generic error handler which puts full error condition info including backtrace to liballegro log and displays error messagebox when not in debugger."
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

(defmacro with-profiling (on marker &body body)
  "On SBCL, when the variable denoted by symbol ON is T, executes BODY with
[statistical profiler](http://www.sbcl.org/manual/#Statistical-Profiler) turned on
and then prints its results into liballegro log along with MARKER string
used to identify this profiling run among others.

Otherwise, just executes BODY."
  (let ((on-variable (symbolicate on)))
    `(if ,on-variable
         #+(or (not sbcl) windows)
         (progn
           (log-warn "Profiling is not supported with ~a on ~a"
                     (lisp-implementation-type)
                     (software-type))
           ,@body)
         #+(and sbcl (not windows))
         (progn
           (let ((was-profiling sb-sprof::*profiling*))
             (sb-sprof:start-profiling)
             (unwind-protect
                  (progn ,@body)
               (sb-sprof:stop-profiling)
               (log-info "Profiling report for ~a:~%~a~%"
                         ,marker
                         (with-output-to-string (s)
                           (sb-sprof:report  :stream s)))
               (sb-sprof:reset)
               (when was-profiling
                 (sb-sprof:start-profiling)))))
         (progn ,@body))))

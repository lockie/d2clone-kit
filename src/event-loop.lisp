(in-package :d2clone-kit)

(defclass event-loop (deeds:compiled-event-loop)
  ((deeds::front-queue :initform nil)
   (deeds::back-queue :initform nil)
   (deeds::queue-condition :initform nil)
   (deeds::queue-lock :initform nil)
   (deeds::queue-thread :initform nil))
  (:documentation "Event loop class.

To avoid overhead of managing the foreign pointers to liballegro events, we use
simple synchronous single-threaded event loop for now."))

(defmethod deeds:running ((event-delivery event-loop))
  t)

(defmethod deeds:start ((event-delivery event-loop))
  event-delivery)

(defmethod deeds:stop ((event-delivery event-loop))
  event-delivery)

(defmethod deeds:handle ((event deeds:event) (event-delivery event-loop))
  (funcall (deeds:delivery-function event-delivery) event))

(defmethod deeds:issue ((event deeds:event) (event-delivery event-loop))
  (deeds:handle event event-delivery)
  event)

(setf deeds:*standard-event-loop* (deeds:start (make-instance 'event-loop)))

(eval-when
    (:compile-toplevel
     :load-toplevel)
  (setf (macro-function 'defevent) (macro-function 'deeds:define-event)))

(defmacro issue (event-type &rest args)
  "Shorthand macro to allow more convenient issuing of events."
  `(deeds:do-issue ,event-type ,@args))

(defmacro defhandler (system event args &body options-and-body)
  "Defines event handler in SYSTEM for event type EVENT. ARGS are handler's arguments.
OPTIONS-AND-BODY are passed as is to DEEDS:DEFINE-HANDLER.

See DEEDS:DEFINE-HANDLER"
  (form-fiddle:with-body-options (body options) options-and-body
    (let ((handler-name
            (format-symbol *package* "~a-~a-HANDLER~:[~;-~]~:*~{~a~^-~}"
                           system event (remove 'quote (flatten options))))
          (system-name
            (format-symbol *package* "*~{~a~^-~}-SYSTEM*"
                           (butlast (uiop:split-string (string system) :separator '(#\-))))))
      `(deeds:define-handler (,handler-name ,event) ,args
         ,@options
         :class 'deeds:locally-blocking-handler
         (let ((system ,system-name))
           (declare (ignorable system))
           ,@body)))))

(declaim (type cffi:foreign-pointer *event-source* ))
(global-vars:define-global-var *event-source* (cffi:null-pointer))

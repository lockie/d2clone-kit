(in-package :d2clone-kit)


(defstruct (event
            (:copier nil)
            (:predicate nil))
  "Base event structure.")

(defgeneric process-event (system event)
  (:documentation "Processes EVENT by SYSTEM."))

(defmethod process-event ((system system) event)
  (declare (ignore system event)))

(declaim (type growable-vector *event-queue*))
(global-vars:define-global-var *event-queue* (make-growable-vector))

(defun process-events ()
  "Processes events collected in event queue by systems."
  (loop :with i :of-type array-index := 0
        :for length :of-type array-length := (growable-vector-length *event-queue*)
        :while (< i length)
        :do (let ((event (growable-vector-ref *event-queue* i)))
              (with-systems system
                (process-event system event)))
        :do (incf i))
  (growable-vector-clear *event-queue*))

(declaim
 (ftype (function (event)) %issue))
(defun %issue (event)
  (growable-vector-push *event-queue* event))

(defmacro issue (event-class &rest args)
  "Shorthand macro to allow more convenient issuing of events."
  `(%issue (,(symbolicate 'make- event-class) ,@args)))

(defmacro defevent (name slots (&key documentation))
  "Defines event class with NAME, SLOTS and DOCUMENTATION."
  `(defstruct (,name
               (:include event)
               (:copier nil)
               (:predicate nil))
     ,documentation
     ,@slots))

(defmacro defhandler ((system event &key (filter t)) &body body)
  "Defines event handler in SYSTEM for event type EVENT. 
FILTER could be an expression checked to be T before running BODY."
  `(defmethod process-event ((system ,system) (event ,event))
     (when ,filter
       ,@body)))

(declaim (type cffi:foreign-pointer *event-source*))
(global-vars:define-global-var *event-source* (cffi:null-pointer))

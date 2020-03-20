(in-package :d2clone-kit)


(defevent allegro-event ()
  ((event-type :initarg :event-type)
   (event :initarg :event))
  (:default-initargs
   :event-type (error "EVENT-TYPE required")
   :event (error "EVENT required"))
  (:documentation "Event class representing low-level liballegro event."))

(defevent quit ()
  ()
  (:documentation "Quit event class.

Do not handle liballegro's :DISPLAY-CLOSE event, handle this instead."))

(deeds:define-handler (broadcast-quit allegro-event) (event event-type)
  :filter '(eq event-type :display-close)
  :class 'deeds:locally-blocking-handler
  (issue quit))

(defevent component-created ()
  ((entity :initarg :entity)
   (system-name :initarg :system-name))
  (:default-initargs
   :entity (error "ENTITY required")
   :system-name (error "SYSTEM-NAME required"))
  (:documentation "Event class issued when a new component is created."))

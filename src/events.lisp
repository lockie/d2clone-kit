(in-package :d2clone-kit)


(defevent allegro-event ()
  ((event-type :initarg :event-type :reader event-type)
   (event :initarg :event :reader event))
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
  (deeds:do-issue quit))

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

(defevent component-created ()
  ((entity :initarg :entity)
   (system-name :initarg :system-name))
  (:default-initargs
   :entity (error "ENTITY required")
   :system-name (error "SYSTEM-NAME required"))
  (:documentation "Event class issued when a new component is created."))

(defevent character-moved ()
  ((entity :initarg :entity)
   (old-x :initarg :old-x)
   (old-y :initarg :old-y)
   (new-x :initarg :new-x)
   (new-y :initarg :new-y))
  (:default-initargs
   :entity (error "ENTITY required")
   :old-x (error "OLD-X required")
   :old-y (error "OLD-Y required")
   :new-x (error "NEW-X required")
   :new-y (error "NEW-Y required"))
  (:documentation "Event class issued when a character is about to move."))

(defevent entity-died ()
  ((entity :initarg :entity)
   (damage-fraction :initarg :damage-fraction))
  (:default-initargs
   :entity (error "ENTITY required")
   :damage-fraction (error "DAMAGE-FRACTION required"))
  (:documentation "Event class issued when entity's HP reaches zero."))

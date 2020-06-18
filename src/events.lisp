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

(declaim (inline exit))
(defun exit ()
  "Issues liballegro's :DISPLAY-CLOSE event."
  (cffi:with-foreign-object (event '(:struct al:any-event))
    (cffi:with-foreign-slots ((al::type al::source al::timestamp)
                              event (:struct al:any-event))
      (setf al::type :display-close
            al::source (cffi:null-pointer)
            al::timestamp (al:get-time)))
    (al:emit-user-event *event-source* event (cffi:null-pointer))))

(defevent component-created ()
  ((entity :initarg :entity)
   (system-name :initarg :system-name))
  (:default-initargs
   :entity (error "ENTITY required")
   :system-name (error "SYSTEM-NAME required"))
  (:documentation "Event class issued when a new component is created."))

(defevent entity-deleted ()
  ((entity :initarg :entity))
  (:default-initargs
   :entity (error "ENTITY required"))
  (:documentation "Event class issued when an entity is about to be deleted."))

(defevent sprite-stance-changed ()
  ((entity :initarg :entity)
   (stance :initarg :stance))
  (:default-initargs
   :entity (error "ENTITY required")
   :stance (error "STANCE required"))
  (:documentation "Event class issued when sprite stance is changed."))

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

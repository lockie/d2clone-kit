(in-package :d2clone-kit)


(defevent allegro-event
    ((type nil :type keyword)
     (struct nil :type cffi:foreign-pointer))
    (:documentation "Event class representing low-level liballegro event."))

(defun exit ()
  "Issues liballegro's :DISPLAY-CLOSE event."
  (cffi:with-foreign-object (event '(:struct al:any-event))
    (cffi:with-foreign-slots ((al::type al::source al::timestamp)
                              event (:struct al:any-event))
      (setf al::type :display-close
            al::source (cffi:null-pointer)
            al::timestamp (al:get-time)))
    (al:emit-user-event *event-source* event (cffi:null-pointer)))
  nil)

(defevent component-created
    ((entity +invalid-entity+ :type fixnum)
     (system-name nil :type symbol))
    (:documentation "Event class issued when a new component is created."))

(defevent entity-deleted
    ((entity +invalid-entity+ :type fixnum))
    (:documentation "Event class issued when an entity is about to be deleted."))

;; TODO : consider making generic "component value changed" event

(defevent sprite-stance-changed
    ((entity +invalid-entity+ :type fixnum)
     (old-stance nil :type keyword)
     (new-stance nil :type keyword))
    (:documentation "Event class issued when sprite stance is changed."))

(defevent character-moved
    ((entity +invalid-entity+ :type fixnum)
     (old-x nil :type double-float)
     (old-y nil :type double-float)
     (new-x nil :type double-float)
     (new-y nil :type double-float))
    (:documentation "Event class issued when a character is about to move."))

(defevent entity-died
    ((entity +invalid-entity+ :type fixnum)
     (damage-fraction nil :type double-float))
    (:documentation "Event class issued when entity's HP reaches zero."))

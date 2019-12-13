(in-package :d2clone-kit)

(defclass console-system (system)
  ((name :initform 'console)
   (shownp
    :initform nil
    :accessor shownp)
   (context
    :initform nil)

   ))

(defmethod system-load ((system console-system))
  ;; (setf (slot-value sys 'context) (nk:make-context))

  t)


(defmethod system-draw ((system console-system) renderer)  ;; gui arg?
  (when (shownp sys)
    ;; (with-slots (context) sys
    ;;   (nk:render-nuklear
    ;;    ))
    ))

(defmethod system-update ((system console-system) dt)
  (when (shownp sys)
    ;; (with-slots (context) sys
    ;;   (claw:c-with ((rect (:struct %nk:rect)))
    ;;     (unless (zerop
    ;;              (%nk:begin context "console" (%nk:rect (rect &) 50 50 200 200)
    ;;                         (nk:panel-mask :border :scroll-auto-hide)))
    ;;       (%nk:layout-row-static nk-context 30 80 1)
    ;;       (%nk:button-label nk-context "button")


    ;;     ;; инпут - nk_edit_string
    ;;     ;; метка - nk_label
    ;;     ;; текст - nk_edit_buffer ?..
    ;;     ;; NK_EDIT_NO_CURSOR , не NK_EDIT_SELECTABLE, не NK_EDIT_CLIPBOARD
    ;;     ;; NK_EDIT_READ_ONLY

    ;;       )
    ;;    )
    )
  )


(defmethod system-event ((system console-system) event-type event)
  ;; (when (and (keyboard-event-downp evt)
  ;;            (= (keyboard-event-keycode evt) sdl2-ffi:+sdlk-backquote+))
  ;;   (setf (shownp sys) (not (shownp sys))))
  )

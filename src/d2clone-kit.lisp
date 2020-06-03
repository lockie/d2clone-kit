(in-package :d2clone-kit)

(defunl handle-event (event)
  "Broadcasts liballegro event EVENT through ECS systems.
Returns T when EVENT is not :DISPLAY-CLOSE."
  (let ((event-type
          (cffi:foreign-slot-value event '(:union al:event) 'al::type)))
    (issue allegro-event
      :event event
      :event-type event-type)
    (not (eq event-type :display-close))))

(defunl game-loop (event-queue &key (repl-update-interval 0.3))
  "Runs game loop."
  (gc :full t)
  (log-info "Starting game loop")
  (with-system-config-options ((display-vsync display-fps))
    (let* ((vsync display-vsync)
           (renderer (make-renderer))
           (last-tick (al:get-time))
           (last-repl-update last-tick))
      (cffi:with-foreign-object (event '(:union al:event))
        (sleep 0.016)
        ;; TODO : restart to continue loop
        (loop :do
          (unless (loop :while (al:get-next-event event-queue event)
                        :always (handle-event event))
            (loop-finish))
          (let ((current-tick (al:get-time)))
            (when (> (- current-tick last-repl-update) repl-update-interval)
              (livesupport:update-repl-link)
              (setf last-repl-update current-tick))
            (when display-fps
              ;; TODO : smooth FPS counter, like in allegro examples
              (add-debug-text :fps "FPS: ~d" (round 1 (- current-tick last-tick))))
            (with-systems sys
              ;; TODO : replace system-update with event?.. maybe even system-draw too?..
              (system-update sys (- current-tick last-tick)))
            (with-systems sys
              (system-draw sys renderer))
            (al:clear-to-color (al:map-rgb 0 0 0))
            (do-draw renderer)
            (setf last-tick current-tick))
          (when vsync
            (setf vsync (al:wait-for-vsync)))
          (al:flip-display))))))

;; TODO : put this to UI subsystem?..
(defvar *small-ui-font* nil)
(defvar *medium-ui-font* nil)
(defvar *large-ui-font* nil)

(defvar *game-name*)
(defvar *new-game-object-specs*)
(defvar *config-options*)

(cffi:defcallback run-engine :int ((argc :int) (argv :pointer))
  (declare (ignore argc argv))
  (with-condition-reporter
    (let* ((dir-name (sanitize-filename *game-name*))
           (data-dir
             (merge-pathnames
              (make-pathname :directory `(:relative ,dir-name))
              (uiop:xdg-data-home))))
      (ensure-directories-exist data-dir)
      (init-log data-dir)
      (al:set-app-name dir-name)
      (al:init)
      (init-fs dir-name data-dir)
      (init-config))

    (unless (al:init-primitives-addon)
      (error "Initializing primitives addon failed"))
    (unless (al:init-image-addon)
      (error "Initializing image addon failed"))
    (al:init-font-addon)
    (unless (al:init-ttf-addon)
      (error "Initializing TTF addon failed"))
    (unless (al:install-audio)
      (error "Intializing audio addon failed"))
    (unless (al:init-acodec-addon)
      (error "Initializing audio codec addon failed"))
    (unless (al:restore-default-mixer)
      (error "Initializing default audio mixer failed"))

    (doplist (key val *config-options*)
      (apply #'(setf config) val
             (mapcar #'make-keyword
                     (uiop:split-string (string key) :separator '(#\-)))))

    (with-system-config-options
        ((display-windowed display-multisampling display-width display-height))
      (al:set-new-display-flags
       (if display-windowed
           '(:windowed)
           '(:fullscreen)))
      (unless (zerop display-multisampling)
        (al:set-new-display-option :sample-buffers 1 :require)
        (al:set-new-display-option :samples display-multisampling :require))

      (with-system-config-options ((display-font))
        (if (length= 0 display-font)
            (progn
              (log-warn "No font specified in config, loading builtin font")
              (setf *small-ui-font* (al:create-builtin-font)
                    *medium-ui-font* (al:create-builtin-font)
                    *large-ui-font* (al:create-builtin-font)))
            (let ((font-name (format nil "fonts/~a" display-font)))
              (setf *small-ui-font* (al:load-ttf-font font-name -8 0)
                    *medium-ui-font* (al:load-ttf-font font-name -12 0)
                    *large-ui-font* (al:load-ttf-font font-name -20 0))
              (when (or (cffi:null-pointer-p *small-ui-font*)
                        (cffi:null-pointer-p *medium-ui-font*)
                        (cffi:null-pointer-p *large-ui-font*))
                (log-warn "Loading ~a failed, falling back to builtin font" font-name)
                (setf *small-ui-font* (al:create-builtin-font)
                      *medium-ui-font* (al:create-builtin-font)
                      *large-ui-font* (al:create-builtin-font))))))

      (let ((display (al:create-display display-width display-height))
            (event-queue (al:create-event-queue)))
        (when (cffi:null-pointer-p display)
          (error "Initializing display failed"))
        (al:inhibit-screensaver t)
        (al:set-window-title display *game-name*)
        (al:register-event-source event-queue (al:get-display-event-source display))
        (al:install-keyboard)
        (al:register-event-source event-queue (al:get-keyboard-event-source))
        (al:install-mouse)
        (al:register-event-source event-queue (al:get-mouse-event-source))

        (al:set-new-bitmap-flags '(:video-bitmap))

        (setf *random-state* (make-random-state t))

        (unwind-protect
             (progn
               (make-instance 'debug-system)
               (make-instance 'sprite-batch-system)
               (make-instance 'collision-system)
               (make-instance 'combat-system)
               (make-instance 'item-system)
               (make-instance 'sound-system)
               (make-instance 'camera-system)
               (make-instance 'coordinate-system)
               (make-instance 'player-system)
               (make-instance 'sprite-system)
               (make-instance 'character-system)
               (make-instance 'hp-system)
               (make-instance 'mana-system)
               (make-instance 'combat-system)
               (make-instance 'mob-system)
               (make-instance 'map-system)
               (with-system-config-options ((debug-profiling))
                 (with-profiling debug-profiling
                   "game loop"
                   (game-loop event-queue))))
          (log-info "Shutting engine down")
          (issue quit)
          (al:inhibit-screensaver nil)
          (unregister-all-systems)
          (al:destroy-display display)
          (al:destroy-event-queue event-queue)
          (when *large-ui-font*
            (al:destroy-font *large-ui-font*))
          (when *medium-ui-font*
            (al:destroy-font *medium-ui-font*))
          (when *small-ui-font*
            (al:destroy-font *small-ui-font*))
          (setf *small-ui-font* (cffi:null-pointer)
                *medium-ui-font* (cffi:null-pointer)
                *large-ui-font* (cffi:null-pointer))
          (al:stop-samples)
          (al:shutdown-ttf-addon)
          (al:shutdown-font-addon)
          (al:shutdown-primitives-addon)
          (close-config)
          (close-fs)
          (al:uninstall-system)))))
  0)

(defunl start-engine (game-name new-game-object-specs &rest config)
  "Initializes and starts engine to run the game named by GAME-NAME.
NEW-GAME-OBJECT-SPECS is list of game object specifications to be created when the new game is started.
 CONFIG plist is used to override variables read from config file."
  (setf *game-name* game-name
        *new-game-object-specs* new-game-object-specs
        *config-options* config)
  (float-features:with-float-traps-masked
      (:divide-by-zero :invalid :inexact :overflow :underflow)
    (al:run-main 0 (cffi:null-pointer) (cffi:callback run-engine))))

(defun demo ()
  "Runs built-in engine demo."
  (start-engine
   "demo"
   '(((:camera)
      (:coordinate :x 0d0 :y 0d0))
     ((:player)
      (:coordinate :x 0d0 :y 0d0)
      (:sprite :prefab :heroine :layers-initially-toggled (:head :clothes))
      (:character :target-x 0d0 :target-y 0d0)
      (:hp :current 100d0 :maximum 100d0)
      (:mana :current 100d0 :maximum 100d0)
      (:combat :min-damage 1d0 :max-damage 2d0))
     ((:mob :name "Spiderant")
      (:coordinate :x 2d0 :y 2d0)
      (:sprite :prefab :spiderant :layers-initially-toggled (:body))
      (:character :target-x 1d0 :target-y 10d0 :speed 1d0)
      (:hp :current 15d0 :maximum 15d0)
      (:combat :min-damage 1d0 :max-damage 10d0))
     ;; ((:mob :name "Spiderant")
     ;;  (:coordinate :x 4d0 :y 4d0)
     ;;  (:sprite :prefab :spiderant :layers-initially-toggled (:body))
     ;;  (:character :target-x 1d0 :target-y 10d0 :speed 1d0)
     ;;  (:hp :current 50d0 :maximum 50d0))
     ;; ((:mob :name "Spiderant")
     ;;  (:coordinate :x 3d0 :y 3d0)
     ;;  (:sprite :prefab :spiderant :layers-initially-toggled (:body))
     ;;  (:character :target-x 1d0 :target-y 10d0 :speed 1d0)
     ;;  (:hp :current 50d0 :maximum 50d0))
     ((:coordinate :x 0d0 :y 0d0)
      (:map :prefab :map)))))

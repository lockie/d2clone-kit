(in-package :d2clone-kit)

(defunl handle-event (event)
  (let ((event-type
          (cffi:foreign-slot-value event '(:union al:event) 'al::type)))
    (if (eq event-type :display-close)
        (broadcast-quit)
        (broadcast-event event-type event))))

(defunl game-loop (event-queue &key (repl-update-interval 0.3))
  ;; TODO : init systems DSL style
  (make-instance 'point-system)
  (let ((camera-entity (make-entity)))
    (make-component (make-instance 'camera-system) camera-entity)
    (make-component (system-ref 'point) camera-entity :x 0 :y 0))
  (let ((map-entity (make-entity)))
    (make-component (make-instance 'map-system) map-entity :prefab 'map)
    ;; 12300 2920
    (make-component (system-ref 'point) map-entity :x 0 :y 0))
  ;; (let ((map-entity (make-entity)))
  ;;   (make-component (make-instance 'map-system) map-entity :prefab 'map2)
  ;;   (make-component (system-ref 'point) map-entity :x -192 :y -160))
  (let ((map-entity (make-entity)))
    (make-component (make-instance 'map-system) map-entity :prefab 'map3)
    (make-component (system-ref 'point) map-entity :x -576 :y 0))
  (let ((sprite-entity (make-entity)))
    (make-component (make-instance 'sprite-system) sprite-entity :prefab 'heroine)
    (toggle-layer sprite-entity 'head t)
    (toggle-layer sprite-entity 'cloth t)
    (make-component (make-instance 'player-system) sprite-entity)
    (make-component (system-ref 'point) sprite-entity :x 0 :y 0))
  (make-entity)

  (with-systems sys
    (unless (system-load sys)
      (error "Failed to initialize ~a system" (name sys))))
  (gc :full t)
  (log-info "Starting game loop")
  (with-config-options (display-vsync display-fps)
    (let* ((vsync display-vsync)
           (builtin-font (al:create-builtin-font))
           (white (al:map-rgb 255 255 255))
           (event (cffi:foreign-alloc '(:union al:event)))
           (failed-systems '())
           (renderer (make-renderer))
           (last-tick (al:get-time))
           (last-repl-update last-tick))
      (sleep 0.1)
      (loop do
        (unless (loop while (al:get-next-event event-queue event)
                      always (handle-event event))
          (loop-finish))
        (let ((current-tick (al:get-time)))
          (when (> (- current-tick last-repl-update) repl-update-interval)
            (livesupport:update-repl-link)
            (setf last-repl-update current-tick))
          (with-systems sys
            (let ((name (name sys)))
              (unless (loadedp sys)
                (system-load sys))
              (if (loadedp sys)
                  (progn
                    (when (member name failed-systems)
                      (log-info "System ~a reloaded" name)
                      (setf failed-systems
                            (delete name failed-systems)))
                    (system-update
                     sys (- current-tick last-tick))
                    (system-draw sys renderer))
                  (unless (member name failed-systems)
                    (setf failed-systems
                          (adjoin name failed-systems))
                    (log-error "System ~a failed to reload" name)))))
          (al:clear-to-color (al:map-rgb 0 0 0))
          (al:hold-bitmap-drawing t)
          (do-draw renderer)
          (al:hold-bitmap-drawing nil)
          (when display-fps
            ;; TODO : smooth FPS counter, like in allegro examples
            (al:draw-text
             builtin-font white 0 0 0
             (format nil "FPS: ~d" (round 1 (- current-tick last-tick)))))
          (setf last-tick current-tick))
        (when vsync
          (setf vsync (al:wait-for-vsync)))
        (al:flip-display))
      (cffi:foreign-free event))))

(defunl start-engine (game-name)
  (let ((data-dir
          (merge-pathnames
           (make-pathname :directory `(:relative ,game-name))
           (uiop:xdg-data-home))))
    (ensure-directories-exist data-dir)
    (init-log data-dir)
    (al:set-app-name game-name)
    (al:init)
    (init-fs game-name data-dir)
    (init-config))
  (unless (al:init-image-addon)
    (error "Initializing image addon failed"))
  (al:init-font-addon)
  (unless (al:init-ttf-addon)
    (error "Initializing TTF addon failed"))
  (unless (al:install-audio)
    (error "Intializing audio addon failed"))
  (unless (al:init-acodec-addon)
    (error "Initializing audio codec addon failed"))

  (with-config-options (display-windowed display-multisampling display-width display-height)
    (al:set-new-display-flags
     (if display-windowed
         '(:windowed)
         '(:fullscreen)))
    (unless (zerop display-multisampling)
      (al:set-new-display-option :sample-buffers 1 :require)
      (al:set-new-display-option :samples display-multisampling :require))

    (let ((display (al:create-display display-width display-height))
          (event-queue (al:create-event-queue)))
      (when (cffi:null-pointer-p display)
        (error "Initializing display failed"))
      (al:inhibit-screensaver t)
      (al:set-window-title display game-name)
      (al:register-event-source event-queue (al:get-display-event-source display))
      (al:install-keyboard)
      (al:register-event-source event-queue (al:get-keyboard-event-source))
      (al:install-mouse)
      (al:register-event-source event-queue (al:get-mouse-event-source))

      (al:set-new-bitmap-flags '(:video-bitmap))

      (setf *random-state* (make-random-state t))

      (unwind-protect
           (float-features:with-float-traps-masked
               (:invalid :inexact :overflow :underflow)
             (game-loop event-queue))
        (al:inhibit-screensaver nil)
        (unregister-all-systems)
        (al:destroy-display display)
        (al:destroy-event-queue event-queue)
        (al:stop-samples)
        (close-config)
        (close-fs)
        (al:uninstall-system)))))


(defun main ()
  ;; TODO : separate thread?
  (with-condition-reporter
      (start-engine "demo")))

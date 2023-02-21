(in-package :d2clone-kit)

;; TODO : (serapeum:toggle-pretty-print-hash-table)

(declaim
 #-d2c-debug (inline ui-handle-event)
 (ftype (function (cffi:foreign-pointer) boolean) ui-handle-event))
(defun ui-handle-event (event)
  (and (ui-on-p)
       (positive-fixnum-p (the fixnum (nk:allegro-handle-event event)))))

(declaim
 #-d2c-debug (inline systems-handle-event)
 (ftype (function (cffi:foreign-pointer) boolean) systems-handle-event))
(defun systems-handle-event (event)
  (let* ((type (cffi:foreign-slot-value event '(:union al:event) 'al::type))
         (allegro-event (make-allegro-event :type type :struct event)))
    ;; NOTE : processing allegro event without queueing, because event struct
    ;; is stack allocated
    (with-systems system
      (process-event system allegro-event))
    (not (eq type :display-close))))

(declaim
 (ftype
  (function (cffi:foreign-pointer &key (:repl-update-interval double-float)))
  game-loop))
(defunl game-loop (event-queue &key (repl-update-interval 0.3d0))
  "Runs game loop."
  (gc :full t)
  (log-info "Starting game loop")
  (livesupport:setup-lisp-repl)
  (uiop:nest
   (with-system-config-options ((display-vsync display-fps)))
   (let* ((vsync display-vsync)
          (renderer (make-renderer))
          (last-tick (the double-float (al:get-time)))
          (last-repl-update last-tick)))
   (cffi:with-foreign-object (event '(:union al:event))
     (sleep 0.016)
     (loop
       :do (restart-case
               (progn
                 (nk:with-input (ui-context)
                   (unless (loop :while (al:get-next-event event-queue event)
                                 :always (or (ui-handle-event event)
                                             (systems-handle-event event)))
                     (loop-finish)))
                 (process-events)
                 (let ((current-tick (the double-float (al:get-time))))
                   (when (> (- current-tick last-repl-update)
                            repl-update-interval)
                     (livesupport:update-repl-link)
                     (setf last-repl-update current-tick))
                   (setf *delta-time* (- current-tick last-tick))
                   ;; TODO : draw FPS counter above the UI
                   (when display-fps
                     ;; TODO : smooth FPS counter, like in allegro examples
                     (add-debug-text :fps "FPS: ~d" (round 1 *delta-time*)))
                   (process-actions)
                   ;; TODO : use separate threads for updating?..
                   (with-systems sys
                     ;; TODO : replace system-update with event?.. maybe even
                     ;; system-draw too?..
                     (system-update sys))
                   (with-systems sys
                     (system-draw sys renderer))
                   (al:clear-to-color (al:map-rgb 0 0 0))
                   (do-draw renderer)
                   (setf last-tick current-tick))
                 (when vsync
                   (setf vsync (al:wait-for-vsync)))
                 (nk:allegro-render)
                 (al:flip-display))
             ;; TODO restart to terminate the loop
             (next-iteration ()
               :report "Proceed to next game loop iteration."
               nil))))))

(defvar *game-name*)
(defvar *sanitized-game-name*)
(defvar *new-game-object-specs*)
(defvar *config-options*)
(defvar *table-indices*)

(defun new-game ()
  "Starts new game."
  (log-info "Starting new game")
  (when (entity-valid-p *session-entity*)
    (delete-entity *session-entity*))
  (growable-vector-clear *event-queue*)
  (setf (player-system-last-target *player-system*) +invalid-entity+)
  (setf *session-entity* (make-entity))
  (dolist (spec *new-game-object-specs*)
    (make-object spec *session-entity*)))

(declaim
 #-d2c-debug (inline game-started-p)
 (ftype (function () boolean) game-started-p))
(defun game-started-p ()
  "Returns boolean indicating whether the game session is currently running."
  ;; HACK
  (entity-valid-p (player-system-entity *player-system*)))

(declaim
 (ftype (function (string (or character symbol string)) (or string null))
        package-version))
(defun package-version (format package)
  "Returns the PACKAGE version from asdf formatted according to FORMAT. If the
package does not exist, then retuns NIL."
  (values
   (when-let (package-instance (asdf:find-system package nil))
     (format nil format (slot-value package-instance 'asdf:version)))))

(cffi:defcallback run-engine :int ((argc :int) (argv :pointer))
  (declare (ignore argc argv))
  (with-condition-reporter
    (let ((data-dir
            (merge-pathnames
             (make-pathname :directory `(:relative ,*sanitized-game-name*))
             (uiop:xdg-data-home))))
      (ensure-directories-exist data-dir)
      ;; TODO : ability to set level e.g. by command line variable
      ;;  (it is not possible through config because of "deadlock")
      (init-log data-dir)
      (al:set-app-name *sanitized-game-name*)
      (al:init)
      (log-info "Starting d2clone-kit engine ~a"
                (package-version "v~a" :d2clone-kit))
      (init-fs *sanitized-game-name* data-dir)
      (init-config))

    ;; TODO : proper recover from those errors (properly finalize)+retry restart
    (unless (al:init-primitives-addon)
      (error "Initializing primitives addon failed"))
    (unless (al:init-image-addon)
      (error "Initializing image addon failed"))
    (al:init-font-addon)
    (unless (al:init-ttf-addon)
      (error "Initializing TTF addon failed"))
    ;; NOTE autodetected pulseaudio driver crashes when initialized
    ;; several times in the same process (REPL case), so forcing
    ;; OpenAL instead
    ;; TODO : refactor setting system config option (same in log.lisp)
    (al:set-config-value (al:get-system-config) "audio" "driver" "openal")
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
       ;; TODO : fix fullscreen
       (if display-windowed
           '(:windowed)
           '(:fullscreen-window :frameless)))
      (unless (zerop display-multisampling)
        (al:set-new-display-option :sample-buffers 1 :require)
        (al:set-new-display-option :samples display-multisampling :require))

      (let ((display (al:create-display display-width display-height))
            (event-queue (al:create-event-queue)))
        (when (cffi:null-pointer-p display)
          (error "Initializing display failed"))
        (al:inhibit-screensaver t)
        (al:set-window-title display *game-name*)
        (al:register-event-source event-queue
                                  (al:get-display-event-source display))
        (al:install-keyboard)
        (al:register-event-source event-queue (al:get-keyboard-event-source))
        (al:install-mouse)
        (al:register-event-source event-queue (al:get-mouse-event-source))
        (setf *event-source* (cffi:foreign-alloc '(:struct al::event-source)))
        (al:init-user-event-source *event-source*)
        (al:register-event-source event-queue *event-source*)

        (al:set-new-bitmap-flags '(:video-bitmap))

        (setf *random-state* (make-random-state t))

        (unwind-protect
             (progn
               (load-data-tables *table-indices*)
               (initialize-systems)
               (game-loop event-queue))
          (log-info "Shutting engine down")
          (when (entity-valid-p *session-entity*)
            (delete-entity *session-entity*))
          (setf *session-entity* +invalid-entity+)
          (finalize-systems)
          (finalize-entities)
          (finalize-actions)
          (growable-vector-clear *event-queue*)
          (al:inhibit-screensaver nil)
          (al:destroy-user-event-source *event-source*)
          (cffi:foreign-free *event-source*)
          (setf *event-source* (cffi:null-pointer))
          (al:destroy-event-queue event-queue)
          (al:destroy-display display)
          (al:stop-samples)
          (close-config)
          (al:uninstall-system)
          (al:uninstall-audio)
          (al:shutdown-ttf-addon)
          (al:shutdown-font-addon)
          (al:shutdown-image-addon)
          (al:shutdown-primitives-addon)
          (close-fs)))))
  0)

(defunl start-engine (game-name new-game-object-specs table-indices
                                &rest config)
  "Initializes and starts engine to run the game named by GAME-NAME.
NEW-GAME-OBJECT-SPECS is list of game object specifications to be created when
 the new game is started with MAKE-OBJECT. TABLE-INDICES, if non-nil,
 specifies the columns in indices to build from a data tables read from .cdb
 file with BUILD-DATA-TABLES. CONFIG plist is used to override variables read
 from config file.

See MAKE-OBJECT
See BUILD-DATA-TABLES"
  (let ((*game-name* game-name)
        (*sanitized-game-name* (sanitize-filename game-name))
        (*new-game-object-specs* new-game-object-specs)
        (*config-options* config)
        (*table-indices* table-indices))
    (float-features:with-float-traps-masked
        (:divide-by-zero :invalid :inexact :overflow :underflow)
      (al:run-main 0 (cffi:null-pointer) (cffi:callback run-engine)))))

(defun demo ()
  "Runs built-in engine demo."
  (start-engine
   "demo"
   ;; TODO : load that from map file!
   ;; TODO : also store initial player position in the file
   ;;  (or some kind of "entrypoint" or "spawnpoint")
   '(((:camera)
      (:coordinate :x 0d0 :y 0d0))
     ((:player)
      (:coordinate :x 0d0 :y 0d0)
      (:sprite :prefab :heroine :layers-initially-toggled (:head :clothes))
      (:character)
      (:hp :current 100d0 :maximum 100d0)
      (:mana :current 100d0 :maximum 100d0)
      (:combat :min-damage 1d0 :max-damage 2d0))
     ((:mob :name "Spiderant")
      (:coordinate :x 2d0 :y 2d0)
      (:sprite :prefab :spiderant :layers-initially-toggled (:body))
      (:character :speed 1d0)
      (:hp :current 15d0 :maximum 15d0)
      (:combat :min-damage 1d0 :max-damage 10d0))
     ;; ((:mob :name "Spiderant")
     ;;  (:coordinate :x 4d0 :y 4d0)
     ;;  (:sprite :prefab :spiderant :layers-initially-toggled (:body))
     ;;  (:character :speed 1d0)
     ;;  (:hp :current 50d0 :maximum 50d0))
     ;; ((:mob :name "Spiderant")
     ;;  (:coordinate :x 3d0 :y 3d0)
     ;;  (:sprite :prefab :spiderant :layers-initially-toggled (:body))
     ;;  (:character :speed 1d0)
     ;;  (:hp :current 50d0 :maximum 50d0))
     ((:coordinate :x 0d0 :y 0d0)
      (:map :prefab :map)))
   nil))

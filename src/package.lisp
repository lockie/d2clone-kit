(in-package :cl-user)

(defpackage :d2clone-kit
  (:documentation "Generic Diablo 2 clone game engine.")
  (:nicknames #:d2c)
  (:use :cl :alexandria :iterate :trivial-garbage :parse-float :xmls)
  (:import-from :cl-containers :priority-queue-on-container :insert-item
   :iterate-elements :empty!)
  (:import-from :make-hash :make-hash)
  (:shadow character)
   ;; camera-system.lisp
   (:export
    #:camera-system
    #:camera-entity
    #:camera-target
    #:with-camera
    #:absolute->viewport
    #:viewport->absolute
    #:visiblep
    #:range-visible-p)
   ;; character-system.lisp
   (:export
    #:character-system)
   ;; config.lisp
   (:export
    #:defoptions
    #:with-system-config-options)
   ;; coordinate-system.lisp
   (:export
    #:coordinate-system
    #:world->screen
    #:with-screen-coordinate)
   ;; d2clone-kit.lisp
   (:export
    #:handle-event
    #:game-loop
    #:start-engine
    #:demo)
   ;; debug-system.lisp
   (:export
    #:debug-system
    #:add-debug-point
    #:add-debug-rectangle
    #:add-debug-tile-rhomb)
   ;; fs.lisp
   (:export
    #:character-stream
    #:binary-stream
    #:virtual-binary-stream
    #:read-binary
    #:define-binary-struct)
   ;; log.lisp
   (:export
    #:defunl
    #:log-debug
    #:log-info
    #:log-warn
    #:log-error
    #:with-condition-reporter)
   ;; map-system.lisp
   (:export
    #:map-system
    #:map->screen
    #:screen->map
    #:screen->map*
    #:ground-layer-p)
   ;; player-system.lisp
   (:export
    #:player-system
    #:player-entity
    #:mouse-position
    #:target-player)
   ;; renderer.lisp
   (:export
    #:make-renderer
    #:render
    #:do-draw)
   ;; sprite-system.lisp
   (:export
    #:sprite-system
    #:angle
    #:toggle-layer
    #:switch-stance
    #:sprite-direction)
   ;; systems.lisp
   (:export
    #:system
    #:system-event
    #:system-update
    #:system-draw
    #:system-quit
    #:system-ref
    #:with-systems
    #:make-component
    #:make-entity
    #:delete-entity
    #:defcomponent
    #:prefab
    #:prefab-path
    #:make-prefab
    #:make-prefab-component
    #:defprefab))

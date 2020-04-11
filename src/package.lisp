(in-package :cl-user)

(defpackage :d2clone-kit
  (:documentation "Generic Diablo 2 clone game engine.")
  (:nicknames #:d2c)
  (:use :cl :alexandria :trivial-garbage :parse-float :xmls)
  (:import-from :make-hash :make-hash)
  (:import-from :cl-inflector :plural-of)
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
    #:character-system
    #:a*
    #:set-character-target)
   ;; collision-system.lisp
   (:export
    #:collision-system
    #:collides
    #:collidesp)
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
    #:add-debug-tile-rhomb
    #:add-debug-text)
   ;; event-loop.lisp
   (:export
    #:event-loop
    #:defevent
    #:issue
    #:defhandler)
   ;; events.lisp
   (:export
    #:allegro-event
    #:quit
    #:component-created)
   ;; fs.lisp
   (:export
    #:character-stream
    #:binary-stream
    #:virtual-binary-stream
    #:read-binary
    #:define-binary-struct)
   ;; growable-vector.lisp
   (:export
    #:growable-vector
    #:make-growable-vector
    #:growable-vector-ref
    #:%growable-vector-ref
    #:growable-vector-length
    #:growable-vector-clear
    #:growable-vector-freeze)
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
    #:tile-index
    #:map->screen
    #:screen->map
    #:screen->map*
    #:ground-layer-p
    #:tile-property)
   ;; player-system.lisp
   (:export
    #:player-system
    #:player-entity
    #:mouse-position
    #:target-player)
   ;; priority-queue.lisp
   (:export
    #:priority-queue
    #:make-priority-queue
    #:priority-queue-find
    #:priority-queue-push
    #:priority-queue-push-many
    #:priority-queue-traverse
    #:priority-queue-pop
    #:priority-queue-remove
    #:priority-queue-clear)
   ;; renderer.lisp
   (:export
    #:make-renderer
    #:render
    #:do-draw)
   ;; sparse-matrix.lisp
   (:export
    #:sparse-matrix
    #:make-sparse-matrix
    #:sparse-matrix-ref
    #:sparse-matrix-remove)
   ;; sprite-batch-system.lisp
   (:export
    #:sprite-batch-system
    #:add-sprite-to-batch
    #:add-sprite-index-to-batch)
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
    #:system-update
    #:system-draw
    #:system-ref
    #:with-systems
    #:make-component
    #:make-entity
    #:delete-entity
    #:make-entity-initializer
    #:defcomponent
    #:prefab
    #:prefab-path
    #:make-prefab
    #:make-prefab-component
    #:defprefab))

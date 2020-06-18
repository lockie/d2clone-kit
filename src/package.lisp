(in-package :cl-user)

#+(and sbcl (not windows)) (require :sb-sprof)

(defpackage :d2clone-kit
  (:documentation "Generic Diablo 2 clone game engine.")
  (:nicknames #:d2c)
  ;; TODO : don't use :use, see https://git.io/Jfc8D
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
    #:face-target
    #:set-character-target
    #:stop-entity)
   ;; collision-system.lisp
   (:export
    #:collision-system
    #:character-at
    #:collides
    #:collidesp)
   ;; combat-system
   (:export
    #:combat-system
    #:attack)
   ;; config.lisp
   (:export
    #:defoptions
    #:with-system-config-options)
   ;; coordinate-system.lisp
   (:export
    #:coordinate-system
    #:orthogonal->isometric
    #:isometric->orthogonal
    #:isometric->orthogonal*
    #:isometric->screen
    #:isometric->screen*
    #:screen->isometric*
    #:orthogonal->screen
    #:screen->orthogonal*
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
    #:component-created
    #:character-moved
    #:entity-died)
   ;; fs.lisp
   (:export
    #:ensure-loaded
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
    ;; hp-system.lisp
   (:export
    #:hp-system
    #:set-hp
    #:deadp)
   ;; log.lisp
   (:export
    #:defunl
    #:log-debug
    #:log-info
    #:log-warn
    #:log-error
    #:with-condition-reporter
    #:with-profiling)
   ;; mana-system.lisp
   (:export
    #:mana-system)
   ;; map-system.lisp
   (:export
    #:map-system
    #:ground-layer-p
    #:tile-property)
   (:export
    #:mob-system)
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
   ;; sound-system.lisp
   (:export
    #:sound-system)
   ;; sparse-matrix.lisp
   (:export
    #:sparse-matrix
    #:make-sparse-matrix
    #:sparse-matrix-ref
    #:sparse-matrix-remove
    #:sparse-matrix-traverse)
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
    #:stance-interruptible-p
    #:switch-stance
    #:sprite-direction)
   ;; systems.lisp
   (:export
    #:system
    #:system-initialize
    #:system-update
    #:system-draw
    #:system-ref
    #:with-systems
    #:make-component
    #:delete-component
    #:has-component-p
    #:make-entity
    #:delete-entity
    #:make-object
    #:defcomponent
    #:prefab
    #:prefab-path
    #:make-prefab
    #:preload-prefabs
    #:make-prefab-component
    #:defprefab))

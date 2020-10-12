(in-package :cl-user)

#+(and sbcl (not windows)) (require :sb-sprof)

(cl-environments:enable-hook)

(defpackage :d2clone-kit
  (:documentation "Generic Diablo 2 clone game engine.")
  (:nicknames #:d2c)
  ;; TODO : don't use :use, see https://git.io/Jfc8D
  (:use #+docs :cl #-docs :static-dispatch-cl :alexandria :trivial-garbage :parse-float :xmls)
  (:import-from :make-hash :make-hash)
  (:import-from :cl-inflector :plural-of)
  (:shadow character)
   ;; actions.lisp
   (:export
    #:current-action
    #:current-action-of
    #:has-action-p
    #:action-type
    #:action-entity
    #:action-parent
    #:action-child
    #:actions-length
    #:action-print
    #:initialize-action
    #:finalize-action
    #:delete-action
    #:delete-entity-actions
    #:defaction
    #:defperformer
    #:process-actions
    #:finalize-actions)
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
    #:move)
   ;; collision-system.lisp
   (:export
    #:collision-system
    #:character-at
    #:collidesp)
   ;; combat-system
   (:export
    #:combat-system
    #:attack)
   ;; components.lisp
   (:export
    #:make-component
    #:delete-component
    #:has-component-p
    #:defcomponent)
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
   ;; credits-system.lisp
   (:export
    #:credits-system
    #:credits-screen)
   ;; d2clone-kit.lisp
   (:export
    #:handle-event
    #:*delta-time*
    #:game-loop
    #:new-game
    #:game-started-p
    #:start-engine
    #:demo)
   ;; debug-system.lisp
   (:export
    #:debug-system
    #:add-debug-point
    #:add-debug-rectangle
    #:add-debug-tile-rhomb
    #:add-debug-text)
   ;; entities.lisp
   (:export
    #:make-entity
    #:delete-child
    #:delete-entity
    #:+invalid-entity+
    #:entity-valid-p)
   ;; event-queue.lisp
   (:export
    #:event
    #:process-event
    #:process-events
    #:issue
    #:defevent
    #:defhandler)
   ;; events.lisp
   (:export
    #:allegro-event
    #:exit
    #:component-created
    #:entity-deleted
    #:sprite-stance-changed
    #:character-moved
    #:entity-died)
   ;; fs.lisp
   (:export
    #:ensure-loaded
    #:read-file-into-list
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
    #:growable-vector-grow
    #:growable-vector-ref*
    #:growable-vector-length
    #:growable-vector-emptyp
    #:growable-vector-push
    #:growable-vector-pop
    #:growable-vector-add
    #:growable-vector-clear
    #:growable-vector-freeze)
    ;; hp-system.lisp
   (:export
    #:hp-system
    #:set-hp
    #:deadp)
   ;; item-system.lisp
   (:export
    #:item-system
    #:+item-pickup-range+
    #:make-item-pickup-action
    #:draw-item-text
    #:item-at
    #:drop-item
    #:pickup-item)
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
   ;; menu-system.lisp
   (:export
    #:menu-system
    #:main-menu)
   ;; mob-system.lisp
   (:export
    #:mob-system
    #:draw-mob-health-bar)
   ;; player-system.lisp
   (:export
    #:player-system
    #:player-entity
    #:mouse-position
    #:target-player)
   ;; prefabs.lisp
   (:export
    #:prefab
    #:prefab-path
    #:make-prefab
    #:preload-prefabs
    #:make-prefab-component
    #:defprefab)
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
   ;; sparse-array.lisp
   (:export
    #:+invalid-index+
    #:index-valid-p
    #:sparse-array-index
    #:make-sparse-array-index
    #:sparse-array-index-grow
    #:sparse-array-index-ref
    #:sparse-array-index-push
    #:sparse-array-index-delete
    #:do-sparse-array)
   ;; sparse-matrix.lisp
   (:export
    #:sparse-matrix
    #:make-sparse-matrix
    #:sparse-matrix-ref
    #:sparse-matrix-remove
    #:sparse-matrix-traverse
    #:sparse-matrix-clear)
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
    #:frame-finished-p
    #:stance-finished-p
    #:sprite-direction)
   ;; systems.lisp
   (:export
    #:system
    #:system-name
    #:system-components
    #:system-order
    #:defsystem
    #:initialize-systems
    #:with-system-slots
    #:system-create
    #:system-initialize
    #:system-finalize
    #:system-update
    #:system-draw
    #:with-systems
    #:make-object)
   ;; ui-system.lisp
   (:export
    #:ui-system
    #:ui-font-small
    #:ui-font-medium
    #:ui-font-large
    #:ui-context
    #:toggle-ui
    #:ui-on-p
    #:make-button-press-sound))

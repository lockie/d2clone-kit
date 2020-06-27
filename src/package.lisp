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
    #:delete-entity
    #:+invalid-entity+
    #:entity-valid-p)
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
    #:exit
    #:component-created
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
   ;; menu-system.lisp
   (:export
    #:menu-system
    #:main-menu)
   ;; mob-system.lisp
   (:export
    #:mob-system)
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

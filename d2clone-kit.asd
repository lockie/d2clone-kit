(asdf:defsystem :d2clone-kit
  :version "0.1.0"
  :description "Generic Diablo 2 clone game engine."
  :homepage "https://lockie.gitlab.io/d2clone-kit"
  :author "Andrew Kravchuk <awkravchuk@gmail.com>"
  :license "GPL v3"
  :depends-on (:alexandria
               :babel
               :chipz
               :cl-csv
               :cl-inflector
               :cl-liballegro
               :cl-liballegro-nuklear
               :cl-ppcre
               :deeds
               :float-features
               :global-vars
               :livesupport
               :make-hash
               :parse-float
               :qbase64
               :trivial-features
               :trivial-garbage
               :trivial-gray-streams
               :uiop
               :xmls)
  :pathname "src"
  :components ((:file "package")
               (:file "aseprite"
                :depends-on ("fs"))
               (:file "camera-system"
                :depends-on ("entities" "systems" "config" "coordinate-system"))
               (:file "character-system"
                :depends-on ("components" "systems" "config" "event-loop" "collision-system"
                                          "coordinate-system" "sprite-system" "priority-queue"))
               (:file "collision-system"
                :depends-on ("entities" "systems" "sparse-matrix" "config" "event-loop" "events"
                                        "coordinate-system" "map-system"))
               (:file "combat-system"
                :depends-on ("entities" "components" "systems" "character-system"
                                        "coordinate-system" "hp-system" "sprite-system"))
               (:file "components"
                :depends-on ("systems" "sparse-array"))
               (:file "config")
               (:file "coordinate-system"
                :depends-on ("components" "systems"))
               (:file "credits-system"
                :depends-on ("entities" "systems"))
               (:file "d2clone-kit"
                :depends-on ("entities" "systems" "renderer" "log" "config" "fs" "event-loop"
                                        "renderer" "player-system" "ui-system"))
               (:file "debug-system"
                :depends-on ("components" "systems" "renderer" "growable-vector"))
               (:file "entities"
                :depends-on ("components" "systems" "log" "event-loop"))
               (:file "event-loop")
               (:file "events"
                :depends-on ("event-loop"))
               (:file "fs"
                :depends-on ("log"))
               (:file "growable-vector")
               (:file "hp-system"
                :depends-on ("components" "systems" "event-loop"))
               (:file "item-system"
                :depends-on ("components" "systems" "renderer" "event-loop" "events"
                                          "combat-system" "coordinate-system" "hp-system"
                                          "player-system" "sprite-system" "ui-system"))
               (:file "log")
               (:file "mana-system"
                :depends-on ("components" "systems"))
               (:file "map-system"
                :depends-on ("components" "systems" "prefabs" "config" "camera-system"
                                          "coordinate-system" "sprite-batch-system" "tiled"))
               (:file "menu-system"
                :depends-on ("entities" "systems" "event-loop" "events" "ui-system"))
               (:file "mob-system"
                :depends-on ("entities" "components" "systems" "character-system"
                                        "coordinate-system" "hp-system"))
               (:file "player-system"
                :depends-on ("entities" "components" "systems" "config" "renderer"
                                        "event-loop" "events" "combat-system" "hp-system"
                                        "mana-system" "mob-system" "ui-system"))
               (:file "prefabs"
                :depends-on ("components" "systems" "event-loop"))
               (:file "priority-queue")
               (:file "renderer"
                :depends-on ("priority-queue"))
               (:file "sound-system"
                :depends-on ("components" "systems" "prefabs" "config" "event-loop" "events"
                                          "camera-system" "coordinate-system" "sprite-system"))
               (:file "sparse-array"
                :depends-on ("growable-vector"))
               (:file "sparse-matrix"
                :depends-on ("growable-vector"))
               (:file "sprite-batch-system"
                :depends-on ("components" "systems" "renderer"))
               (:file "sprite-system"
                :depends-on ("entities" "components" "systems" "prefabs" "event-loop" "events"
                                        "aseprite" "hp-system" "sprite-batch-system"))
               (:file "systems")
               (:file "tiled"
                :depends-on ("fs"))
               (:file "ui-system"
                :depends-on ("components" "systems" "prefabs" "sound-system")))
  :around-compile (lambda (next)
                    (when (uiop:featurep :release)
                      (proclaim '(optimize
                                  (speed 3)
                                  (debug 0)
                                  (compilation-speed 0)
                                  (safety 1))))
                    (funcall next)))

(pushnew :deeds-no-startup *features*)

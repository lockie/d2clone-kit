(asdf:defsystem :d2clone-kit
  :version "0.1.1"
  :description "Generic Diablo 2 clone game engine."
  :homepage "https://lockie.gitlab.io/d2clone-kit"
  :author "Andrew Kravchuk <awkravchuk@gmail.com>"
  :license "GPL v3"
  :depends-on (:babel
               :chipz
               :cl-csv
               :cl-inflector
               :cl-liballegro
               :cl-liballegro-nuklear
               :cl-ppcre
               :float-features
               :global-vars
               :jonathan
               :livesupport
               :make-hash
               :golden-utils
               :parse-float
               :qbase64
               :trivial-features
               :trivial-garbage
               :trivial-gray-streams
               :uiop
               :xmls)
  :pathname "src"
  :components ((:file "package")
               (:file "actions"
                :depends-on ("entities" "components" "systems"
                                        "growable-vector"))
               (:file "aseprite"
                :depends-on ("fs"))
               (:file "camera-system"
                :depends-on ("entities" "systems" "config" "coordinate-system"))
               (:file "castledb"
                :depends-on ("fs"))
               (:file "character-system"
                :depends-on ("components" "systems" "config" "event-queue"
                                          "actions" "renderer"
                                          "collision-system"
                                          "coordinate-system" "sprite-system"
                                          "priority-queue"))
               (:file "collision-system"
                :depends-on ("entities" "systems" "sparse-matrix" "config"
                                        "event-queue" "events"
                                        "coordinate-system" "map-system"))
               (:file "combat-system"
                :depends-on ("entities" "components" "systems" "data-tables"
                                        "character-system"
                                        "coordinate-system" "hp-system"
                                        "sprite-system"))
               (:file "components"
                :depends-on ("systems" "sparse-array"))
               (:file "config")
               (:file "coordinate-system"
                :depends-on ("components" "systems"))
               (:file "credits-system"
                :depends-on ("entities" "systems"))
               (:file "d2clone-kit"
                :depends-on ("entities" "systems" "renderer" "log" "config" "fs"
                                        "event-queue" "actions" "renderer"
                                        "castledb" "data-tables" "player-system"
                                        "ui-system"))
               (:file "data-tables"
                :depends-on ("log" "fs"))
               (:file "debug-system"
                :depends-on ("components" "systems" "renderer"
                                          "growable-vector"))
               (:file "entities"
                :depends-on ("components" "systems" "log" "event-queue"
                                          "growable-vector"))
               (:file "event-queue")
               (:file "events"
                :depends-on ("entities" "event-queue"))
               (:file "fs"
                :depends-on ("log"))
               (:file "growable-vector")
               (:file "hp-system"
                :depends-on ("components" "systems" "event-queue" "actions"))
               (:file "item-system"
                :depends-on ("components" "systems" "renderer" "event-queue"
                                          "events" "combat-system"
                                          "coordinate-system" "hp-system"
                                          "player-system" "sprite-system"
                                          "ui-system"))
               (:file "loading-screen-system"
                :depends-on ("entities" "systems" "ui-system"))
               (:file "log")
               (:file "mana-system"
                :depends-on ("components" "systems"))
               (:file "map-system"
                :depends-on ("components" "systems" "prefabs" "config"
                                          "camera-system" "coordinate-system"
                                          "sprite-batch-system" "tiled"))
               (:file "menu-system"
                :depends-on ("entities" "systems" "event-queue" "events"
                                        "ui-system"))
               (:file "mob-system"
                :depends-on ("entities" "components" "systems"
                                        "character-system"
                                        "coordinate-system" "hp-system"
                                        "ui-system"))
               (:file "player-system"
                :depends-on ("entities" "components" "systems" "config"
                                        "renderer"
                                        "event-queue" "events"
                                        "coordinate-system" "combat-system"
                                        "hp-system" "mana-system" "mob-system"
                                        "ui-system"))
               (:file "prefabs"
                :depends-on ("components" "systems" "event-queue"))
               (:file "priority-queue")
               (:file "renderer"
                :depends-on ("priority-queue"))
               (:file "sound-system"
                :depends-on ("components" "systems" "prefabs" "config"
                                          "event-queue" "events" "camera-system"
                                          "coordinate-system"))
               (:file "sparse-array"
                :depends-on ("growable-vector"))
               (:file "sparse-matrix"
                :depends-on ("growable-vector"))
               (:file "sprite-batch-system"
                :depends-on ("components" "systems" "renderer"))
               (:file "sprite-system"
                :depends-on ("entities" "components" "systems" "prefabs"
                                        "event-queue" "events"
                                        "renderer" "aseprite"
                                        "hp-system" "sprite-batch-system"
                                        "sound-system"))
               (:file "systems")
               (:file "tiled"
                :depends-on ("fs"))
               (:file "ui-system"
                :depends-on ("components" "systems" "prefabs"))))

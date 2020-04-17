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
               :cl-ppcre
               :deeds
               :float-features
               :livesupport
               :make-hash
               :parse-float
               :qbase64
               :trivial-garbage
               :trivial-gray-streams
               :uiop
               :xmls)
  :pathname "src"
  :serial t
  :components ((:file "package")
               (:file "priority-queue")
               (:file "growable-vector")
               (:file "sparse-matrix")
               (:file "config")
               (:file "log")
               (:file "event-loop")
               (:file "events")
               (:file "fs")
               (:file "tiled")
               (:file "aseprite")
               (:file "systems")
               (:file "coordinate-system")
               (:file "debug-system")
               (:file "console-system")
               (:file "camera-system")
               (:file "sprite-batch-system")
               (:file "map-system")
               (:file "collision-system")
               (:file "sprite-system")
               (:file "hp-system")
               (:file "mana-system")
               (:file "character-system")
               (:file "combat-system")
               (:file "mob-system")
               (:file "player-system")
               (:file "renderer")
               (:file "d2clone-kit")))

(pushnew :deeds-no-startup *features*)

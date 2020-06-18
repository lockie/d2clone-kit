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
               (:file "camera-system")
               (:file "sprite-batch-system")
               (:file "map-system")
               (:file "collision-system")
               (:file "sprite-system")
               (:file "sound-system")
               (:file "hp-system")
               (:file "mana-system")
               (:file "character-system")
               (:file "combat-system")
               (:file "item-system")
               (:file "mob-system")
               (:file "player-system")
               (:file "ui-system")
               (:file "menu-system")
               (:file "credits-system")
               (:file "renderer")
               (:file "d2clone-kit"))
  :around-compile (lambda (next)
                    (when (find :release *features*)
                      (proclaim '(optimize
                                  (speed 3)
                                  (debug 0)
                                  (compilation-speed 0)
                                  (safety 1))))
                    (funcall next)))

(pushnew :deeds-no-startup *features*)

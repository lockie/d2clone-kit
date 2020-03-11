(asdf:defsystem :d2clone-kit
  :description "Generic Diablo 2 clone game engine."
  :author "Andrew Kravchuk <awkravchuk@gmail.com>"
  :license "GPL v3"
  :depends-on (:alexandria
               :babel
               :chipz
               :cl-csv
               :cl-inflector
               :cl-liballegro
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
               (:file "config")
               (:file "log")
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
               (:file "sprite-system")
               (:file "character-system")
               (:file "player-system")
               (:file "renderer")
               (:file "d2clone-kit"))
  ;; :around-compile (lambda (next)
  ;;                   (proclaim '(optimize (debug 0)
  ;;                               (safety 0)
  ;;                               (speed 3)))
  ;;                   (funcall next))
  :defsystem-depends-on (:deploy)
  :build-operation "deploy:deploy-op"
  :build-pathname "d2clone"
  :entry-point "d2clone-kit:demo")

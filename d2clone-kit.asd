(asdf:defsystem :d2clone-kit
  :description "Generic Diablo 2 clone game engine."
  :author "Andrew Kravchuk <awkravchuk@gmail.com>"
  :license "GPL v3"
  :depends-on (:iterate
               :make-hash
               :trivial-gray-streams
               :alexandria
               :trivial-garbage
               :cl-containers
               :parse-float
               :float-features
               :babel
               :qbase64
               :chipz
               :uiop
               :xmls
               :cl-csv
               :cl-liballegro
               :livesupport)
  :pathname "src"
  :serial t
  :components ((:file "package")
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
  :defsystem-depends-on (:asdf-shared-library  ;; XXX : replace this GPL crap with asdf-linguist
                         :deploy)
  :build-operation "deploy:deploy-op"
  :build-pathname "d2clone"
  :entry-point "d2clone-kit:main")

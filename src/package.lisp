(in-package :cl-user)

(defpackage :d2clone-kit
  (:documentation "Generic Diablo 2 clone game engine.")
  (:use :cl :alexandria :iterate :trivial-garbage :parse-float :xmls)
  (:import-from :cl-containers :priority-queue-on-container :insert-item
   :iterate-elements :empty!)
  (:import-from :make-hash :make-hash)
  (:shadow character)
  (:export :demo))

(in-package :d2clone-kit)

(declaim (inline make-renderer))
(defun make-renderer ()
  "Creates functional renderer instance."
  (make-priority-queue #'car))

(declaim
 (inline render)
 (ftype (function (priority-queue double-float function)) render))
(defun render (renderer z-order render-proc)
  "Schedules function RENDER-PROC to be called in accordance with specified
Z-ORDER within renderer instance RENDERER."
  (priority-queue-push renderer (cons z-order render-proc)))

(declaim
 (inline do-draw)
 (ftype (function (priority-queue)) do-draw))
(defun do-draw (renderer)
  "Calls renderer functions scheduled within renderer instance RENDERER in
accordance with their respective Z order values."
  (priority-queue-traverse
   renderer
   #'(lambda (item)
       (funcall (the function (cdr item)))))
  (priority-queue-clear renderer))

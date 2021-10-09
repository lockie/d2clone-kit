(in-package :d2clone-kit)


(declaim (type double-float *delta-time*))
(global-vars:define-global-var *delta-time* 0d0
  "Delta time between current frame and the previous one, in seconds.")

(declaim #-d2c-debug (inline make-renderer))
(defun make-renderer ()
  "Creates functional renderer instance.

See RENDER
See DO-DRAW"
  (make-priority-queue #'car))

(declaim
 #-d2c-debug (inline render)
 (ftype (function (priority-queue double-float function)) render))
(defun render (renderer z-order render-proc)
  "Schedules function RENDER-PROC to be called in accordance with specified
Z-ORDER within renderer instance RENDERER.

See MAKE-RENDERER
See DO-DRAW"
  (priority-queue-push renderer (cons z-order render-proc)))

(declaim
 #-d2c-debug (inline do-draw)
 (ftype (function (priority-queue)) do-draw))
(defun do-draw (renderer)
  "Calls renderer functions scheduled within renderer instance RENDERER in
accordance with their respective Z order values.

See MAKE-RENDERER
See RENDER"
  (priority-queue-traverse
   renderer
   #'(lambda (item)
       (funcall (the function (cdr item)))))
  (priority-queue-clear renderer))

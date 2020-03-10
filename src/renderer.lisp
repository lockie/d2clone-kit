(in-package :d2clone-kit)

(defun make-renderer ()
  "Creates functional renderer instance."
  (make-priority-queue #'car))

(defun render (renderer z-order render-proc)
  "Schedules function RENDER-PROC to be called in accordance with specified Z order Z-ORDER within renderer instance RENDERER."
  (priority-queue-push renderer (cons z-order render-proc)))

(defun do-draw (renderer)
  "Calls renderer functions scheduled within renderer instance RENDERER in accordance with their respective Z order values."
  (priority-queue-traverse
   renderer
   #'(lambda (item)
       (funcall (cdr item))))
  (priority-queue-clear renderer))


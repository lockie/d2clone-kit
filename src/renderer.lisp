(in-package :d2clone-kit)

(defun make-renderer ()
  "Creates functional renderer instance."
  (make-instance 'priority-queue-on-container
                 :sorter #'(lambda (a b) (< (car a) (car b)))
                 :test #'(lambda (a b) (= (car a) (car b)))))

(defun render (renderer z-order render-proc)
  "Schedules function RENDER-PROC to be called in accordance with specified Z order Z-ORDER within renderer instance RENDERER."
  (insert-item renderer (cons z-order render-proc)))

(defun do-draw (renderer)
  "Calls renderer functions scheduled within renderer instance RENDERER in accordance with their respective Z order values."
  (iterate-elements
   renderer
   #'(lambda (item)
       (funcall (cdr item))))
  (empty! renderer))


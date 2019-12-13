(in-package :d2clone-kit)

(defun make-renderer ()
  (make-instance 'priority-queue-on-container
                 :sorter #'(lambda (a b) (< (car a) (car b)))
                 :test #'(lambda (a b) (= (car a) (car b)))))

(defun render (renderer z-order render-proc)
  (insert-item renderer (cons z-order render-proc)))

(defun do-draw (renderer)
  (iterate-elements
   renderer
   #'(lambda (item)
       (funcall (cdr item))))
  (empty! renderer))


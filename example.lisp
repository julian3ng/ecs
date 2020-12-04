(defpackage :example
  (:use :cl :allocator :ecs))

(in-package :example)

(progn 
 (ecs:clear ecs:*ecs*)
 (ecs:defcomponent pos
     ((x :initform (error "Supply :x") :initarg :x :accessor x)
      (y :initform (error "Supply :y") :initarg :y :accessor y)))
 (ecs:defcomponent appearance
     ((glyph :initform nil :initarg :glyph :accessor glyph)
      (color :initform nil :initarg :color :accessor color)))
 (let ((A (make-instance 'allocator:allocator)))
   (let ((e1 (allocate A))
         (e2 (allocate A)))
     (setf (ecs:component e1 'pos) (make-instance 'pos :x 1 :y 2))
     (list
      (let ((p (ecs:component e1 'pos)))
        (list (x p) (y p)))
      (ecs:component e2 'pos)))))




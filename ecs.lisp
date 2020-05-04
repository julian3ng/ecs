(defpackage :ecs
  (:use :cl
        :allocator
        :gix-array
        :set)
  (:shadow :delete
           :defsystem)
  (:shadowing-import-from :cl
                          :elt
                          :set
                          :equal)
  (:export :defcomponent
           :clear
           :*ecs*))
(in-package :ecs)

(defclass ecs ()
  ((component-tables :accessor component-tables
                     :initform (make-hash-table :test #'equal))))

(defmethod print-object ((obj ecs) out)
  (with-slots (component-tables) obj
    (format out "~{~A~%~}" (loop for k being the hash-keys in component-tables
                              using (hash-value v) collect (list k v)))))

(defmethod clear ((e ecs))
  (setf (component-tables e) (make-hash-table :test #'equal)))

(defparameter *ecs* (make-instance 'ecs))

(defclass component () ())

(defmacro defcomponent (name slots-list)
  `(progn (defclass ,name (component)
            ,slots-list)
          (setf (gethash ',name (component-tables *ecs*))
                (make-instance 'gix-array:gix-array))

          (defmethod component ((ix allocator:gix) (c (eql ',name)))
            (with-slots (component-tables) *ecs*
              (let ((ctable (gethash ',name component-tables)))
                (gix-array:elt ctable ix))))

          (defmethod (setf component) (value (ix allocator:gix) (c (eql ',name)))
            (with-slots (component-tables) *ecs*
                (let ((ctable (gethash ',name component-tables)))
                  (setf (gix-array:elt ctable ix) value))))

          (defmethod delete ((ix allocator:gix) (c (eql ',name)))
            (with-slots (component-tables) *ecs*
              (let ((ctable (gethash ',name component-tables)))
                (gix-array:delete ctable ix))))))


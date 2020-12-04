(defpackage :ecs
  (:use :cl :allocator :gix-array :hashset)
  (:shadow :delete :defsystem)
  (:shadowing-import-from :cl :elt :set :equal)
  (:export :defsystem
   :defcomponent :component :delete :clear :*ecs*))
(in-package :ecs)

(declaim (optimize (debug 3) (speed 1)))
(defclass ecs ()
  ((component-tables :accessor component-tables
                     :initform (make-hash-table :test #'equal))
   (system-component-sets :accessor system-component-sets
                          :initform (make-hash-table :test #'equal))
   (component-system-sets :accessor component-system-sets
                          :initform (make-hash-table :test #'equal))
   (system-gix-sets :accessor system-gix-sets
                    :initform (make-hash-table :test #'equal))))

(defmethod print-object ((obj ecs) out)
  (with-slots (component-tables
               system-component-sets
               component-system-sets
               system-gix-sets)
      obj
    (format out "~&Component-tables:~%~{~&	~A~}"
            (loop for k being the hash-keys in component-tables
                    using (hash-value v) collect (list k v)))
    (format out "~&System-component-sets:~%~{~&	~A~}"
            (loop for k being the hash-keys in system-component-sets
                    using (hash-value v) collect (list k v)))
    (format out "~&component-System-sets:~%~{~&	~A~}"
            (loop for k being the hash-keys in component-system-sets
                    using (hash-value v) collect (list k v)))
    (format out "~&System-gix-sets:~%~{~&	~A~}"
            (loop for k being the hash-keys in system-gix-sets
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

(defmacro defsystem (name (var component-type-list) &body body)
  (let ((undefined-components
          ;; All components that don't have a component-table in *ecs* (meaning they're not defined)
          (remove-if #'null
                     (mapcar (lambda (ctype)
                               (when (not (nth-value 1 (gethash ctype (component-tables *ecs*))))
                                 ctype))
                             component-type-list))))
    (if (null undefined-components)
        ;; If no undefined components, ok to define system
        (let ((ctype-list (gensym "ctype-list"))) 
          `(progn
             ;; Evaluate the component type list only once in the generated code
             (let ((,ctype-list ',component-type-list))
               ;; Set up tables
               (setf (gethash ',name (system-component-sets *ecs*))
                     (make-instance 'hashset :values ,ctype-list))

               
               (mapcar (lambda (ctype)
                         (let ((maybe-system-set (gethash ctype (component-system-sets *ecs*))))
                           (if maybe-system-set
                               (insert maybe-system-set ',name)
                               (setf (gethash ctype (component-system-sets *ecs*))
                                     (make-instance 'hashset :values (list ',name))))))
                       ,ctype-list)
               (setf (gethash ',name (system-gix-sets *ecs*)) (make-instance 'hashset))
               ;; Define the system
               (defun ,name ()
                 (do-hashset (,var (gethash ',name (system-gix-sets *ecs*)))
                   ,@body))

               (defmethod part-of-system ((ix allocator:gix) (s (eql ',name)))
                 (with-slots (component-tables) *ecs*
                   (let ((components-present
                           (mapcar (lambda (c) (gix-array:elt (gethash c component-tables) ix)) ,ctype-list)))
                     (= 0 (count nil components-present)))))


               (defmethod (setf component) :after (value (ix allocator:gix) c)
                 (do-hashset (sysname (gethash c (component-system-sets *ecs*)))
                   (when (part-of-system ix sysname)
                     (insert (gethash sysname (system-gix-sets *ecs*)) ix))))

               (defmethod delete :after ((ix allocator:gix) c)
                 (do-hashset (sysname (gethash c (component-system-sets *ecs*)))
                   (when (not (part-of-system ix sysname))
                     (hashset:delete (gethash sysname (system-gix-sets *ecs*)) ix))))
               ;; THIS IS WRONG: need to have one :after for setf and
               ;; delete that goes through all systems affected and
               ;; adds/deletes the index0
               
               
               ;; ,@(mapcar (lambda (ctype)
               ;;             `(progn  (defmethod (setf component) :after
               ;;                          (value (ix allocator:gix) (c (eql ',ctype)))
               ;;                        (when (part-of-system ix ',name)
               ;;                          (insert (gethash ',name (system-gix-sets *ecs*)) ix)))
               ;;                      (defmethod delete :after ((ix allocator:gix) (c (eql ',ctype)))
               ;;                        (when (not (part-of-system ix ',name))
               ;;                          (hashset:delete (gethash ',name (system-gix-sets *ecs*)) ix)))))
               ;;           component-type-list)
               )))
        ;; Else error out
        (progn
          (format t "Undefined components: ~{~A~^, ~}~%" undefined-components)
          (error 'components-undefined-error :undefined-components undefined-components)))))


(define-condition components-undefined-error (error)
  ((undefined-components :initarg :undefined-components :reader undefined-components)))


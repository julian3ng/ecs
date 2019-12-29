(defpackage :ecs
  (:use :cl :gmap)
  (:shadowing-import-from :cl
                          :delete
                          :elt
                          :length)
  (:export
   :add-entity
   :get-entity
   :delete-entity
   :add-component
   :delete-component
   :get-component
   :defcomponent))

(in-package :ecs)

;;; ECS describes a table of entities
;;;
;;; For each component, have a gmap
;;; [(C00) (C01) (C02) (C03)]
;;; [(C10)       (C12)      ]
;;; [      (C21) (C22) (C23)]
;;;
;;; and entities are represented as hashmaps of gixes
;;; [{C0: (0 g), C1: (0 g)},
;;;  {C0: (1 g), C2: (1 g)},
;;;  etc.
;;; ]

(declaim (optimize (speed 0) (space 0) (debug 3)  (safety 3)))
(defclass ecs ()
  ((entities :accessor entities
             :initform (make-instance 'gmap:gmap))
   (component-tables :accessor component-tables
                      :initform (make-hash-table))))

(defparameter *ecs* (make-instance 'ecs))

(defclass component () ())

(defclass entity ()
  ((index :accessor index
          :initarg :index
          :type gmap:gix)
   (components :accessor components
               :initarg :components
               :initform (make-hash-table))))

(defgeneric add-component (entity component)
  (:documentation "Add component to entity"))

(defgeneric delete-component (entity component-name)
  (:documentation "Remove component from entity by name"))

(defgeneric get-component (entity component-name)
  (:documentation "Get component from entity by name"))

(defmacro defcomponent (name slots-list)
  `(progn (defclass ,name (component)
            ,slots-list)
          (setf (gethash ',name (component-tables *ecs*))
                (make-instance 'gmap:gmap))

          (defmethod add-component ((e entity) (c ,name))
            (with-slots (component-tables) *ecs*
              (let ((this-table (gethash ',name component-tables)))
                (with-slots (components) e
                  (multiple-value-bind (entity-component-index present) (gethash ',name components)
                    (if present
                        (values entity-component-index nil)
                        (let ((component-index (gmap:insert this-table c)))
                          (setf (gethash ',name components) component-index)
                          (values component-index t))))))))

          (defmethod delete-component ((e entity) (cname (eql ',name)))
            (with-slots (component-tables) *ecs*
              (let ((this-table (gethash cname component-tables)))
                (with-slots (components) e
                  (multiple-value-bind (entity-component-index present) (gethash cname components)
                    (if present
                        (values (gmap:delete this-table entity-component-index) t)
                        (values nil nil)))))))

          (defmethod get-component ((e entity) (cname (eql ',name)))
            (with-slots (component-tables) *ecs*
              (let ((this-table (gethash cname component-tables)))
                (with-slots (components) e
                  (multiple-value-bind (entity-component-index present) (gethash cname components)
                    (if present
                        (values (gmap:elt this-table entity-component-index) t)
                        (values nil nil)))))))

          (defmethod (setf get-component) ((value ,name) (e entity))
              (with-slots (component-tables) *ecs*
                (let ((this-table (gethash ',name component-tables)))
                  (with-slots (components) e
                    (multiple-value-bind (entity-component-index present) (gethash ',name components)
                      (if present
                          (setf (gmap:elt this-table entity-component-index) value)
                          (add-component e value)))))))))

;; given: (with-components ((f 'foo) (g 'bar) (h 'baz)) entity
;;           body)
;; expand: (let ((f (get-component 'foo entity))
;;               (g (get-component 'bar entity))
;;               (h (get-component 'baz entity)))
;;            body)
(defmacro with-components (component-list entity &body body)
  (let* ((ent (gensym))
         (modified-component-list (mapcar (lambda (binding)
                                            `(,(first binding) (get-component ,ent ,(second binding))))
                                          component-list)))
    `(let* ((,ent ,entity)
            ,@modified-component-list)
       ,@body)))

(defparameter *blueprints* (make-hash-table :test #'equal))

(defmacro defblueprint (name component-list)
  `(progn
     (setf (gethash ',name *blueprints*) ,component-list)))

(defun add-entity (&key (blueprint nil))
  (let ((ent (make-instance 'entity)))
    (when blueprint
      (multiple-value-bind (components present) (gethash blueprint *blueprints*)
        (when (not present)
          (error "Blueprints corrupted"))
        (dolist (c components)
          (add-component ent (make-instance c)))))
    (setf (index ent) (gmap:insert (entities *ecs*) ent))
    ent))

(defmethod get-entity ((ix gmap:gix))
  (gmap:elt (entities *ecs*) ix))

(defmethod delete-entity ((ix gmap:gix))
  (gmap:delete (entities *ecs*) ix))


(defcomponent pos1d ((x :initarg :x :initform 0 :accessor x)))
(defcomponent health ((value :initarg :value :initform (error "provide :value") :accessor value)))
(defblueprint test '(pos1d))
(let ((e (add-entity :blueprint 'test)))
  (setf (x  (get-component e 'pos1d)) 10)
  (x (get-component e 'pos1d)))

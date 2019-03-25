(in-package :ecs)

(defun make-hash-table-from-lists (keys values)
  "Make a hashtable from list of keys and values.
Length of keys and length of values should be the same."
  (let ((ht (make-hash-table)))
    (mapcar (lambda (k v) (setf (gethash k ht) v)) keys values)
    ht))

(defun hash-keys (ht)
  "Get the hash keys of a hashtable."
  (loop for k being the hash-keys of ht collect k))

(defun hash-values (ht)
  "Get the hash values of a hashtable."
  (loop for v being the hash-values of ht collect v))

;; components: just data
(defclass component () ()) ; not sure if I need this even

(defun make-component-system-map ()
  "Make the hashmap of component-name: system-name"
  (let ((h (make-hash-table)))
    (setf (gethash nil h) nil)
    h))

(defparameter *component-system-map* (make-component-system-map))

(defmacro define-component (name spec)
  "Define a class for this component using the given spec.
Add that classname to the component-system mapping.
Define a getter on entities for that component name."
  `(progn (defclass ,name (component) ,spec)
          (setf (gethash ',name *component-system-map*) nil)
          (defmethod ,name ((entity entity))
            (with-slots (components) entity
              (gethash ',name components)))))

;;; systems: bag of entity indices, set of component types,
;;; run function that takes one entity and does something with it

;; name: system
(defparameter *systems* (make-hash-table) "Hashtable from system name to system object.")
(defclass system ()
  ((entities :initarg :entities
             :initform nil
             :accessor entities
             :documentation "List of entities this sytem operates on.")
   (ctypes :initarg :ctypes
           :initform nil
           :accessor ctypes
           :documentation "List of component types entities must have in this system.")
   (run-function :initarg :run-function
                 :initform (error "Supply :run-function")
                 :accessor run-function
                 :documentation "What happens when this system is invoked.")))

(defmacro define-system (name (entity-var ctype-list) &body body)
  "Define a system.
Systems have a name, a variable that binds to the entity in its body,
a list of components that it requires in its entities, and what it
does."
  (let ((sys (gensym))
        (ctype (gensym)))
    ;; make the system
    `(progn (let ((,sys (make-instance 'system
                                       :entities nil
                                       :ctypes ,ctype-list
                                       :run-function (lambda (,entity-var) ,@body))))
              ;; if there' already a system, transfer the entities
              ;; (if (nth-value 1 (gethash ',name *systems*))
              ;;     (setf (entities ,sys) (entities (gethash ',name *systems*))))

              ;; we could be defining systems mid-usage: run through
              ;; entities and register the ones that satisfy our
              ;; requirements
              (let ((compatible-entities '()))
                (gmap-do (e *entities*)
                  (when (compatible ,sys e)
                    (setf compatible-entities (cons e compatible-entities))))
                (setf (entities ,sys) compatible-entities))

              ;; register our system
              (setf (gethash ',name *systems*) ,sys)

              (if (null ,ctype-list)
                  (pushnew ',name (gethash nil *component-system-map*))
                  ;; for each component type, key the system with said
                  ;; component type
                  (dolist (,ctype ,ctype-list)
                    (if (member ,ctype
                                (hash-keys *component-system-map*))
                        (pushnew ',name (gethash ,ctype *component-system-map*))
                        (error "~A isn't a component type" ,ctype))))))))


;; entities: alist of (ctype . component)
(defparameter *entities* (make-instance 'gmap))

(defclass entity ()
  ((index :initarg :index
          :initform (error "Supply :index")
          :accessor index
          :documentation "Index into the global entity map.")
   (components :initarg :components
               :initform (make-hash-table)
               :accessor components
               :documentation "Table of components this entity has.")))

(defmethod ctypes ((entity entity))
  "Get the component types of an entity as a list"
  (with-slots (components) entity
    (hash-keys components)))

(defmethod print-object ((object entity) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (components) object
      (format stream "[~{~A~^, ~}]" (hash-values components)))))

(defmethod compatible ((sys system) (ent entity))
  "Show if a system can act on an entity"
  (with-slots ((s-ctypes ctypes)) sys
    (with-slots (components) ent
      (subsetp s-ctypes (ctypes ent)))))


(defmethod add-entity-to-system ((sys system) entity)
  "Add an entity to the given system."
  (if (compatible sys entity)
      (push entity (entities sys))))

(defmethod remove-entity-from-system ((sys system) entity)
  "Remove an entity from the given system."
  (with-slots (entities) sys
    (setf entities (delete entity entities))))


(defun make-entity (&rest components)
  "Create an entity, optionally with components."
  (let* ((ctypes (mapcar #'type-of components))
         (ctable (make-hash-table-from-lists ctypes components))
         ;; need the index to construct our entity
         (eix (insert-val *entities* nil))
         (entity (make-instance 'entity
                                :index eix
                                :components ctable))
         )
    ;; insert entity into table for real
    (setf (get-val *entities* eix) entity)
    ;; for each component type
    (dolist (ctype ctypes)
      ;; get the names of systems we may add them to
      (let ((sysnames (gethash ctype *component-system-map*)))
        ;; for each system name
        (dolist (sysname sysnames)
          ;; add the entity
          (add-entity-to-system (gethash sysname *systems*) entity))))
    entity))

(defmethod add-component-to-entity ((entity entity) (component component))
  "Add a component to an entity"
  (with-slots (components) entity
    ;; whether or not it was in there
    (let ((new (null (nth-value 1 (gethash (type-of component) components)))))
      ;; set new component
      (setf (gethash (type-of component) components) component)
      ;; if it's a new component type, check every system
      ;; that uses this component type to see if we can add ourselves in
      (if new
          (dolist (ctype (ctypes entity))
            (let ((sysnames (gethash ctype *component-system-map*)))
              (dolist (sysname sysnames)
                (add-entity-to-system (gethash sysname *systems*) entity)))))))
  t)

(defmethod get-component-from-entity ((entity entity) ctype)
  "Get a component from an entity"
  (if (subtypep ctype 'component)
      (with-slots (components) entity
        (gethash ctype components))
      (error "~A is not a subtype of component ~%" ctype)))

(defmethod remove-component-from-entity ((entity entity) ctype)
  "Remove a component from an entity"
  (if (or (subtypep ctype 'component) (null ctype))
      (progn
        ;; remove eix from all systems that needed that component
        (let ((sysnames (gethash ctype *component-system-map*)))
          (dolist (sysname sysnames)
            (remove-entity-from-system (gethash sysname *systems*) entity)))
        ;; remove component from entity
        (with-slots (components) entity
          (remhash ctype components)))
      (error "~A is not a subtype of component~%" ctype)))

(defun remove-entity-by-index (eix)
  "Given the entity's index, remove all of its components, then delete
it from the global entities map."
  (let ((entity (get-val *entities* eix)))
    (with-slots (components) entity
      (dolist (ctype (cons nil (ctypes entity)))
        (remove-component-from-entity entity ctype))))
  (remove-val *entities* eix))

(defmethod remove-entity ((entity entity))
  "Remove an entity"
  (with-slots (index) entity
    (remove-entity-by-index index)))

(defun run-system (sysname)
  "Run a system's run function"
  (let ((sys (gethash sysname *systems*)))
    (with-slots (entities ctypes run-function) sys
      (if (null ctypes)
          (gmap-do (e *entities*)
            (funcall run-function e))
          (loop
             for e in entities
             do (funcall run-function e))))))

;; utility functions
(defun print-ecs ()
  (format t "****************************************************************~%")
  (format t "==========Entities==========~%~S~%"
          (gmap::entries *entities*))
  (format t "~%=========Components=========~%~S~%"
          (hash-keys *component-system-map*))
  (format t "~%==========Systems===========~%~S~%"
          (loop for k being the hash-keys of
               *systems* using (hash-value v)
             collect (list k
                           :entities (entities v)
                           :ctypes (ctypes v) )))
  (format t "~%====Component-System-Map====~%~S~%"
          (loop for k being the hash-keys of
               *component-system-map* using (hash-value v)
             collect (list k :systems v)))
  (format t "~%****************************************************************~%"))


(defun reset-ecs ()
  (setf *component-system-map* (make-component-system-map))
  (setf *systems* (make-hash-table))
  (setf *entities* (make-instance 'gmap)))

(defun reset-entities ()
  (dolist (sysname  (hash-keys *systems*))
    (setf (entities  (gethash sysname *systems*)) nil))
  (reset *entities*))

(defpackage :storage
  (:use :cl))

(defpackage :gmap
  (:use :cl :storage)
  (:export :gmap
           :insert-val
           :remove-val
           :get-val
           :storage-length
           :clear
           :reset
           :gmap-do
           :make-gindex))

(defpackage :ecs
  (:use :cl :gmap)
  (:export :defcomponent
           :defsystem
           :entity
           :make-entity
           :remove-entity
           :add-component-to-entity
           :remove-component-from-entity
           :run-system
           :reset-ecs))



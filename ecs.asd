(defpackage :ecs-asd
  (:use :cl :asdf))

(in-package :ecs-asd)

(defsystem ecs
    :name "ecs"
    :version "0.0.0"
    :maintainer "Julian Eng"
    :author "Julian Eng"
    :description "Entity Component System"
    :components ((:file "packages")
                 (:file "storage")
                 (:file "gmap")
                 (:file "ecs")))

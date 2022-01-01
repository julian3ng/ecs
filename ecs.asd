(asdf:defsystem :ecs
  :version      "0.1.0"
  :description  "ECS"
  :author       "Julian Eng <julian3ng@gmail.com>"
  :serial       t
  :components   ((:file "hashset")
                 (:file "allocator")
                 (:file "gix-array")
                 (:file "ecs")))

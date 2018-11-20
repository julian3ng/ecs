(defpackage :cl-jeng
  (:use :cl)
  (:export :with-multiple-slots))

(in-package :cl-jeng)

(defmacro with-multiple-slots (slot-specifications &body body)
  (if (null slot-specifications)
      `(progn ,@body)
      `(with-multiple-slots
           ,(cdr slot-specifications)
         (with-slots ,(caar slot-specifications) ,(cadar slot-specifications)
           ,@body))))

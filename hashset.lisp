(defpackage   :hashset
  (:use       :cl)
  (:shadow    :set
              :equal
              :elt
              :delete)
  (:export    :hashset
              :elt
              :insert
              :equal
              :delete
              :do-hashset
              :collect-hashset))

(in-package :hashset)

(defclass hashset ()
  ((data :initform (make-hash-table :test #'cl:equal))))

(defmethod print-object ((object hashset) out)
  (with-slots (data) object
    (format out "<HASHSET: ~{~A~^, ~}>" (loop for k being the hash-keys of data collect k))))

(defmethod initialize-instance :after ((hs hashset) &key values)
  (when values
    (with-slots (data) hs
      (mapcar (lambda (v) (setf (gethash v data) t)) values))))

(defmethod elt ((hs hashset) element)
  "Check if element is in hs"
  (with-slots (data) hs
    (gethash element data)))

(defmethod insert ((hs hashset) element)
  "Insert element into hs"
  (with-slots (data) hs
    (setf (gethash element data) t)))

(defmethod equal ((hs1 hashset) (hs2 hashset))
  "Check if two hashsets are equal"
  (with-slots ((data1 data)) hs1
    (with-slots ((data2 data)) hs2
      (null (set-difference 
             (loop for k1 being the hash-keys in data1 collect k1)
             (loop for k2 being the hash-keys in data2 collect k2))))))

(defmethod delete ((hs hashset) element)
  "Remove element from hs"
  (with-slots (data) hs
    (remhash element data)))

(defmacro do-hashset ((var hs &optional result) &body body)
  "Bind run body on each value of hs bound to var"
  `(with-slots (data) ,hs
     (loop for ,var being the hash-keys in data do ,@body)
     ,result))

(defmethod collect-hashset ((hs hashset))
  "Collect items of hs into list"
  (with-slots (data) hs
    (loop for k being the hash-keys in data collect k)))

(defmethod intersect ((hs1 hashset) (hs2 hashset))
  "Intersect two hashsets"
  (let ((out (make-instance 'hashset)))
    (do-hashset (k1 hs1)
      (when (elt hs2 k1)
        (insert out k1)))
    out))

(defun test0 ()
  (let ((hs (make-instance 'hashset)))
    (dotimes (x 3)
      (insert hs x))
    (dotimes (x 3)
      (assert (elt hs x)))
    (delete hs 2)
    (assert (not (elt hs 2)))))

(defun test1 ()
  (let ((hs (make-instance 'hashset)))
    (dotimes (x 3)
      (insert hs x))

    (let ((res (make-instance 'hashset)))
      (do-hashset (x hs)
        (insert res x))
      (assert (equal hs res)))))


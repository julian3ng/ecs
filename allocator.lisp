(defpackage   :allocator
  (:use       :cl)
  (:export    :allocator
              :gix
              :allocate
              :deallocate
              :is-live
              :index
              :generation))

(in-package :allocator)

(defclass entry ()
  ((is-live :type 'boolean
            :initarg :is-live)
   (generation :type 'integer
               :accessor generation
               :initarg :generation)))

(defclass gix ()
  ((index
    :reader index
    :initarg :index
    :type 'integer)
   (generation
    :initarg :generation
    :type 'integer)))

(defclass allocator ()
  ((entries :accessor entries
            :initform (make-array 0
                                  :adjustable t
                                  :element-type 'entry
                                  :fill-pointer 0))
   (free :accessor free
         :initform nil)))


;;(defparameter *next-index* 0)

(defmethod allocate ((a allocator))
  (with-slots (entries free) a
    (let ((next-index (if (null free)
                          (vector-push-extend (make-instance 'entry
                                                             :is-live nil
                                                             :generation -1)
                                              entries)
                          (pop free))))

      (let ((new-entry (elt entries next-index)))
        (setf (slot-value new-entry 'is-live) t)
        (incf (generation new-entry))
        (make-instance 'gix :index next-index :generation (generation new-entry))))))

(defmethod deallocate ((a allocator) (gix gix))
  (with-slots (index generation) gix
    (with-slots (entries free) a
      (let ((cur-entry (elt entries index)))
        (when (and  (slot-value cur-entry 'is-live) (= (generation cur-entry) generation))
          (setf (slot-value cur-entry 'is-live) nil)
          (push index free)
          t)))))

(defmethod is-live ((a allocator) (gix gix))
  (with-slots (index) gix
    (with-slots (entries) a
      (slot-value (elt entries index) 'is-live))))

(defun test-0 ()
  (let ((a (make-instance 'allocator)))
    (assert (=  (length (entries a)) 0))
    (assert (null (free a)))))

(defun test-1 ()
  (let ((a (make-instance 'allocator)))
    (let ((ix (allocate a)))
      (assert (zerop  (index ix)))
      (assert (zerop (slot-value ix 'generation))))))

(defun test-2 ()
  (let ((a (make-instance 'allocator)))
    (let ((ix (allocate a))
          (ix2 (allocate a)))
      (assert (is-live a ix))
      (assert (is-live a ix2))
      (deallocate a ix)
      (assert (null (is-live a ix)))
      (assert (equal (free a) '(0))))))

(defun test-3 ()
  (let ((a (make-instance 'allocator)))
    (loop repeat 3 do (allocate a))
    (assert (= (index (allocate a)) 3))
    (assert (= (length (entries a)) 4))))

(defun run-tests (&rest tests)
  (mapcar #'null (mapcar #'funcall tests)))

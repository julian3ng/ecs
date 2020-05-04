(defpackage   :gix-array
  (:use       :cl
              :allocator)
  (:shadow    :elt
              :delete)
  (:export    :gix-array
              :elt
              :delete))

(in-package :gix-array)

(defclass entry ()
    ((value :initarg :value
            :accessor value)
     (generation :type 'integer
                 :initarg :generation
                 :accessor generation)))

(defun null-or-entry-p (e?)
  (or (null e?) (equal (type-of e?) 'entry)))

(defclass gix-array ()
    ((backing-array :initarg :backing-array
                    :initform (make-array 0
                                          :initial-element nil
                                          :element-type '(satisfies null-or-entry-p)
                                          :adjustable t)
                    :accessor backing-array)))


(defmethod elt ((gixarr gix-array) (gix allocator:gix))
  (with-slots (backing-array) gixarr
    (with-slots (allocator:index allocator:generation) gix
      (let ((maybe-entry (cl:elt backing-array index)))
        (when (and maybe-entry (= (generation maybe-entry) generation))
          (value maybe-entry)))))
)

(defmethod (setf elt) (val (gixarr gix-array) (gix allocator:gix))
  (with-slots (backing-array) gixarr
    (with-slots (allocator:index generation) gix
      (when (>= (length backing-array) index)
        (setf backing-array (adjust-array backing-array (* 2 (1+ index))
                                          :element-type '(satisfies null-or-entry-p)
                                          :initial-element nil)))
      (when (null (cl:elt backing-array index))
          (setf (cl:elt backing-array index) (make-instance 'entry :generation 0)))
        (when (<= (generation (cl:elt backing-array index)) generation)
              (setf (value (cl:elt backing-array index)) val)))))

(defmethod delete ((gixarr gix-array) (gix allocator:gix))
  (with-slots (backing-array) gixarr
    (with-slots (index generation) gix
      (when (not (null (cl:elt backing-array index)))
        (setf (cl:elt backing-array index) nil)))))
  

(defun test-0 ()
  (let ((garr (make-instance 'gix-array))
        (alloc (make-instance 'allocator)))
    (let ((ix (allocate alloc)))
      (handler-case (elt garr ix)
        (t nil)))))

(defun test-1 ()
  (let ((garr (make-instance 'gix-array))
        (alloc (make-instance 'allocator)))
    (let ((ix (allocate alloc)))
      (setf (elt garr ix) 10)
      (assert (=  (elt garr ix) 10)))))

(defun test-2 ()
  (let ((garr (make-instance 'gix-array))
        (alloc (make-instance 'allocator)))
    (let ((ix (allocate alloc)))
      (setf (elt garr ix) 10)
      (delete garr ix)
      (assert (null (elt garr ix))))))

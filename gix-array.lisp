(defpackage   :gix-array
  (:use       :cl
              :allocator
              :hashset)
  (:shadow
   :elt
   :delete
   :equal)
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

(defmethod print-object ((e entry) out)
  (with-slots (value generation) e
    (format out "<Entry: ~A gen:~A>" value generation)))

(defun null-or-entry-p (e?)
  (or (null e?) (equal (type-of e?) 'entry)))

(defclass gix-array ()
    ((backing-array :initarg :backing-array
                    :initform (make-array 1
                                          :initial-element nil
                                          :element-type '(satisfies null-or-entry-p)
                                          :adjustable t)
                    :accessor backing-array)
     (used-ixs :initarg :used-ixs
               :accessor used-ixs
               :initform (make-instance 'hashset:hashset))))

(defmethod print-object ((gixarr gix-array) out)
  (with-slots (backing-array used-ixs) gixarr
    (format out "<gix-array: [~{~A~^ ~}], <~{~A~^ ~}>>"
            (loop for x across backing-array collect x)
            (collect-hashset used-ixs))))

(defmethod elt ((gixarr gix-array) (gix allocator:gix))
  (with-slots (backing-array) gixarr
    (with-slots (allocator:index allocator:generation) gix
      (when (< index (length backing-array)) 
        (let ((maybe-entry (cl:elt backing-array index)))
          (when (and maybe-entry (= (generation maybe-entry) generation))
            (value maybe-entry)))))))

(defmethod (setf elt) (val (gixarr gix-array) (gix allocator:gix))
  (with-slots (backing-array used-ixs) gixarr
    (with-slots (allocator:index generation) gix
      (when (>= index (length backing-array))
        (setf backing-array (adjust-array backing-array (* 2 (1+ index))
                                          :element-type '(satisfies null-or-entry-p)
                                          :initial-element nil)))
      (when (null (cl:elt backing-array index))
          (setf (cl:elt backing-array index) (make-instance 'entry :generation 0)))
        (when (<= (generation (cl:elt backing-array index)) generation)
          (setf (value (cl:elt backing-array index)) val)
          (insert used-ixs gix)))))

(defmethod delete ((gixarr gix-array) (gix allocator:gix))
  (with-slots (backing-array used-ixs) gixarr
    (with-slots (index generation) gix
      (when (not (null (cl:elt backing-array index)))
        (setf (cl:elt backing-array index) nil)
        (hashset:delete used-ixs gix)))))


(defmacro do-gix-array ((var gixarr &optional result) &body body)
  (let ((ix (gensym "index"))) 
    `(with-slots (used-ixs) ,gixarr
       (do-hashset (,ix used-ixs ,result)
         (let ((,var (elt ,gixarr ,ix)))
           ,@body)))))

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

(defun test-3 ()
  (let ((garr (make-instance 'gix-array))
        (alloc (make-instance 'allocator)))
    (let ((ix (allocate alloc))
          (ix2 (allocate alloc)))
      (setf (elt garr ix) 1)
      (setf (elt garr ix2) 2)
      (assert (hashset:elt (used-ixs garr) ix))
      (assert (hashset:elt (used-ixs garr) ix2))

      (setf (elt garr (allocate alloc)) 3)

      (delete garr ix2))

    (let ((sum 0)) 
      (do-gix-array (n garr)
        (setf sum (+ sum n)))
      (assert (= sum 4)))))

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
   (generation :type integer
               :initarg :generation
               :accessor generation))
  (:documentation "An entry in the gix-array.
If the gix used to key into this entry has a different generation then the access will fail."))

(defmethod print-object ((e entry) out)
  (with-slots (value generation) e
    (format out "<Entry: ~A gen:~A>" value generation)))

(defun null-or-entry-p (e?)
  (or (null e?) (cl:equal (type-of e?) 'entry)))

(defclass gix-array ()
    ((backing-array :initarg :backing-array
                    :initform (make-array 1
                                          :initial-element nil
                                          :element-type '(satisfies null-or-entry-p)
                                          :adjustable t)
                    :accessor backing-array
                    :documentation "Array of entries (value, generation)")
     (used-ixs :initarg :used-ixs
               :accessor used-ixs
               :initform (make-instance 'hashset:hashset)
               :documentation "Hashset of used indices, used for iteration over gix-array."))
  (:documentation "Array indexed by generational indexes created by an allocator:allocator."))

(defmethod print-object ((gixarr gix-array) out)
  (with-slots (backing-array used-ixs) gixarr
    (format out "<gix-array: [~{~A~^ ~}], <~{~A~^ ~}>>"
            (loop for x across backing-array collect x)
            (collect-hashset used-ixs))))

(defmethod elt ((gixarr gix-array) (gix allocator:gix))
  "Obtain the element indexed by gix if the generation of gix matches
the element's generation."
  (with-slots (backing-array) gixarr
    (with-slots (allocator:index allocator:generation) gix
      (if (< index (length backing-array)) 
          (let ((maybe-entry (cl:elt backing-array index)))
            (when (and maybe-entry (= (generation maybe-entry) generation))
              (value maybe-entry)))
          (error "gix-array:elt index out of bounds")))))

(defmethod (setf elt) (val (gixarr gix-array) (gix allocator:gix))
  "Insert the given value into the gix-array at the index of gix.
When index is out of bounds, increase the length to accomodate the index, then double it.
When an entry doesn't exist, create it and set its value to the given value.
Add the gix to the used-ix hashset.
"
  (with-slots (backing-array used-ixs) gixarr
    (with-slots (allocator:index generation) gix
      (when (>= index (length backing-array))
        ;; Double backing array
        (setf backing-array (adjust-array backing-array (* 2 (1+ index))
                                          :element-type '(satisfies null-or-entry-p)
                                          :initial-element nil)))
      ;; When the entry doesn't exist, make it 
      (when (null (cl:elt backing-array index))
        (setf (cl:elt backing-array index) (make-instance 'entry :generation generation)))

      ;; When the entry's generation is at most the gix's generation,
      ;; overwrite the contents
      (when (<= (generation (cl:elt backing-array index)) generation)
        (setf (value (cl:elt backing-array index)) val)
        (setf (generation (cl:elt backing-array index)) generation)
        (insert used-ixs index)))))

(defmethod delete ((gixarr gix-array) (gix allocator:gix))
  "Remove the element at the index of gix from the gix-array."
  (with-slots (backing-array used-ixs) gixarr
    (with-slots (index generation) gix
      (let ((maybe-entry (cl:elt backing-array index)))
        (when (and maybe-entry (<= (generation maybe-entry) generation))
          (setf (cl:elt backing-array index) nil)
          (hashset:delete used-ixs index))))))

(defmacro do-gix-array ((var gixarr &optional result) &body body)
  "Iterate over used entries of gixarr doing body to them."
  (let ((ix (gensym "index"))) 
    `(with-slots (backing-array used-ixs) ,gixarr
       (do-hashset (,ix used-ixs ,result)
         (let ((,var (value (cl:elt backing-array ,ix))))
           ,@body)))))

(defun test-0 ()
  "Ensure elt throws an error when out of bounds"
  (let ((garr (make-instance 'gix-array))
        (alloc (make-instance 'allocator)))
    (let ((ix (allocate alloc)))
      (handler-case (elt garr ix)
        (t nil)))))

(defun test-1 ()
  "Ensure setf and elt work with the same gix"
  (let ((garr (make-instance 'gix-array))
        (alloc (make-instance 'allocator)))
    (let ((ix (allocate alloc)))
      (setf (elt garr ix) 10)
      (assert (= (elt garr ix) 10)))))

(defun test-2 ()
  "Make sure delete works with the same index"
  (let ((garr (make-instance 'gix-array))
        (alloc (make-instance 'allocator)))
    (let ((ix (allocate alloc)))
      (setf (elt garr ix) 10)
      (delete garr ix)
      (assert (null (elt garr ix))))))

(defun test-3 ()
  "Make sure used-ixs updates correctly"
  (let ((garr (make-instance 'gix-array))
        (alloc (make-instance 'allocator)))
    (let ((ix (allocate alloc))
          (ix2 (allocate alloc)))
      (setf (elt garr ix) 1)
      (setf (elt garr ix2) 2)
      (assert (hashset:elt (used-ixs garr) (index ix)))
      (assert (hashset:elt (used-ixs garr) (index ix2)))
      (delete garr ix2)
      (assert (null (hashset:elt (used-ixs garr) (index ix2)))))))

(defun test-4 ()
  "Ensure setf of a later generation sets the entry's generation"
  (let ((garr (make-instance 'gix-array))
        (a (make-instance 'allocator)))
    (let ((ix (allocate a)))
      (deallocate a ix)
      (let ((ix2 (allocate a)))
        (setf (elt garr ix2) 1)
        (assert (= 1 (elt garr ix2)))))))

(defun test-5 ()
  "Ensure setf of a later generation overwrites previous generations"
  (let ((garr (make-instance 'gix-array))
        (a (make-instance 'allocator)))
    (let ((ix (allocate a)))
      (setf (elt garr ix) 1)
      (deallocate a ix)
      (let ((ix2 (allocate a)))
        (setf (elt garr ix2) 2)
        (assert (null (elt garr ix)))
        (assert (= 2 (elt garr ix2)))))))

(defun test-6 ()
  "Ensure elt only allows the same gix to access an element"
  (let ((garr (make-instance 'gix-array))
        (a (make-instance 'allocator)))
    (let ((ix (allocate a)))
      (setf (elt garr ix) 42)
      (deallocate a ix))

    (let ((ix (allocate a)))
      (assert (null (elt garr ix))))))

(defun run-tests ()
  (every #'null (mapcar #'funcall '(test-0 test-1 test-2 test-3 test-4 test-5 test-6))))

(run-tests)


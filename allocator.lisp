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
  ((is-live :type boolean
            :initarg :is-live)
   (generation :type integer
               :accessor generation
               :initarg :generation)))

(defclass gix ()
  ((index
    :reader index
    :initarg :index
    :type integer
    :documentation "Index into an array")
   (generation
    :initarg :generation
    :type integer
    :documentation "Generation that the array slot should be for valid access"))
  (:documentation "Generational index"))

(defmethod print-object ((g gix) out)
  (with-slots (index generation) g
    (format out "<gix: ix:~A, gen:~A>" index generation)))

(defclass allocator ()
  ((entries :accessor entries
            :initform (make-array 0
                                  :adjustable t
                                  :element-type 'entry
                                  :fill-pointer 0))
   (free :accessor free
         :initform nil))
  (:documentation "Allocator of generational indices.
Stores allocated indices: entries[i] = (g, live) represents that the ith index is of generation g"))

(defmethod print-object ((a allocator) out)
  (with-slots (entries free) a
    (format out "<allocator: entries: ~%~{~A~%~}~%free: ~%~{~A~%~}"
            (loop for e across entries collect e)
            free)))

(defmethod allocate ((a allocator))
  "Return a new generational index, with either a new index or an old index and a new generation."
  (with-slots (entries free) a
    ;; Make a new entry, returning a new index, or pull an old index from the free list
    (let ((next-index (if (null free)
                          (vector-push-extend (make-instance 'entry
                                                             :is-live nil
                                                             :generation -1)
                                              entries)
                          (pop free))))
      (let ((new-entry (elt entries next-index)))
        ;; If it's new or formerly free, it won't be alive
        (setf (slot-value new-entry 'is-live) t)
        (incf (generation new-entry))
        (make-instance 'gix :index next-index :generation (generation new-entry))))))

(defmethod deallocate ((a allocator) (gix gix))
  "Return an index to the free list.
Sets is-live to nil in the entries table, and adds the index to the free list.
"
  (with-slots (index generation) gix
    (with-slots (entries free) a
      (let ((cur-entry (elt entries index)))
        ;; Only free when it was live in the first place and it was the same generation.
        ;; This is to avoid a prior generation freeing a later one
        (when (and (slot-value cur-entry 'is-live) (= (generation cur-entry) generation))
          (setf (slot-value cur-entry 'is-live) nil)
          (push index free)
          t)))))

(defmethod is-live ((a allocator) (gix gix))
  "Checks if a generational index is live and valid"
  (with-slots (index generation) gix
    (with-slots (entries) a
      (let ((entry (elt entries index)))
        (and (slot-value entry 'is-live)
             (= generation (generation entry)))))))


;; Tests!
(defun test-0 ()
  "Ensure the allocator is created correctly with no entries and no free indices"
  (let ((a (make-instance 'allocator)))
    (assert (=  (length (entries a)) 0))
    (assert (null (free a)))))

(defun test-1 ()
  "Ensure indices are created correctly"
  (let ((a (make-instance 'allocator)))
    (let ((ix (allocate a)))
      (assert (zerop  (index ix)))
      (assert (zerop (slot-value ix 'generation))))
    (let ((ix (allocate a)))
      (assert (= 1 (index ix)))
      (assert (zerop (slot-value ix 'generation))))))

(defun test-2 ()
  "Ensure the liveliness of indices"
  (let ((a (make-instance 'allocator)))
    (let ((ix (allocate a))
          (ix2 (allocate a)))
      (assert (is-live a ix))
      (assert (is-live a ix2))
      (deallocate a ix)
      (assert (null (is-live a ix)))
      (assert (is-live a ix2))
      (assert (equal (free a) '(0))))))

(defun test-3 ()
  "Ensure repeated allocations push entries into the entry array"
  (let ((a (make-instance 'allocator)))
    (loop repeat 3 do (allocate a))
    (assert (= (index (allocate a)) 3))
    (assert (= (length (entries a)) 4))))

(defun test-4 ()
  "Ensure is-live accounts for generation"
  (let ((a (make-instance 'allocator)))
    (let ((ix (allocate a)))
      (deallocate a ix)
      (let ((ix2 (allocate a)))
        (assert (null (is-live a ix)))
        (assert (is-live a ix2))))))

(defun test-5 ()
  "Ensure deallocate doesn't hit future generations"
  (let* ((a (make-instance 'allocator))
         (ix (allocate a)))
    (deallocate a ix)
    (let ((ix2 (allocate a)))
      (deallocate a ix)
      (assert (is-live a ix2)))))

(defun run-tests ()
  (every #'null (mapcar #'funcall '(test-0 test-1 test-2 test-3 test-4 test-5))))

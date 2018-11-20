(in-package :gmap)

(defstruct gindex
  (index (error ":index not supplied"))
  (generation (error ":generation not supplied")))

(defstruct entry)
(defstruct (free-entry (:include entry))
  (next-free (error ":next-free not supplied")))

(defstruct (used-entry (:include entry))
  (value (error ":value not supplied"))
  (generation (error ":generation not supplied")))

(defparameter *default-gmap-size* 10)

(defclass gmap ()
  ((entries :initform (make-array *default-gmap-size*
                                  :adjustable t :fill-pointer 0)
            :accessor entries)
   (generation :initform 0 :accessor generation)
   (next-free :initform 0 :accessor next-free)))

(defmethod initialize-instance :after ((instance gmap) &key)
  (dotimes (i (1- *default-gmap-size*))
    (vector-push-extend
     (make-free-entry :next-free (+ i 1)) (entries instance)))
  (vector-push-extend
   (make-free-entry :next-free nil) (entries instance)))

(defmethod insert-val ((gmap gmap) value)
  (with-slots ((gm-entries entries)
               (gm-next-free next-free)
               (gm-gen generation)) gmap
    ;; make the new entry we're inserting
    (let ((new-entry (make-used-entry :value value
                                      :generation gm-gen)))
      (if (null gm-next-free)
          ;; if there's no more space left, push a new entry
          ;; CL will take care of doubling the size of the vector for us
          ;; return the index to the entry
          (let ((ix (make-gindex :index (length gm-entries)
                                  :generation gm-gen)))
            (vector-push-extend new-entry gm-entries)
            ix)
          ;; otherwise, look at the next free entry and make an index to it
          (let ((target-free-entry (elt gm-entries gm-next-free))
                (ix (make-gindex :index gm-next-free
                                  :generation gm-gen)))
            (if (free-entry-p target-free-entry)
                ;; if it's actually a free entry, replace it with the used entry
                ;; update the next free of the table
                ;; return the index to the entry
                (with-slots ((entry-next-free next-free)) target-free-entry
                  (setf (elt gm-entries gm-next-free) new-entry)
                  (setf gm-next-free entry-next-free)
                  ix)
                ;; if it's not actually a free entry, then the next-free of the
                ;; gmap doesn't point to a free entry, so the list is corrupted
                (error "Free list is corrupted")))))))

(defmethod remove-val ((gmap gmap) (gix gindex))
  (with-slots ((ix index) (ix-gen generation)) gix ; grab index stuff
    (with-slots (entries next-free (gm-gen generation)) gmap ; grap gmap stuff
      (let ((target-entry (elt entries ix))) ; get the entry in question
        (if (used-entry-p target-entry) ; if it's used
            (with-slots (value (used-entry-gen generation)) target-entry
              (if (= ix-gen used-entry-gen) ; and if the generation matches
                  ;; set the entry to a free entry targetting the next-free
                  ;; of the gmap
                  (progn (setf (elt entries ix) (make-free-entry
                                                 :next-free next-free))
                         ;; Further inserts will be in the next generation
                         (incf gm-gen)
                         ;; Set the gm's next-free to our location
                         (setf next-free ix)
                         ;; return what we removed
                         value)
                  nil))
            nil)))))


(defmethod get-val ((gmap gmap) (gix gindex))
  (with-slots ((ix index) (ix-gen generation)) gix ; grab index stuff
    (with-slots (entries next-free (gm-gen generation)) gmap ; grap gmap stuff
      (let ((target-entry (elt entries ix))) ; get the entry in question
        (if (used-entry-p target-entry) ; if it's used
            (with-slots (value (used-entry-gen generation)) target-entry
              (if (= ix-gen used-entry-gen) ; and if the generation matches
                  ;; set the entry to a free entry targetting the next-free
                  ;; of the gmap
                  (values value t)
                  (values nil nil)))
            (values nil nil))))))

(defmethod (setf get-val) (value (gmap gmap) (gix gindex))
  (with-slots ((ix index) (ix-gen generation)) gix ; grab index stuff
    (with-slots (entries next-free (gm-gen generation)) gmap ; grap gmap stuff
      (let ((target-entry (elt entries ix))) ; get the entry in question
        (if (used-entry-p target-entry) ; if it's used
            (with-slots ((old-value value) (used-entry-gen generation)) target-entry
              (if (= ix-gen used-entry-gen) ; and if the generation matches
                  ;; set the entry to a free entry targetting the next-free
                  ;; of the gmap
                  (setf old-value value)
                  nil))
            nil)))))

(defmethod storage-length ((gmap gmap))
  (length (entries gmap)))

(defmethod clear ((gmap gmap))
  (with-slots (entries next-free generation) gmap
    (let ((l (storage-length gmap))) 
      (dotimes (x (1- l))
        (setf (elt entries x) (make-free-entry :next-free (+ x 1))))
      (setf (elt entries (1- l)) (make-free-entry :next-free nil))
      (setf next-free 0)
      (setf generation 0))))

(defmethod reset ((gmap gmap))
  (with-slots (entries next-free generation) gmap
    (setf entries (make-array *default-gmap-size*
                              :adjustable t :fill-pointer 0))
    (setf next-free 0)
    (setf generation 0)
    (dotimes (i (1- *default-gmap-size*))
      (vector-push-extend
       (make-free-entry :next-free (+ i 1)) entries))
    (vector-push-extend
     (make-free-entry :next-free nil) entries)
    ))

(defmacro gmap-do ((var gmap) &body body)
  (let ((entry (gensym))
        (final-value (gensym)))
    `(with-slots (entries) ,gmap
       (loop with ,final-value = nil
          for ,entry across entries do
            (if (used-entry-p ,entry)
                (with-slots ((,var value)) ,entry
                  (setf ,final-value (progn ,@body))))
            finally (return ,final-value)))))

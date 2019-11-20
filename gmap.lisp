(defpackage :gmap
  (:use :cl)
  (:shadow :delete
           :elt
           :length)
  (:export :gix
           :gmap
           :insert
           :delete
           :elt
           :do-values))

(in-package :gmap)

(defstruct gix
  (ix (error ":ix not supplied"))
  (gen (error ":gen not supplied")))

(defstruct entry)
(defstruct (free-entry (:include entry))
  (next (error ":next-free not supplied")))
(defstruct (used-entry (:include entry))
  (value (error ":value not supplied"))
  (generation (error ":generation not supplied")))

(defparameter *default-gmap-size* 16)


;;; GMAP describes an array of entries, either free or used
;;; Start like this:
;;; ((free 1) (free 2) (free 3) ... (free nil)) ; next-free 0, generation 0
;;; On insert:
;;; ((used val 0) (free 2) (free 3) ... (free nil)) ; next-free 1, generation 0
;;;     returns (gix 0 0)
;;; On delete (gix 0 0)
;;; ((free (old next-free)) (free 2) ... (free nil)) ; next-free 0, generation 1
;;;
;;; On get (gix i g)
;;; Check g against generation, if match, return (ith element t), else (nil nil)
;;; On (set (get (gix i g)))
;;; Same checks, but do a setf


(defclass gmap ()
  ((entries :accessor entries
            :initform (make-array *default-gmap-size*
                                  :adjustable t
                                  :element-type 'entry
                                  :fill-pointer 0)
            :documentation "Entries for the generational map.")
   (length :accessor length
           :initform 0)
   (generation :initform 0)
   (next-free :initform 0)))


(defmethod initialize-instance :after ((instance gmap) &key)
  (with-slots (entries) instance
    (dotimes (i (1- *default-gmap-size*))
      (vector-push-extend (make-free-entry :next (1+ i)) entries))
    (vector-push-extend (make-free-entry :next nil) entries)))

(defmethod insert ((g gmap) value)
  (with-slots (entries next-free generation length) g
      (let ((new-entry (make-used-entry :value value :generation generation)))
        (if (null next-free)
            (let ((ix (make-gix :ix (cl:length entries) :gen generation)))
              (vector-push-extend new-entry entries)
              ix)
            (let ((ix (make-gix :ix next-free :gen generation))
                  (old-entry (cl:elt entries next-free)))
              (if (free-entry-p old-entry)
                  (progn (setf (cl:elt entries next-free) new-entry)
                         (setf next-free (free-entry-next old-entry))
                         (incf length)
                         ix)
                  (error "Free list corrupted")))))))

(defmethod delete ((g gmap) (i gix) &key)
  (with-slots (entries next-free (map-generation generation) length) g
    (with-slots (ix gen) i
      (let ((target-entry (cl:elt entries ix)))
        (if (used-entry-p target-entry)
            (with-slots (value generation) target-entry
              (when (= generation gen)
                (setf (cl:elt entries ix) (make-free-entry :next next-free))
                (incf map-generation)
                (setf next-free ix)
                (decf length)
                value)))))))

(defmethod elt ((g gmap) (i gix))
  (with-slots (entries next-free) g
    (with-slots (ix gen) i
      (let ((target-entry (cl:elt entries ix)))
        (if (used-entry-p target-entry)
            (with-slots (value generation) target-entry
              (if (= generation gen)
                  (values value t)
                  (values nil nil)))
            (values nil nil))))))

(defgeneric (setf elt) (value place index))
(defmethod (setf elt) (value (g gmap) (i gix))
  (with-slots (entries next-free) g
    (with-slots (ix gen) i
      (let ((target-entry (cl:elt entries ix)))
        (if (used-entry-p target-entry)
            (with-slots ((value-slot value) generation) target-entry
              (when (= generation gen)
                (setf value-slot value)))
            nil)))))

(defmethod get-ix ((g gmap) (i integer))
  (with-slots (entries generation) g
    (let ((target-entry (cl:elt entries i)))
      (if (used-entry-p target-entry)
          (make-gix :ix i :gen (used-entry-generation target-entry))
          (make-gix :ix i :gen generation)
          ))))

(defmacro do-values ((var gmap) &body body)
  (let ((entry (gensym))
        (final-value (gensym)))
    `(with-slots (entries) ,gmap
       (loop with ,final-value = nil
          for ,entry across entries do
            (when (used-entry-p ,entry)
              (with-slots ((,var value)) ,entry
                (setf ,final-value (progn ,@body))))
          finally (return ,final-value) ))))

(defun run-tests ()
  (let* ((g (make-instance 'gmap))
         (ixs (loop for i from 0 to 9
                 collect (insert g i))))
    (assert (= (length g) 10))
    (loop for i from 0 to 5 do (delete g (nth i ixs)))
    (assert (= (length g) 4))
    (setf (elt g (seventh ixs)) 100)
    (assert (= (elt g (seventh ixs)) 100))
    (assert (equalp (get-ix g 0) (make-gix :ix 0 :gen 6)))))

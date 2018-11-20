(in-package :storage)

(defgeneric insert-val (storage value)
  (:documentation "Insert value into storage"))

(defgeneric remove-val (storage index)
  (:documentation "Remove value at index from storage"))

(defgeneric get-val (storage index)
  (:documentation "Get value at index from storage"))

(defgeneric storage-length (storage)
  (:documentation "Get length of storage"))

(defgeneric clear (storage)
  (:documentation "Clear storage"))

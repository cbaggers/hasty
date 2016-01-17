(in-package #:hasty)

;;----------------------------------------------------------------------

(defstruct %component)

(def-typed-bag %component-bag %component (make-%component))

;;----------------------------------------------------------------------

(defstruct entity
  (components (bag-of-%component!) :type %component-bag))

(def-typed-bag entity-bag entity (make-entity))

;;----------------------------------------------------------------------

(let ((id -1))
  (defun %next-id () (incf id))
  (defun %reset-ids () (setf id 0)))

;;----------------------------------------------------------------------

(defun symb (&rest args)
  (intern (format nil "~{~a~}" args)))

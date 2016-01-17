(in-package #:hasty)

;;----------------------------------------------------------------------

(defstruct %component)

(def-typed-bag %component-bag %component (make-%component))

;;----------------------------------------------------------------------

(defstruct entity
  (components (bag-of-%component!) :type %component-bag))

(def-typed-bag entity-bag entity (make-entity))

;;----------------------------------------------------------------------

(defstruct %system
  (entities (error "system created without rummager")
	    :type entity-rummager)
  (pass-function (error "system created without pass function")
		 :type (function (entity) t)))

;;----------------------------------------------------------------------

(let ((id -1))
  (defun %next-id () (incf id))
  (defun %reset-ids () (setf id 0)))

;;----------------------------------------------------------------------

(defun symb (&rest args)
  (intern (format nil "~{~a~}" args)))

(defun kwd (&rest args)
  (intern (format nil "~{~a~}" args) 'keyword))

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
		 :type (function (entity) t))
  (friends nil :type list))

(defun sort-systems (systems)
  (let* ((scored (make-hash-table))
	 (to-sort (copy-list systems))
	 (count 0)
	 (limit (expt (length systems) 2)))
    (labels ((r (s)
	       (let ((f (%system-friends s)))
		 (cond
		   ((null f) (setf (gethash s scored) 1) nil)
		   ((every λ(gethash _ scored) f)
		    (setf (gethash s scored)
			  (reduce #'+ (mapcar λ(gethash _ scored) f)))
		    nil))
		 s)))
      (loop :until (or (> count limit) (null to-sort)) :do
	 (incf count)
	 (setf to-sort (remove nil (mapcar #'r to-sort))))
      (if (null to-sort)
	  (mapcar #'car
		  (sort (loop :for k :being :the :hash-keys :of scored :collect
			   (cons k (gethash k scored)))
			#'< :key #' cdr))
	  (error "Could not sort systems in a reasonable amount of time
You may have recursive system friendships:
~s" systems)))))

;;----------------------------------------------------------------------

(let ((id -1))
  (defun %next-id () (incf id))
  (defun %reset-ids () (setf id 0)))

;;----------------------------------------------------------------------

(defun symb (&rest args)
  (intern (format nil "~{~a~}" args)))

(defun kwd (&rest args)
  (intern (format nil "~{~a~}" args) 'keyword))

(in-package #:hasty)

;;----------------------------------------------------------------------

(defstruct %component)

(def-typed-bag %component-bag %component (make-%component))

;;----------------------------------------------------------------------

(defstruct entity
  (components (bag-of-%component!) :type %component-bag)
  (dirty nil :type boolean))

(def-typed-bag entity-bag entity (make-entity))

(defun %check-component-friendships-of-entity (entity)
  (let ((components (get-items-from-%component-bag (entity-components entity))))
    (labels ((friend-present-in-entity (friend)
	       (member friend components
		       :test (lambda (x) (typep x friend))))
	     (component-friendships-present (component)
	       (every #'friend-present-in-entity (%get-friends component))))
      (if (every #'component-friendships-present components)
	  (setf (entity-dirty entity) nil)
	  (error "The entity has the following components with missing friends:~%~s"
		 entity)))))

;; {TODO} entities can recieve messages from the moot, these are for moot wide
;;        things like undefining a type and as such dont need to be super
;;        performant.

;;----------------------------------------------------------------------

(defstruct %system
  (entities (error "system created without rummager")
	    :type entity-rummager)
  (pass-function (error "system created without pass function")
		 :type (function (entity) t))
  (event-based-p nil :type boolean)
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

(defmacro unless-release (&body body)
  (unless grab-bag::+release-mode+
    `(progn ,@body)))

;;----------------------------------------------------------------------

(defun symb (&rest args)
  (intern (format nil "~{~a~}" args)))

(defun kwd (&rest args)
  (intern (format nil "~{~a~}" args) 'keyword))

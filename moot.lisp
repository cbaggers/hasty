(in-package #:hasty)

;; {TODO} Tick driven Systems perform a pass when the moot's tick function is
;;        called

(defun %make-systems-array (&optional systems)
  (make-array (length systems) :element-type '%system
	      :initial-contents systems))

(let ((all-entities (bag-of-entity!))
      (systems (%make-systems-array)))
  (defun %rummage-master (predicate)
    "used internally by systems"
    (rummage-entity-bag all-entities predicate))

  (defun step-hasty ()
    (loop :for system :in systems :do
       (let ((entities (get-items-from-entity-bag (%system-entities system)))
	     (pass-function (%system-pass-function system)))
	 (loop :for entity :in entities :do
	    (funcall pass-function entity)))))

  (defun %add-system (system)
    (if (find system systems :test #'eq)
	(error "System has already been added")
	(let ((sorted (sort-systems
		       (cons system (loop :for s :in systems :collect s)))))
	  (setf systems (%make-systems-array sorted))
	  systems)))

  (defun %remove-system (system)
    (let ((sorted (sort-systems
		   (remove system (loop :for s :in systems :collect s)))))
      (setf systems (%make-systems-array sorted))
      systems)))

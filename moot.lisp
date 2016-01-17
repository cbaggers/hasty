(in-package #:hasty)

;; Tick driven Systems perform a pass when the moot's tick function is
;; called

(let ((all-entities (bag-of-entity!))
      (systems '&&&&&&&))

  (defun %rummage-master (predicate)
    "used internally by systems"
    (rummage-entity-bag all-entities predicate))

  (defun step-hasty ()
    (loop :for system :in systems :do
       (let ((entities (get-items-from-entity-bag (%system-entities system)))
	     (pass-function (%system-pass-function system)))
	 (loop :for entity :in entities :do
	    (funcall pass-function entity))))))

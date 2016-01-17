(in-package #:hasty)

(let ((all-entities (bag-of-entity!)))



  (defun %rummage-master (predicate)
    "used internally by systems"
    (rummage-entity-bag all-entities predicate)))

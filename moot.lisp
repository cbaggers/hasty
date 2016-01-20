(in-package #:hasty)

;; {TODO} Tick driven Systems perform a pass when the moot's tick function is
;;        called

(defun %make-systems-array (&optional systems)
  (make-array (length systems) :element-type '%system
	      :initial-contents systems))

;; (let ((all-entities (bag-of-entity!))
;;       (regular-systems (%make-systems-array))
;;       (pending-systems nil)))

(defvar all-entities (bag-of-entity!))
(defvar regular-systems (%make-systems-array))
(defvar reactive-systems
  (make-array 0 :element-type '%system :adjustable t :fill-pointer 0))
(defvar pending-systems nil)

(defun %rummage-master (predicate)
  "used internally by systems"
  (rummage-entity-bag all-entities predicate))

(defun step-hasty ()
  (when pending-systems
    (%commit-pending-systems))
  (loop :for system :across regular-systems :do (%run-pass system)))

(defun run-pass (system)
  (if (%system-reactive-p system)
      (%run-pass system)
      (error "Cannot manually trigger pass on non reactive system ~s"
	     system)))

(defun %run-pass (system)
  (let ((entities (get-items-from-entity-rummager
		   (%system-entities system)))
	(pass-function (%system-pass-function system)))
    (loop :for entity :across entities :do
       (unless-release
	 (when (entity-dirty entity)
	   (%check-component-friendships-of-entity entity)))
       (funcall pass-function entity))))

(defun %commit-pending-systems ()
  (if pending-systems
      (%add-systems pending-systems)
      (print "%commit-pending-systems: no pending systems found"))
  (setf pending-systems nil))

(defun %add-systems (systems)
  (let ((sorted (sort-systems
		 (append systems
			 (loop :for s :across regular-systems :collect s)))))
    (setf regular-systems (%make-systems-array sorted))
    regular-systems))

(defun add-system (system)
  (labels ((system-eq (x y) (= (%system-component-id x)
			       (%system-component-id y))))
    (let* ((reactive (%system-reactive-p system))
	   (systems-array (if reactive
			      reactive-systems
			      regular-systems))
	   (index (position-if (lambda (x) (system-eq x system))
			       systems-array)))
      (cond
	(index (setf (aref systems-array index) system))
	(reactive (vector-push-extend system reactive-systems))
	(t (setf pending-systems
		 (remove-duplicates (cons system pending-systems)
				    :test #'system-eq :from-end t))))))
  system)

(defun remove-system (system)
  (let ((sorted (sort-systems
		 (remove system (loop :for s :in regular-systems :collect s)))))
    (setf regular-systems (%make-systems-array sorted))
    regular-systems))

(defun register-entity (entity)
  (add-item-to-entity-bag all-entities entity)
  entity)

(defun unregister-entity (entity)
  (remove-item-from-entity-bag all-entities entity))

(defun %get-all-entities ()
  all-entities)

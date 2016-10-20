(in-package #:hasty)
(named-readtables:in-readtable fn:fn-reader)

;;----------------------------------------------------------------------

(defun mapcat (function &rest lists)
  (reduce #'append (apply #'mapcar function lists) :initial-value nil))

;;----------------------------------------------------------------------

(defgeneric component-name (component))
(defgeneric %set-id (component new-id))
(defgeneric %get-component-adder (component-type))
(defgeneric %get-component-remover (component-type))
(defgeneric %get-friends (component))
(defgeneric initialize-system (name))
(defgeneric get-system (name))
(defgeneric system-name (name))

;;----------------------------------------------------------------------

(defstruct %component)

(def-typed-bag %component-bag %component (make-%component))

;;----------------------------------------------------------------------

(defstruct (entity (:constructor %make-entity))
  (components (bag-of-%component!) :type %component-bag)
  (dirty nil :type boolean))

(defun %check-component-friendships-of-entity (entity)
  (let ((components (get-items-from-%component-bag (entity-components entity))))
    (labels ((friend-present-in-entity (friend)
	       (find-if (lambda (x) (typep x friend)) components))
	     (component-friendships-present (component)
	       (every #'friend-present-in-entity (%get-friends component))))
      (if (every #'component-friendships-present components)
	  (setf (entity-dirty entity) nil)
	  (error "The entity has the following components with missing friends:~%~s"
		 entity)))))

(defun make-entity () (%make-entity))

(defun entity! () (make-entity))

(def-typed-bag entity-bag entity (make-entity))

;; {TODO} entities can recieve messages from the moot, these are for moot wide
;;        things like undefining a type and as such dont need to be super
;;        performant.

;;----------------------------------------------------------------------

(defstruct %system
  (entities (error "system created without rummager")
	    :type entity-rummager)
  (pass-function (error "system created without pass function")
		 :type (function (entity) t))
  (reactive-p nil :type boolean)
  (friends nil :type list)
  (component-id -1 :type fixnum)
  (debug-id -1 :type fixnum))

(defun sort-systems (systems)
  (let* ((scored (make-hash-table :test #'eq))
	 (to-sort (copy-list systems))
	 (count 0)
	 (limit (expt (length systems) 2)))
    (labels ((r (s)
	       (let ((f (%system-friends s)))
		 (cond
		   ((null f)
		    (setf (gethash s scored) 1)
		    nil)
		   ((every λ(gethash (get-system _) scored) f)
		    (setf (gethash s scored)
			  (1+ (reduce #'+ (mapcar
					   λ(gethash (get-system _) scored)
					   f))))
		    nil)
		   (t s)))))
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

;;----------------------------------------------------------------------

(defmacro bind-many (args list &body body)
  (labels ((&-p (x)
	     (when (symbolp x)
	       (char= #\& (aref (symbol-name x) 0)))))
    (let* ((clean-args (remove-if #'&-p args))
	   (clean-names (mapcar λ(if (listp _) (first _) _) clean-args))
	   (gsyms (mapcar λ(gensym (symbol-name _)) clean-names))
	   (extract (gensym "extract")))
      `(let ,gsyms
	 (labels ((,extract ,args
		    ,@(loop :for g :in gsyms :for a :in clean-names :collect
			 `(push ,a ,g))))
	   (loop :for x :in ,list :do (apply #',extract x))
	   (let ,(loop :for g :in gsyms :for a :in clean-names :collect
		    `(,a (reverse ,g)))
	     ,@body))))))

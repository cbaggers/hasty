(in-package #:hasty)
(named-readtables:in-readtable fn-reader)

;; (def-component test9 ()
;;     ((x 0s0 :type single-float)
;;      (y 0s0 :type single-float))
;;   (update-test9 (+ test9-x 1) (+ test9-y 2)))

(defvar debug-id-source -1)

(defmacro def-component (name depends-on (&rest slot-descriptions) &body pass-body)
  (assert (and (listp depends-on)
	       (symbolp (first depends-on))
	       (not (some #'keywordp (rest depends-on)))
	       (symbolp name)
	       (every #'symbolp depends-on)
	       (not (member name depends-on))))
  (assert (valid-slot-descriptions-p slot-descriptions))
  (let* ((system-name (symb name :-system))

	 (reactive (eq (first depends-on) :reactive))
	 (friends (if reactive (rest depends-on) depends-on))

	 (let-id (symb :%- (format nil "~{~s~^-~}"
				   (map 'list #'char-code (symbol-name name)))))
	 (id (gensym "id"))

	 (init (symb :make- name))
	 (hidden-init (symb :%make- name))

	 (with (symb :with- name))

	 (update (symb :update))
	 (has (symb :has- name))
	 (add (symb :add- name))
	 (remove (symb :remove- name))

	 (original-slot-names (mapcar #'first slot-descriptions))
	 (hidden-slot-names (mapcar λ(gensym (symbol-name _))
				    original-slot-names))
	 (init-pairs (mapcan λ`(,(kwd _) ,_1)
			     hidden-slot-names
			     original-slot-names))

	 (system-init (symb :initialize- system-name))
	 (get-system (symb :get- system-name))
	 (pass (gensym "pass"))
	 (entity (symb :entity)))
    `(progn
       ;; the component itself
       (defstruct (,name (:include %component) (:conc-name nil))
	 ,@(mapcar λ`(,_ ,@(rest _1)) hidden-slot-names slot-descriptions))

       (defstruct (,system-name (:include %system) (:constructor ,hidden-init)))

       ,(unless grab-bag::+release-mode+ `(defvar ,let-id (%next-id)))

       (let ((,id ,(if grab-bag::+release-mode+
		       (%next-id)
		       `(if (boundp ',let-id)
			    (symbol-value ',let-id)
			    (%next-id)))))

	 ;; local get from entity helper func {TODO} should inline this
	 (labels ((%get-from (entity)
		    (get-item-from-%component-bag-at
		     (entity-components entity) ,id)))

	   ;; make public getters as component getters are hidden
	   ,@(mapcar λ`(defun ,(symb name :- _) (entity) (,_1 (%get-from entity)))
		     original-slot-names hidden-slot-names)

	   (defmacro ,with (entity slot-names &body body)
	     (unless (and (listp slot-names)
			  (>= (length slot-names) 1)
			  (every (lambda (x) (find x ',original-slot-names))
				 slot-names))
	       (error ,(format nil "~%~s:~%Invalid slot names. Must be a list of one or more of the following:~%~s" with original-slot-names)))
	     (gen-slot-accessors entity slot-names ',hidden-slot-names body))

	   (defun ,has (entity)
	     (has-item-in-%component-bag-at
	      (entity-components entity) ,id))

	   (defun ,add (entity-to-add-to &key ,@(mapcar λ`(,_ ,(second _1))
							original-slot-names
							slot-descriptions))
	     (let ((component (,init ,@init-pairs)))
	       (add-item-to-%component-bag-at
		(entity-components entity-to-add-to) component ,id))
	     (unless-release
	       (setf (entity-dirty entity-to-add-to) t))
	     entity-to-add-to)

	   (defun ,remove (entity-to-remove-from)
	     (remove-item-from-%component-bag-at
	      (entity-components entity-to-remove-from) ,id)
	     entity-to-remove-from)

	   ,(unless grab-bag::+release-mode+
		    `(defmethod %set-id ((component ,name) new-id)
		       (error "Updating underlying component ids not yet")))

	   (defmethod component-name ((component ,name))
	     (declare (ignore component))
	     ',name)

	   (defmethod component-name ((component ,system-name))
	     (declare (ignore component))
	     ',name)

	   (defmethod %get-component-adder ((component ,name))
	     (declare (ignore component))
	     #',add)

	   (defmethod %get-component-adder ((component-type (eql ',name)))
	     (declare (ignore component-type))
	     #',add)

	   (defmethod %get-component-remover ((component-type (eql ',name)))
	     (declare (ignore component-type))
	     #',remove)

	   (defmethod %get-friends ((component ,name))
	     ',friends)

	   (defun ,pass (,entity)
	     ;; cant use our with-macro from earlier as it's not yet compiled
	     ;; so we locally define it here and use it immediately
	     (macrolet ((,with (,entity &body body)
			  (gen-slot-accessors
			   ,entity ',original-slot-names ',hidden-slot-names body)))
	       (,with ,entity
		      (labels ((,update (,@original-slot-names)
				 (let ((component (%get-from ,entity)))
				   ,@(mapcar λ`(setf (,_ component) ,_1)
					     hidden-slot-names
					     original-slot-names))))
			(declare (ignorable (function ,update)))
			,@pass-body))))

	   (let ((created nil))
	     (defun ,system-init ()
	       (when created
		 (error ,(format nil "system for ~s has already been instantiated"
				 name)))
	       (setf created
		     (add-system
		      (,hidden-init
		       :entities (%rummage-master #',has)
		       :pass-function #',pass
		       :friends ',friends
		       :reactive-p ,reactive
		       :component-id ,id
		       :debug-id (incf debug-id-source)))))
	     (,system-init)
	     (defmethod initialize-system ((name (eql ',system-name)))
	       (,system-init))
	     (defun ,get-system ()
	       (or created (error "system does has not been initialized")))
	     (defmethod get-system ((name (eql ',name)))
	       (,get-system))
	     (defmethod get-system ((name ,name))
	       (,get-system))))))))


(defun gen-slot-accessors (entity human-slot-names gensym-slot-names body)
  (let* ((gensym-func-names (mapcar λ(gensym (symbol-name _))
				      human-slot-names))
	 (c-inst (gensym "component")))
    ;; get this component from the entity
    `(let* ((,c-inst (%get-from ,entity)))
       (declare (ignorable ,c-inst))
       ;; make the functions to access the slots, we need this
       ;; as otherwise you could setf
       (labels ,(mapcar λ`(,_ () (,_1 ,c-inst))
			gensym-func-names gensym-slot-names)
	 (declare (ignorable
		   ,@(mapcar λ`(function ,_) gensym-func-names)))
	 ;; make symbol macros so we can use the above functions like
	 ;; symbols again
	 (symbol-macrolet ,(mapcar λ`(,_ (,_1))
				   human-slot-names
				   gensym-func-names)
	   ,@body)))))

(defun valid-slot-descriptions-p (slot-descriptions)
  (labels ((valid-slot (s)
	     (destructuring-bind (name default &key type) s
	       (declare (ignore default type))
	       (and (listp s) (symbolp name)))))
    (every #'valid-slot slot-descriptions)))

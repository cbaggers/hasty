(in-package #:hasty)
(named-readtables:in-readtable fn-reader)

;; (def-component test9-non-opt ()
;;     ((x 0s0 :type single-float))
;;   (update :x (+ x 1)))

;; (def-component (test9-opt :optimize t) ()
;;     ((x 0s0 :type single-float)
;;      (y 0s0 :type single-float))
;;   (update :x (+ x 1) :y (+ y 2)))

(defvar debug-id-source -1)

(defun hidden-name (&rest x)
  (intern (format nil "~a~{-~a~}" (package-name (symbol-package (first x)))
		  (mapcar #'symbol-name x))
	  (find-package :hasty-hidden)))

(defmacro def-component (name-and-options depends-on (&rest slot-descriptions)
			 &body pass-body)
  (destructuring-bind (name &key optimize) (if (listp name-and-options)
					       name-and-options
					       (list name-and-options))
    (assert (and (listp depends-on)
		 (symbolp (first depends-on))
		 (not (some #'keywordp (rest depends-on)))
		 (symbolp name)
		 (every #'symbolp depends-on)
		 (not (member name depends-on))))
    (assert (valid-slot-descriptions-p slot-descriptions))
    (bind-many (original-slot-names init-vals &key (type t) accessor)
	slot-descriptions
      (let* ((types type)
	     (accessors accessor)
	     (system-name (symb name :-system))
	     (payload (symb :% name :-payload))
	     (payload-type (hidden-name (symb name :-payload)))

	     (reactive (eq (first depends-on) :reactive))
	     (friends (if reactive (rest depends-on) depends-on))

	     (let-id (hidden-name (symb name :-id)))
	     (id (gensym "id"))

	     (init (symb :make- name))
	     (non-optimal-init (symb :%%make- name))
	     (hidden-init (symb :%make- system-name))

	     (with (symb :with- name))

	     (update (symb :update))
	     (update-component (symb :update- name))
	     (has (symb :has- name))
	     (add (symb :add- name))
	     (remove (symb :remove- name))

	     (slot-names (mapcar λ(symb :% name :- _) original-slot-names))
	     (getter-names (mapcar λ(symb name :- _) original-slot-names))

	     (add-args (mapcar #'list original-slot-names init-vals))
	     (add-pairs (mapcat λ`(,(kwd _) ,_1)
				slot-names
				original-slot-names))

	     (non-opt-init-args (mapcar #'list slot-names init-vals))
	     (non-opt-init-pairs (mapcat λ`(,(kwd _) ,_) slot-names))

	     (update-args (mapcar λ`(,_ nil ,(symb :set _)) original-slot-names))

	     (system-init (symb :initialize- system-name))
	     (get-system (symb :get- system-name))
	     (pass (hidden-name (symb name :-PASS)))
	     (component (gensym "component"))
	     (entity (symb :entity)))
	`(progn
	   ,@(when (not optimize)
		   `((defclass ,payload-type ()
		       ,(mapcar λ`(,_ :initarg ,(kwd _) :initform ,_1)
				slot-names init-vals))
		     ,@(mapcar λ`(defun ,_ (x)
				   (slot-value
				    (slot-value x ',payload)
				    ',_))
			       slot-names)
		     ,@(mapcar λ`(defun (setf ,_) (v x)
				   (setf (slot-value
					  (slot-value x ',payload)
					  ',_)
					 v))
			       slot-names)
		     (defun ,init (&key ,@non-opt-init-args)
		       (,non-optimal-init
			,(kwd payload) (make-instance
					',payload-type
					,@non-opt-init-pairs)))))

	   ;; the component itself
	   (defstruct (,name (:include %component) (:conc-name nil)
			     ,@(when (not optimize)
				     `((:constructor ,non-optimal-init))))
	     ,@(if optimize
		   (mapcar λ`(,_ ,_1 :type ,_2)
			   slot-names
			   init-vals
			   types)
		   `((,payload (make-instance ',payload-type)
			       :type ,payload-type))))

	   (defstruct (,system-name (:include %system) (:constructor ,hidden-init)))

	   ,(unless grab-bag::+release-mode+ `(defvar ,let-id (%next-id)))

	   (let ((,id ,(if grab-bag::+release-mode+
			   (%next-id)
			   `(if (boundp ',let-id)
				(symbol-value ',let-id)
				(%next-id)))))

	     (progn
	       ,(when original-slot-names
		      `(defmacro ,with (slot-names entity &body body)
			 (unless (and (listp slot-names)
				      (>= (length slot-names) 1)
				      (every (lambda (x) (find x ',original-slot-names))
					     slot-names))
			   (error ,(format nil "~%~s:~%Invalid slot names. Must be a list of one or more of the following:~%~s~%You gave~~s" with original-slot-names)
				  slot-names))
			 (let ((inst (gensym "inst"))
			       (lookup ',(mapcar #'cons original-slot-names slot-names)))
			   `(let* ((,inst (get-item-from-%component-bag-at
					   (entity-components ,entity) ,,id)))
			      (declare (ignorable ,inst))
			      (symbol-macrolet ,(mapcar λ`(,_ (,(cdr (assoc _ lookup))
								,inst))
							slot-names)
				,@body)))))

	       ,@(mapcar λ`(defun ,_ (entity)
			     (,_1 (get-item-from-%component-bag-at
				   (entity-components entity) ,id)))
			 getter-names
			 slot-names)

	       ,@(mapcar λ`(defun (setf ,_) (value entity)
			     (setf (,_1 (get-item-from-%component-bag-at
					 (entity-components entity) ,id))
				   value))
			 getter-names
			 slot-names)

	       ,@(remove
		  nil
		  (mapcar λ(when _ `(defmethod ,_ ((entity entity))
				      (,_1 entity)))
			  accessors
			  getter-names))

	       ,@(remove
		  nil
		  (mapcar λ(when _
			     `(defmethod (setf ,_) (value (entity entity))
				(setf (,_1 entity) value)))
			  accessors
			  getter-names))

	       (defun ,has (entity)
		 (has-item-in-%component-bag-at
		  (entity-components entity) ,id))

	       (defun ,add (entity-to-add-to &key ,@add-args)
		 (let ((component (,init ,@add-pairs)))
		   (add-item-to-%component-bag-at
		    (entity-components entity-to-add-to) component ,id))
		 (unless-release
		   (setf (entity-dirty entity-to-add-to) t))
		 (touch-entity-item all-entities entity-to-add-to
				    :error-if-missing nil)
		 entity-to-add-to)

	       (defun ,remove (entity-to-remove-from)
		 (remove-item-from-%component-bag-at
		  (entity-components entity-to-remove-from) ,id)
		 (touch-entity-item all-entities entity-to-remove-from
				    :error-if-missing nil)
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

	       ,(when original-slot-names
		      `(defun ,update-component (entity &key ,@update-args)
			 (let ((,component (get-item-from-%component-bag-at
					    (entity-components entity) ,id)))
			   ,@(mapcar
			      λ`(when ,(third _1)
				  (setf (,_ ,component) ,(first _1)))
			      slot-names
			      update-args))))

	       (defun ,pass (,entity)
		 ;; cant use our with-macro from earlier as it's not yet compiled
		 ;; so we locally define it here and use it immediately
		 ,(if original-slot-names
		      `(let ((,component (get-item-from-%component-bag-at
					  (entity-components ,entity) ,id)))
			 (symbol-macrolet ,(mapcar λ`(,_ (,_1 ,component))
						   original-slot-names
						   slot-names)
			   (labels ((,update (&key ,@update-args)
				      ,@(mapcar
					 λ`(when ,(third _1)
					     (setf (,_ ,component) ,(first _1)))
					 slot-names
					 update-args)))
			     (declare (ignorable (function ,update)))
			     ,@pass-body)))
		      `(progn ,@pass-body)))

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
		   (,get-system))))))))))

(defun valid-slot-descriptions-p (slot-descriptions)
  (labels ((valid-slot (s)
	     (destructuring-bind (name default &key type accessor) s
	       (declare (ignore default type accessor))
	       (and (listp s) (symbolp name)))))
    (every #'valid-slot slot-descriptions)))

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

(defun hidden-name (x)
  (intern (format nil "~a-~a" (package-name (symbol-package x)) (symbol-name x))
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
    (let* ((system-name (symb name :-system))
	   (payload (symb :% name :-payload))
	   (payload-type (symb name :-payload))

	   (reactive (eq (first depends-on) :reactive))
	   (friends (if reactive (rest depends-on) depends-on))

	   (let-id (symb :%- (format nil "~{~s~^-~}"
				     (map 'list #'char-code (symbol-name name)))))
	   (id (gensym "id"))

	   (init (symb :make- name))
	   (non-optimal-init (symb :%%make- name))
	   (hidden-init (symb :%make- system-name))

	   (with (symb :with- name))

	   (update (symb :update))
	   (has (symb :has- name))
	   (add (symb :add- name))
	   (remove (symb :remove- name))

	   (original-slot-names (mapcar #'first slot-descriptions))
	   (hidden-slot-names (mapcar #'hidden-name original-slot-names))

	   (init-args (mapcar λ`(,_ ,(second _1))
			      original-slot-names
			      slot-descriptions))
	   (init-pairs (mapcan λ`(,(kwd _) ,_1)
			       hidden-slot-names
			       original-slot-names))

	   (non-opt-init-args (mapcar λ`(,_ ,(second _1))
				      hidden-slot-names
				      slot-descriptions))
	   (non-opt-init-pairs (mapcan λ`(,(kwd _) ,_)
				       hidden-slot-names))

	   (update-args (mapcar λ`(,_ nil ,(symb :set _)) original-slot-names))

	   (system-init (symb :initialize- system-name))
	   (get-system (symb :get- system-name))
	   (pass (hidden-name 'pass))
	   (component (gensym "component"))
	   (entity (symb :entity)))
      `(progn
	 ,@(when (not optimize)
		 `((defclass ,payload-type ()
		     ,(mapcar λ`(,_ :initarg ,(kwd _)
				    :initform ,(second _1))
			      hidden-slot-names
			      slot-descriptions))
		   ,@(mapcar λ`(defun ,_ (x)
				 (slot-value
				  (slot-value x ',payload)
				  ',_))
			     hidden-slot-names)
		   ,@(mapcar λ`(defun (setf ,_) (v x)
				 (setf (slot-value
					(slot-value x ',payload)
					',_)
				       v))
			     hidden-slot-names)
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
		 (mapcar λ`(,_ ,@(rest _1)) hidden-slot-names slot-descriptions)
		 `((,payload (make-instance ',payload-type)
			     :type ,payload-type))))

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

	     (defmacro ,with (slot-names entity &body body)
	       (unless (and (listp slot-names)
			    (>= (length slot-names) 1)
			    (every (lambda (x) (find x ',original-slot-names))
				   slot-names))
		 (error ,(format nil "~%~s:~%Invalid slot names. Must be a list of one or more of the following:~%~s~%You gave~~s" with original-slot-names)
			slot-names))
	       (gen-slot-accessors entity slot-names ',original-slot-names
				   ',hidden-slot-names body ,id))

	     (defun ,has (entity)
	       (has-item-in-%component-bag-at
		(entity-components entity) ,id))

	     (defun ,add (entity-to-add-to &key ,@init-args)
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
			     ,entity ',original-slot-names ',original-slot-names
			     ',hidden-slot-names body ',id)))
		 (let ((,component (%get-from ,entity)))
		   (,with ,entity
			  (labels ((,update (&key ,@update-args)
				     ,@(mapcar
					λ`(when ,(third _1)
					    (setf (,_ ,component) ,(first _1)))
					hidden-slot-names
					update-args)))
			    (declare (ignorable (function ,update)))
			    ,@pass-body)))))

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
		 (,get-system)))))))))

(defun gen-slot-accessors (entity needed-slots human-slot-names
			   gensym-slot-names body id)
  (let* ((name-pairs (mapcar #'list human-slot-names gensym-slot-names))
	 (needed (intersection name-pairs needed-slots
			       :test λ(eq (car _) _1)))
	 (human-slot-names (mapcar #'first needed))
	 (gensym-slot-names (mapcar #'second needed))
	 (gensym-func-names (mapcar λ(gensym (symbol-name _))
				    human-slot-names))

	 (c-inst (gensym "component")))
    ;; get this component from the entity
    `(let* ((,c-inst (get-item-from-%component-bag-at
		      (entity-components ,entity) ,id)))
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

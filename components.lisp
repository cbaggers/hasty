(in-package #:hasty)
(named-readtables:in-readtable fn-reader)

;; TODO:
;; dependant components: When you add a component to an entity you need
;;                       to check that the friend's of your component's
;;                       system as present

(defmacro def-component (name friends (&rest slot-descriptions) &body pass-body)
  (assert (valid-slot-descriptions-p slot-descriptions))
  (let* ((system-name (symb name :-system))

	 (id (gensym "id"))

	 (init (symb :make- name))
	 (hidden-init (symb :%make- name))

	 (with (symb :with- name))

	 (update (symb :update- name))
	 (has (symb :has- name))
	 (add (symb :add- name))
	 (remove (symb :remove- name))

	 (original-slot-names (mapcar #'first slot-descriptions))
	 (hidden-slot-names (mapcar λ(gensym (symbol-name _))
				    original-slot-names))
	 (init-pairs (mapcan λ`(,(kwd _) ,_1)
			     hidden-slot-names
			     original-slot-names))
	 (with-names (mapcar λ(symb name :- _) original-slot-names))

	 (c-inst (gensym "component")))

    ;;
    `(progn
       ;; the component itself
       (defstruct (,name (:conc-name nil))
	 ,@(mapcar λ`(,_ ,@(rest _1)) hidden-slot-names slot-descriptions))

       (defstruct (,system-name (:include %system) (:constructor ,hidden-init)))

       (let ((,id `(if grab-bag::+release-mode+
		    (%next-id)
		    `(%next-id))))

	 ;; local get from entity helper func {TODO} should inline this
	 (labels ((%get-from (entity)
		    (get-item-from-%component-bag-at
		     (entity-components entity) ,id)))

	   ;; make public getters as component getters are hidden
	   ,@(mapcar λ`(defun ,_ (entity) (,_1 (%get-from entity)))
		     original-slot-names hidden-slot-names)

	   ,@(make-with-component with with-names c-inst hidden-slot-names)

	   (defun ,has (entity)
	     (not (null (%get-from entity))))

	   (defun ,add (entity-to-add-to ,@original-slot-names)
	     (let ((component (,init ,@init-pairs)))
	       (add-item-to-%component-bag-at
		(entity-components entity-to-add-to) component ,id))
	     entity-to-add-to)

	   (defun ,remove (entity-to-remove-from)
	     (remove-item-from-%component-bag-at
	      (entity-components entity-to-remove-from) ,id)
	     entity-to-remove-from)

	   ,(unless grab-bag::+release-mode+
		    `(defmethod %set-id ((component ,name) new-id)
		       (error "Updating underlying component ids not yet")))

	   (defmethod %get-component-adder ((component-type (eql ',name)))
	     #',add)

	   (defmethod %get-component-remover ((component-type (eql ',name)))
	     #',remove)

	   ,@(def-system system-name with update hidden-init name friends
			 pass-body hidden-slot-names original-slot-names
			 c-inst hidden-slot-names with with-names))))))

(defun make-with-component (with-name with-names c-inst getters)
  `((defmacro ,with-name (entity &body body)
      (make-with-component-internals entity ',with-names ',c-inst ',getters body))))


(defun make-with-component-internals (entity with-names c-inst getters body)
  (let* ((hiding-getter-names
	  (mapcar λ(gensym (symbol-name _)) with-names)))
    `(let* ((,c-inst (%get-from ,entity)))
       (declare (ignorable ,c-inst))
       (labels ,(mapcar λ`(,_ () (,_1 ,c-inst))
			hiding-getter-names getters)
	 (symbol-macrolet ,(mapcar λ`(,_ (,_1))
				   with-names hiding-getter-names)
	   ,@body)))))

(defun valid-slot-descriptions-p (slot-descriptions)
  t)

;;----------------------------------------------------------------------

;; systems to be only able to modify one type of component but
;; view many what they can view has to be declared.

(defun def-system (system-name with update hidden-init primary-component-type
		   friends pass-body hidden-slot-names original-slot-names
		   C-INST GETTERS WITH-NAME WITH-NAMES)
  (assert (and (symbolp primary-component-type)
	       (every #'symbolp friends)
	       (not (member primary-component-type friends))))
  (let* ((primary primary-component-type)
	 (init (symb :make- system-name))
	 (pass (gensym "pass"))
	 (predicate (symb system-name :-p))
	 (body
	  (reduce λ`((,(symb :with- _1) entity ,@_))
		  friends :initial-value pass-body)))
    `((defun ,pass (entity)
	(macrolet ((,with-name (entity &body body)
		     (make-with-component-internals
		      entity ',with-names ',c-inst ',getters body)))
	  (,with entity
		 (labels ((,update (,@original-slot-names)
			    (let ((component (%get-from entity)))
			      ,@(mapcar λ`(setf (,_ component) ,_1)
					hidden-slot-names
					original-slot-names))))
		   ,@body))))

      (let ((created nil))
	(defun ,init ()
	  (when created
	    (error ,(format nil "system for ~s has already been instantiated"
			    primary)))
	  (setf created t)
	  (,hidden-init
	   :entities (%rummage-master #',predicate)
	   :pass-function #',pass
	   :friends ',friends))))))

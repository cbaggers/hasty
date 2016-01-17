(in-package #:hasty)
(named-readtables:in-readtable fn-reader)

;; TODO:
;; dependant components: so to add one it checks for the other.
;; It is NOT inheritance the components are still seperate things.
;; An example is that movable needs position

(defmacro def-component (name &body slot-descriptions)
  (assert (valid-slot-descriptions-p slot-descriptions))
  (let* ((init (symb :make- name))

	 (with (symb :with- name))
	 (update (symb :update- name))
	 (has (symb :has- name))
	 (add (symb :add- name))
	 (remove (symb :remove- name))

	 (original-slot-names (mapcar #'first slot-descriptions))
	 (hidden-slot-names (mapcar λ(gensym (symbol-name _))
				    original-slot-names))
	 (init-pairs (mapcar λ`(,(kwd _) ,_) original-slot-names))
	 (with-names (mapcar λ(symb name :- _) original-slot-names))

	 (c-inst (gensym "component")))

    ;;
    `(let ((id `(if grab-bag::+release-mode+
		    (%next-id)
		    `(%next-id))))
       ;; the component itself
       (defstruct (,name (:conc-name nil)) ,@slot-descriptions)

       ;; local get from entity helper func {TODO} should inline this
       (labels ((%get-from (entity)
		  (get-item-from-%component-bag-at
		   (entity-components entity) id)))

	 ;; make public getters as component getters are hidden
	 ,@(mapcar λ`(defun ,_ (entity) (,_1 (%get-from entity)))
		   original-slot-names hidden-slot-names)

	 ,(make-with-component with with-names c-inst hidden-slot-names)

	 (defun ,update (entity ,@original-slot-names)
	   (let ((component (%get-from entity)))
	     (mapcar λ`(setf (,_ component) ,_1)
		     hidden-slot-names
		     original-slot-names)))

	 (defun ,has (entity)
	   (not (null (%get-from entity))))

	 (defun ,add (entity-to-add-to ,@original-slot-names)
	   (let ((component (,init ,@init-pairs)))
	     (add-item-to-%component-bag-at (entity-components entity)
					    component
					    id))
	   entity)

	 (defun ,remove (entity-to-remove-from)
	   (remove-item-from-%component-bag-at (entity-components entity) id)
	   entity)

	 ,(unless grab-bag::+release-mode+
		  `(defmethod %set-id ((component ,name) new-id)
		     (error "Updating underlying component ids not yet")))

	 (defmethod %get-component-adder ((component-type (eql ,name)))
	   #',add)

	 (defmethod %get-component-remover ((component-type (eql ,name)))
	   #',remove)))))

(defun make-with-component (with with-names c-inst getters)
  `(defmacro ,with (entity &body body)
     (make-with-component-internals ',with-names ',c-inst ',getters body)))

(defun make-with-component-internals (with-names c-inst getters body
				      &optional editable)
  (let* ((hiding-getter-names
	  (mapcar λ(gensym (symbol-name _)) with-names)))
    (if editable
	`(let ((,c-inst (%get-from entity)))
	   (macrolet ,(mapcar λ`(,_ () (,_1 ,c-inst))
			      with-names getters)
	     ,@body))
	`(let ((,c-inst (%get-from entity)))
	   (labels ,(mapcar λ`(,_ () (,_1 ,c-inst))
			    hiding-getter-names getters)
	     (macrolet ,(mapcar λ`(,_ (,_1 ,c-inst))
				with-names hiding-getter-names)
	       ,@body))))))

(defun valid-slot-descriptions-p (slot-descriptions)
  t)

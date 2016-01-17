(in-package #:hasty)
(named-readtables:in-readtable fn-reader)



(defmacro def-component (name &body slot-descriptions)
  (assert (valid-slot-descriptions-p slot-descriptions))
  (let* ((conc (symb :% name :-))
	 (with (symb :with- name))
	 (update (symb :update- name))
	 (slot-names (mapcar #'first slot-descriptions))
	 (with-names (mapcar λ(symb name :- _) slot-names))
	 (getters (mapcar λ(symb conc _) slot-names))
	 (c-inst (gensym "component")))

    ;;
    `(let ((id (%next-id)))
       ;; the component itself
       (defstruct (,name (:conc-name ,conc)) ,@slot-descriptions)

       ;; local get from entity helper func {TODO} should inline this
       (labels ((%get-from (entity)
		  (get-item-from-%component-bag-at
		   (entity-components entity) id)))

	 ;; make public getters as component getters are hidden
	 ,@(mapcar λ`(defun ,_ (entity) (,_1 (%get-from entity)))
		   slot-names getters)

	 ,(make-with-component with with-names c-inst getters)

	 (defun ,update (entity &key ,@wi))))))

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

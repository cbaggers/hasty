(in-package #:hasty)

;;----------------------------------------------------------------------

(defmethod print-object ((object entity) stream)
  (format stream "#<ENTITY~@[ ~s~]>"
	  (map 'list #'component-name
	   (get-items-from-%component-bag
	    (entity-components object)))))

(defun add-component (entity component)
  (funcall (%get-component-adder component) entity)
  entity)

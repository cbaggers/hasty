(in-package #:hasty)

;; Systems are associated with one component type.

;; Systems can be either be tick-driven or event-driven

;; if only the relevant system can edit a component, what
;; is the point of a component with no system?

;; dependant systems: Systems can be declared order dependent, thus it
;; is an error to start a pass on a system before the systems it is
;; dependant on. These checks are turned off in release-mode

;; systems to be only able to modify one type of component but
;; view many what they can view has to be declared.

(defmacro def-system (name (primary-component-type &key friends)
		      &body pass-body)
  (assert (and (symbolp name)
	       (symbolp primary-component-type)
	       (every #'symbolp friends)
	       (not (member name friends))))
  (let ((primary primary-component-type)
	(hidden-init (symb :%make- name))
	(init (symb :make- name))
	(pass (gensym "pass"))
	(predicate (symb name :-p)))
    `(let ((created nil))
       (defstruct (,name (:include %system) (:constructor ,hidden-init)))

       (defun ,pass (entity)
	 ;; TODO: add setf'able with-* stuff for primary, and
	 ;;       add readonly with-* stuff for friends
	 ,@pass-body)

       (defun ,init ()
	 (when created
	   (error ,(format nil "system for ~s has already been instantiated"
			   primary)))
	 (setf created t)
	 (,hidden-init
	  :entities (%rummage-master #',predicate)
	  :pass-function #',pass
	  :friends ',friends)))))




;;----------------------------------------------------------------------
;; Event-driven-system

;; Event-driven systems only run passes when a certain event is
;; triggered

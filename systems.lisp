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
  (let ((primary primary-component-type)
	(hidden-init (symb :%make- name))
	(init (symb :make- name))
	(pass (gensym "pass")))
    `(progn
       (defstruct (,name (:include %system) (:constructor ,hidden-init))
	 )
       (defun ,pass (entity)
	 ;; TODO: add setf'able with-* stuff for primary, and
	 ;;       add readonly with-* stuff for friends
	 ,@body)
       (defun ,init ()
	 (,hidden-init
	  :entities (%rummage-master #',predicate)
	  :pass-function #',pass)))))



;;----------------------------------------------------------------------
;; Event-driven-system

;; Event-driven systems only run passes when a certain event is
;; triggered

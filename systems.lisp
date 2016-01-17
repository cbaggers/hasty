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

(defmacro def-system (name primary-component-type &key friends depends-on)
  `(defstruct (,name (:include %system)))
  )



;;----------------------------------------------------------------------
;; Event-driven-system

;; Event-driven systems only run passes when a certain event is
;; triggered

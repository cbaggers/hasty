;;;; package.lisp

(defpackage #:hasty
  (:use #:cl #:grab-bag #:fn #:structy-defclass)
  (:export :step-hasty :run-pass
	   :entity :make-entity :entity! :register-entity :unregister-entity
	   :def-component :component-name
	   :add-system :remove-system :initialize-system :get-system))

(defpackage #:hasty-hidden
  (:use #:cl))

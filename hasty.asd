;;;; hasty.asd

(asdf:defsystem #:hasty
  :description "A Simple Entity Component System"
  :author "Baggers <techsnuffle@gmail.com>"
  :license "BSD 2 Clause"
  :serial t
  :depends-on (#:grab-bag #:fn)
  :components ((:file "package")
               (:file "base")))

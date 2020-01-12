(uiop:define-package :network-simulator/generators/all
    (:nicknames :ns-gen)
  (:use :common-lisp)
  (:use-reexport :network-simulator/generators/generics
                 :network-simulator/generators/hierarchy
		 :network-simulator/generators/distributed))

(uiop:define-package :network-simulator/generators/generics
    (:nicknames :ns-gen-base)
  (:use :common-lisp)
  (:export #:generate-connections))

(in-package :ns-gen-base)


(defgeneric generate-connections (policy channel-builder))

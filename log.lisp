(uiop:define-package :network-simulator/log
    (:nicknames :ns-log)
  (:use :common-lisp)
  (:export #:*network-log*))

(in-package :ns-log)


(defvar *network-log*)

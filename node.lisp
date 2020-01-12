(uiop:define-package :network-simulator/node
    (:nicknames :ns-node)
  (:use :common-lisp :alexandria :anaphora)
  (:use :network-simulator/utils)
  (:export #:node
	   #:build-node
	   #:node-channels
	   #:node-channels-count
	   #:node-interfaces
	   #:node-interface-generator))

(in-package :ns-node)


(defstruct node
  id channels channels-count interface-generator interfaces)

(defun build-node (nodes density id)
  (setf (gethash id nodes)
	(make-node :id id
		   :channels-count density
		   :channels (make-node-map density)
		   :interface-generator (make-integer-generator 1)
		   :interfaces (make-node-map density))))













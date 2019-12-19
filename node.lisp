(uiop:define-package :network-simulator/node
    (:nicknames :ns-node)
  (:use :common-lisp :alexandria :anaphora)
  (:use :network-simulator/utils)
  (:export #:node
	   #:build-node
	   #:node-channels
	   #:node-channels-count))

(in-package :ns-node)


(defstruct node
  id channels channels-count)

(defun build-node (nodes density id)
  (setf (gethash id nodes)
	(make-node :id id
		   :channels-count density
		   :channels (make-node-map density))))













(uiop:define-package :network-simulator/node
    (:nicknames :ns-node)
  (:use :common-lisp :alexandria :anaphora)
  (:use :network-simulator/utils)
  (:export #:node
	   #:build-node
	   #:node-channels
	   #:node-channels-count
	   #:build-channel
	   #:make-channel-builder))

(in-package :ns-node)


(defstruct node
  id channels channels-count)

(defstruct channel-builder
  error-probability
  duplex-p
  weight-generator)

(defstruct channel
  error-probability
  duplex-p
  weight)

(defun build-channel (channel-builder)
  (make-channel 
   :error-probability (channel-builder-error-probability channel-builder)
   :duplex-p (funcall (channel-builder-duplex-p channel-builder))
   :weight (funcall (channel-builder-weight-generator channel-builder))))

(defun build-node (nodes density id)
  (setf (gethash id nodes)
	(make-node :id id
		   :channels-count density
		   :channels (make-node-map density))))













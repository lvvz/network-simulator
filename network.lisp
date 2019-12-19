(uiop:define-package :network-simulator/network
    (:nicknames :ns-net)
  (:use :common-lisp)
  (:use :network-simulator/utils
	:network-simulator/node)
  (:export #:*network*
	   #:network
	   #:network-nodes
	   #:network-satellite-node-id
	   #:make-network-builder
	   #:build-network
	   #:network-builder

	   #:get-node
	   #:remove-node
	   #:remove-channel

	   #:*channels*
	   #:*channels-id-generator*

	   #:build-channel
	   #:make-channel-builder

	   #:channel-from
	   #:channel-to
	   #:channel-weight
	   #:channel-duplex-p))

(in-package :ns-net)


(defvar *network*)

(defvar *channels*)

(defvar *channels-id-generator*)

(defstruct channel-builder
  error-probability
  duplex-p
  weight-generator)

(defstruct channel
  from to
  error-probability
  duplex-p
  weight)

(defun build-channel (channel-builder from to)
  (let ((channel
	  (make-channel
	   :from from
	   :to to
	   :error-probability (channel-builder-error-probability channel-builder)
	   :duplex-p (funcall (channel-builder-duplex-p channel-builder))
	   :weight (funcall (channel-builder-weight-generator channel-builder)))))
    (setf (gethash (funcall *channels-id-generator*)
		   *channels*)
	  channel)))

(defstruct network
  satellite-node-id
  nodes)

(defun get-node (id)
  (gethash id (network-nodes *network*)))

(defstruct network-builder
  satellite-channels-count
  communication-nodes-count
  density-generator
  common-channel-builder
  satellite-channel-builder)

(defun build-network (network-builder)
  (with-slots (satellite-channels-count
	       communication-nodes-count
	       density-generator
	       common-channel-builder
	       satellite-channel-builder)
      network-builder
    (let* ((id-generator (make-integer-generator))
	   (nodes (make-node-map (1+ communication-nodes-count)))
	   (satellite-node-id (funcall id-generator)))
      (build-node nodes satellite-channels-count
		  satellite-node-id)
      (dotimes (i communication-nodes-count)
	(build-node nodes (funcall density-generator) 
		    (funcall id-generator)))
      (make-network :nodes nodes
		    :satellite-node-id satellite-node-id))))

(defun remove-link (from-node-id to-node-id)
  (remhash to-node-id
	   (node-channels
	    (gethash from-node-id
		     (network-nodes *network*)))))

(defun remove-channel (node1 node2)
  (remove-link node1 node2)
  (remove-link node2 node1))

(defun remove-node (node-id)
  (maphash (lambda (to-node-id channel)
	     (declare (ignore channel))
	     (remove-channel node-id to-node-id))
	   (node-channels (gethash node-id *network*)))
  (remhash node-id *network*))

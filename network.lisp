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

	   #:channel-id
	   #:channel-from
	   #:channel-to
	   #:channel-weight
	   #:channel-duplex-p
	   #:channel-address

	   #:build-interface
	   #:interface-address
	   #:interface-channel-id

	   #:create-address-generator
	   #:*address-generator*
	   #:make-ip-address
	   #:ip-address-value
	   #:ip-address-last))

(in-package :ns-net)


(defvar *network*)

(defvar *channels*)

(defvar *channels-id-generator*)

(defvar *address-generator*)

(defstruct ip-address
  value last)

(defun create-address-generator ()
  (let ((num-gen (make-integer-generator)))
    (lambda ()
      (make-ip-address :value (funcall num-gen) :last 0))))

(defmethod print-object ((object ip-address) stream)
  (with-slots (value last) object
    (let ((main (mod value 256))
	  (secondary (floor value 256)))
      (format stream "10.~A.~A.~A" main secondary last))))

(defstruct channel-builder
  error-probability
  duplex-p
  weight-generator)

(defstruct channel
  id
  from to
  address
  error-probability
  duplex-p
  weight)

(defstruct interface
  channel-id
  address)

(defun build-interface (address ch-id)
  (make-interface :address address :channel-id ch-id))

(defun build-channel (channel-builder from to)
  (let* ((id (funcall *channels-id-generator*))
	 (channel
	  (make-channel
	   :id id
	   :from from
	   :to to
	   :error-probability (channel-builder-error-probability channel-builder)
	   :address (funcall *address-generator*)
	   :duplex-p (funcall (channel-builder-duplex-p channel-builder))
	   :weight (funcall (channel-builder-weight-generator channel-builder)))))
    (setf (gethash id *channels*)
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

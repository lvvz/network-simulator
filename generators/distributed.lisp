(uiop:define-package :network-simulator/generators/distributed
    (:nicknames :ns-gen-distributed)
  (:use :common-lisp :alexandria :anaphora)
  (:use :network-simulator/generators/generics
	:network-simulator/network
        :network-simulator/node)
  (:export #:distributed-network))

(in-package :ns-gen-distributed)


(defstruct distributed-network)

(defun node-availiable-p (id)
  (< (hash-table-count (node-channels (get-node id)))
     (node-channels-count (get-node id))))

(defun cleanup-availiability (id availiable-nodes)
  (unless (node-availiable-p id)    
    (remhash id availiable-nodes)))

(defun create-connection (channel id to-node-id availiable-nodes)
  (setf (gethash id (node-channels (get-node to-node-id)))
	channel)
  (let ((interface-id (funcall (node-interface-generator (get-node to-node-id)))))
    (setf (gethash id (node-interfaces (get-node to-node-id)))
	  (build-interface (make-ip-address :value (ip-address-value (channel-address channel))
					    :last interface-id)
			   id)))
  (cleanup-availiability to-node-id availiable-nodes))

(defun create-channel (id to-node-id channel-builder availiable-nodes)
  (when (and (node-availiable-p id)
	     (node-availiable-p to-node-id))
    (let ((channel (build-channel channel-builder id to-node-id)))
      (create-connection channel id to-node-id availiable-nodes)
      (create-connection channel to-node-id id availiable-nodes)
      to-node-id)))

(defun create-random-channel (id availiable-nodes channel-builder)
  (let* ((availiable-node-ids (remove id
				      (remove-if (rcurry #'gethash (node-channels (get-node id)))
						 (hash-table-keys availiable-nodes))))
	 (availiable-node-ids (or (remove-if-not (lambda (id)
						   (= 0 (hash-table-count (node-channels (get-node id)))))
						 availiable-node-ids)
				  availiable-node-ids))
	 (availiable-nodes-count (length availiable-node-ids)))
    (unless (= 0 availiable-nodes-count)
      (let ((to-node-id (nth (random availiable-nodes-count)
			     availiable-node-ids)))
	(create-channel id to-node-id channel-builder availiable-nodes)))))

(defun create-random-channel-recursively (id availiable-nodes channel-builder)
  (dotimes (_ (node-channels-count (get-node id)))
    (awhen (create-random-channel id availiable-nodes channel-builder)
      (create-random-channel-recursively it availiable-nodes channel-builder))))

(defun create-united-random-network (channel-builder)
  (let ((availiable-nodes (alexandria:copy-hash-table (network-nodes *network*))))
    (create-random-channel-recursively (network-satellite-node-id *network*)
				       availiable-nodes channel-builder)))

(defmethod generate-connections ((policy distributed-network) channel-builder)
  (create-united-random-network channel-builder))

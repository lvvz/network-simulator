(uiop:define-package :network-simulator/init
    (:nicknames :ns-init)
  (:use :common-lisp :alexandria)
  (:use :network-simulator/utils
        :network-simulator/log
	:network-simulator/network
	:network-simulator/node
	:network-simulator/generators/all
        :network-simulator/routing
	:network-simulator/dot)
  (:export #:init-network))

(in-package :ns-init)


(defun init-network (satellite-channels-count
		     communication-nodes-count
		     density)
  (setf ns-log:*NETWORK-LOG* *standard-output*)
  (princ "Network build starts" *network-log*)
  (setf *channels* (make-node-map))
  (let ((*channels-id-generator* (make-integer-generator)))
    (setf *address-generator* (create-address-generator))
    (setf *network*
	  (build-network
	   (make-network-builder
	    :satellite-channels-count satellite-channels-count
	    :communication-nodes-count communication-nodes-count
	    :density-generator (make-random-generator-from-range
				(1- density)
				(1+ density))
	    :common-channel-builder (make-channel-builder
				     :error-probability 0.01
				     :duplex-p (make-random-boolean-generator)
				     :weight-generator (make-random-generator-from #(2 3 7 8 12 14 16 18 20 21 24 29)))
	    :satellite-channel-builder (make-channel-builder
					:error-probability 0.01
					:duplex-p (make-random-boolean-generator)
					:weight-generator (make-random-generator-from #(2 3 7 8 12 14 16 18 20 21 24 29))))))
    ;; (generate-connections nil (make-channel-builder
    ;; 			     :error-probability 0.01
    ;; 			     :duplex-p (make-random-boolean-generator)
    ;; 			     :weight-generator (make-random-generator-from #(2 3 7 8 12 14 16 18 20 21 24 29))))
    (generate-connections (make-instance 'distributed-network)
			  ;; (make-instance 'hierarchical-network)
			  (make-channel-builder
			   :error-probability 0.01
			   :duplex-p (make-random-boolean-generator)
			   :weight-generator (make-random-generator-from #(2 3 7 8 12 14 16 18 20 21 24 29))))
    (setf *routes-table*
	  (let ((*routes-table* (make-node-map)))
	    (propagate-route-table)
	    (network-dot)
	    *routes-table*
	    ;; (break "~A" *routes-table*)
	    ))))

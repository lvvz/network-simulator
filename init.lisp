(uiop:define-package :network-simulator/init
    (:nicknames :ns-init)
  (:use :common-lisp :alexandria)
  (:use :network-simulator/utils
        :network-simulator/log
	:network-simulator/network
	:network-simulator/generators/all)
  (:export #:init-network))

(in-package :ns-init)


(defun init-network ()
  (princ "Network build starts" *network-log*)
  (setf *network*
	(build-network
	 (make-network-builder
	  :satellite-channels-count 2
	  :communication-nodes-count 36
	  :density-generator (make-random-generator-from-range 2 4)
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
  (generate-connections (make-instance 'united-random-connection)
			;; (make-instance 'hierarchical-network)
			(make-channel-builder
			 :error-probability 0.01
			 :duplex-p (make-random-boolean-generator)
			 :weight-generator (make-random-generator-from #(2 3 7 8 12 14 16 18 20 21 24 29)))))

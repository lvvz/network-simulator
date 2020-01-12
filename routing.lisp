(uiop:define-package :network-simulator/routing
    (:nicknames :ns-routing)
  (:use :common-lisp :alexandria :anaphora)
  (:use :network-simulator/network
	:network-simulator/utils)
  (:export #:run-routing
	   #:propagate-route-table
	   #:*routes-table*))

(in-package :ns-routing)


(defun run-routing ()
  ())

(defvar *routes-table*)

(defun weight> (w1 w2)
  (or (eq w1 :inf)
      (eq w2 :inf)
      (> w1 w2)))

(defun weight+ (w1 w2)
  (if (or (eq w1 :inf)
	  (eq w2 :inf))
      :inf
      (+ w1 w2)))

(defun create-route-table (id)
  (let ((weight-table (make-node-map))
	(route-table (make-node-map)))
    (maphash (lambda (id node)
	       (declare (ignore node))
	       (setf (gethash id weight-table)
		     (make-array (hash-table-count (network-nodes *network*))
				 :initial-element :inf))
	       (setf (gethash id route-table)
		     (make-array (hash-table-count (network-nodes *network*))
				 :initial-element nil)))
	     (network-nodes *network*))
    (setf (aref (gethash id weight-table) 0) 0)
    (do ((i 1 (1+ i)))
	((>= i (hash-table-count (network-nodes *network*))))
      (maphash (lambda (id channel)
		 (declare (ignore id))
		 (labels ((%update-weights (from to)
			    (let ((maybe-new-weight (weight+ (aref (gethash from
									    weight-table)
								   (1- i))
							     (channel-weight channel))))
			      (unless (eq :inf maybe-new-weight)
				(when (weight> (aref (gethash to weight-table) i)
					       maybe-new-weight)
				  (setf (aref (gethash to weight-table) i)
					maybe-new-weight)
				  (setf (aref (gethash to route-table) i)
					from))))))
		   (%update-weights (channel-from channel) (channel-to channel))
		   (%update-weights (channel-to channel) (channel-from channel))))
	       *channels*))
    ;; (break "~A" weight-table)
    (values weight-table route-table)))

(defun create-route-table-by-node-count (id)
  (let ((route-table (make-node-map)))
    (maphash (lambda (id node)
	       (declare (ignore node))
	       (setf (gethash id route-table)
		     (make-array (hash-table-count (network-nodes *network*))
				 :initial-element nil)))
	     (network-nodes *network*))
    (push id (aref (gethash id route-table) 0))
    (do ((i 1 (1+ i)))
	((>= i (hash-table-count (network-nodes *network*))))
      (maphash (lambda (id channel)
		 (declare (ignore id))
		 (labels ((%update-weights (from to)
			    (let ((path-before (aref (gethash from route-table)
						     (1- i))))
			      (when path-before
				(push from (aref (gethash to route-table) i))))))
		   (%update-weights (channel-from channel) (channel-to channel))
		   (%update-weights (channel-to channel) (channel-from channel))))
	       *channels*))
    route-table))

(defun restore-path-1 (to last-idx routes-table)
  ;; (unless to (break "~A" to routes-table))
  (when to
    (when (> last-idx 0)
      (let ((prev-node-id (aref (gethash to routes-table)
				last-idx)))
	(cons prev-node-id
	      (restore-path prev-node-id
			    (1- last-idx)
			    routes-table))))))

(defun restore-path (to last-idx routes-table)
  ;; (unless to (break "~A" to routes-table))
  (when to
    (when (> last-idx 0)
      (let* ((prev-node-ids (aref (gethash to routes-table)
				  last-idx)))
	;; (break "rp ~A ~A" prev-node-ids (mapcan (lambda (prev-node-id)
	;; 	  (aif (restore-path prev-node-id
	;; 			     (1- last-idx)
	;; 			     routes-table)
	;; 	       (mapcar (curry #'cons prev-node-id) it)
	;; 	       (list (list prev-node-id))))
	;; 	prev-node-ids) )
	(mapcan (lambda (prev-node-id)
		  (aif (restore-path prev-node-id
				     (1- last-idx)
				     routes-table)
		       (mapcar (curry #'cons prev-node-id) it)
		       (list (list prev-node-id))))
		prev-node-ids)
	;; (break "rp : ~A ~A ~A" (mapcar #'cons prev-node-ids restored-paths) prev-node-ids restored-paths)
	;; restored-paths
	;; (if restored-paths
	;;     (mapcar #'cons prev-node-ids restored-paths)
	;;     (mapcar #'list prev-node-ids)
	;;     )
	))))

(defun find-index-if (fn list)
  (let ((idx -1))
    (find-if (lambda (x) (prog1 (funcall fn x)
			   (incf idx)))
	     list)
    idx))

(defun evaluate-shortest-paths-1 (to routes-table)
  (list (cons to
	      (restore-path to
			    (find-index-if #'identity
					   (coerce (gethash to routes-table) 'list))
			    routes-table))))

(defun evaluate-shortest-paths (to routes-table)
  (mapcar (curry #'cons to)
	  (restore-path to
			(find-index-if #'identity
				       (coerce (gethash to routes-table) 'list))
			routes-table)))

;; (defun evaluate-widest-paths (to routes-table weight-table)
;;   (break "~A" weight-table)
;;   ;; (cons to
;;   ;; 	(restore-path to
;;   ;; 		      (reduce (lambda (min-idx current)
;;   ;; 				(if (and current
;;   ;; 					 (> (aref (gethash to weight-table)
;;   ;; 						  min-idx)
;;   ;; 					    )))))))
;;   )

;; (defun propagate-route-table1 ()
;;   (maphash (lambda (id node)
;; 	     (declare (ignore node))
;; 	     (multiple-value-bind (weight-table routes-table) (create-route-table id)
;; 	       (setf (gethash id *routes-table*)
;; 		     (alist-hash-table
;; 		      (mapcar (lambda (to)
;; 				(cons to
;; 				      ;; (evaluate-widest-paths to routes-table weight-table)
;; 				      (evaluate-shortest-paths to routes-table)))
;; 			      (remove id (hash-table-keys (network-nodes *network*)))))))
;; 	     )
;; 	   (network-nodes *network*)))

(defun propagate-route-table ()
  (maphash (lambda (id node)
	     (declare (ignore node))
	     (let ((routes-table (create-route-table-by-node-count id)))
	       ;; (break "~A" routes-table)
	       (setf (gethash id *routes-table*)
		     (alist-hash-table
		      (mapcar (lambda (to)
				(cons to
				      ;; (evaluate-widest-paths to routes-table weight-table)
				      (evaluate-shortest-paths to routes-table)))
			      (remove id (hash-table-keys (network-nodes *network*)))))))
	     )
	   (network-nodes *network*)))

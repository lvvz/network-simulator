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
		 (let ((maybe-new-weight (weight+ (aref (gethash (channel-from channel)
								 weight-table)
							(1- i))
						  (channel-weight channel))))
		   (unless (eq :inf maybe-new-weight)
		     (when (weight> (aref (gethash (channel-to channel) weight-table) i)
				    maybe-new-weight)
		       (setf (aref (gethash (channel-to channel) weight-table) i)
			     maybe-new-weight)
		       (setf (aref (gethash (channel-to channel) route-table) i)
			     (channel-from channel))))))
	       *channels*))
    ;; (break "~A" weight-table)
    route-table))

(defun restore-path (to last-idx routes-table)
  ;; (unless to (break "~A" to routes-table))
  (when to
    (when (> last-idx 0)
      (let ((prev-node-id (aref (gethash to routes-table)
				last-idx)))
	(cons prev-node-id
	      (restore-path prev-node-id
			    (1- last-idx)
			    routes-table))))))

(defun find-index-if (fn list)
  (let ((idx -1))
    (find-if (lambda (x) (prog1 (funcall fn x)
			   (incf idx)))
	     list)
    idx))

(defun evaluate-shortest-paths (to routes-table)
  (cons to
	(restore-path to
		      (find-index-if #'identity
				     (coerce (gethash to routes-table) 'list))
		      routes-table)))

;; (defun evaluate-widest-paths (to routes-table weight-table)
;;   (restore-path to
;; 		(reduce (lambda (min-idx current)
;; 			  (if (and current
;; 				   (> (aref (gethash to weight-table)
;; 					    min-idx)
;; 				      )))))))

(defun propagate-route-table ()
  (maphash (lambda (id node)
	     (declare (ignore node))
	     (let ((routes-table (create-route-table id))
	     	   ;; (widest-paths-table (make-node-map))
	     	   ;; (shortest-paths-table (make-node-map))
		   )
	       (setf (gethash id *routes-table*)
		     (alist-hash-table
		      (mapcar (lambda (to)
				(cons to
				      (evaluate-shortest-paths to routes-table)))
			      (remove id (hash-table-keys (network-nodes *network*)))))))
	     )
	   (network-nodes *network*)))

(uiop:define-package :network-simulator/dot
    (:nicknames :ns-dot)
  (:use :common-lisp)
  (:use :ns-net :ns-node)
  (:export #:network-dot))

(in-package :ns-node)


(defun channels-dot ()
  (let ((visited (make-node-map 
		  (hash-table-count (network-nodes *network*)))))
    (with-output-to-string (s)
      (maphash (lambda (id node)
		 (maphash (lambda (dst dst-node)
			    (declare (ignore dst-node))
			    (unless (gethash dst visited)
			      (format s "~A -- ~A [label=\"~A\"];~%" id dst
				      (channel-weight (gethash id (node-channels (get-node dst)))))))
			  (node-channels node))
		 (setf (gethash id visited) t))
	       (network-nodes *network*)))))

(defun network-dot ()
  (write-string-into-file
   (format nil "graph {~A~%~A~%}"
	   ""
	   (channels-dot))
   "/home/t/Documents/net.dot"
   :if-exists :supersede
   :if-does-not-exist :create))

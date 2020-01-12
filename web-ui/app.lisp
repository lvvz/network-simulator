(uiop:define-package :network-simulator/web-ui/app
    (:nicknames :ns-ui)
  (:use :common-lisp :hunchentoot)
  (:use :network-simulator/web-ui/net)
  (:export #:start-server))

(in-package :ns-ui)

;;;;;;;;;;;;;;;;;;;;;;;;; Server Boilerplate ;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass vhost (tbnl:acceptor)
  ((dispatch-table
    :initform '()
    :accessor dispatch-table
    :documentation "List of dispatch functions"))
  (:default-initargs
   :address (error "Host address must be specified.")
   :document-root nil
   :error-template-directory nil
   :persistent-connections-p t))

(defun create-dispatcher (exact-prefix method handler)
  (check-type exact-prefix string)
  (check-type method symbol)
  (check-type handler (or symbol function))
  (lambda (request)
    (and (eq method (tbnl:request-method request))
         (if (string= exact-prefix (tbnl:script-name request))
             handler
             nil))))

(defmethod tbnl:acceptor-dispatch-request ((vhost vhost) request)
  (mapc (lambda (dispatcher)
          (let ((handler (funcall dispatcher request)))
            (when handler
              (return-from tbnl:acceptor-dispatch-request (funcall handler request)))))
        (dispatch-table vhost))
  (call-next-method))


(defmethod tbnl:acceptor-status-message ((acceptor vhost) (http-status-code (eql #.tbnl:+http-internal-server-error+)) &key error &allow-other-keys)
  (declare (ignore error))
  "the server has dun goofed")


;;;;;;;;;;;;;;;;;;; Functions that will be routed ;;;;;;;;;;;;;;;;;;;;

;;; Each of these functions should take a REQUEST as input.
;;;
;;; POST data can be retrieved with
;;;
;;;     (hunchentoot:raw-post-data :request request :force-text t)

(defun hello (request)
  (declare (ignore request))
  "Hello!")

(defun say-number (request)
  (let* ((number-string (tbnl:get-parameter "number" request))
         (parsed (and number-string (parse-integer number-string :junk-allowed t))))
    (cond
      ((null number-string)
       "Provide a number with <tt>?number=</tt><em>n</em>!")
      (parsed
       (format nil "~R" parsed))
      (t
       "You gave me something that really isn't a number."))))

;;;;;;;;;;;;;;;;;;;;;;; Server Initialization ;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *app* nil)

(defparameter *routes* '(("/"    :get initialize-network)
                         ("/say" :get say-number)
			 ("/net" :get initialize-network)
			 ("/nodes" :get get-nodes)
			 ("/edges" :get get-edges)
			 ("/gen-net" :post generate-network)
			 ("/rt-cols" :get rt-cols)
			 ("/rt-data" :post rt-data)
			 ("/rt-shortest-paths" :post rt-shortest-paths)
			 ("/send-message" :post send-message)))

(defun start-server ()
  ;; Some optional configuration.
  (setq tbnl:*show-lisp-errors-p* t
        tbnl:*show-lisp-backtraces-p* t
        tbnl:*catch-errors-p* t)
  (tbnl:reset-session-secret)
  (setq tbnl:*default-connection-timeout* 15)
  ;; (Re-)start the app.
  (unless (null *app*)
    (stop-server))
  (setq *app*
        (make-instance 'vhost
                       :address "127.0.0.1"
                       :port 3945
                       :taskmaster (make-instance 'tbnl:one-thread-per-connection-taskmaster)))
  
  ;; Install the routes onto *APP*.
  (dolist (route *routes*)
    (destructuring-bind (uri method handler) route
      (push (create-dispatcher uri method handler) (dispatch-table *app*))))

  (tbnl:start *app*))

(defun stop-server ()
  (unless (null *app*)
    (tbnl:stop *app*)
    (setq *app* nil)))

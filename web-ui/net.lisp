(uiop:define-package :network-simulator/web-ui/net
    (:nicknames :ns-ui-net)
  (:use :common-lisp :cl-who :alexandria :yason)
  (:use :network-simulator/network
   :network-simulator/utils)
  (:export #:initialize-network
	   #:get-nodes
	   #:get-edges))

(in-package :ns-ui-net)


(defun get-nodes (request)
  (declare (ignore request))
  (with-output-to-string* (:indent t)
    (encode (mapcar (lambda (id)
		      (alist-hash-table
		       (list (cons "id" id)
			     (cons "label" (format nil "RT-~A" id))
			     (cons "color"
				   (if (eq id (network-satellite-node-id *network*))
				       (alist-hash-table
					(list (cons "border" "#367377")
					      (cons "background" "#c98c88")))
				       (alist-hash-table
					(list (cons "border" "#367377")
					      (cons "background" "#439095"))))))))
		    (hash-table-keys (network-nodes *network*)))
	    yason::*json-output*)))

(defun get-edges (request)
  (declare (ignore request))
  (with-output-to-string* (:indent t)
    (encode (mapcar (lambda (id)
		      (let ((channel (gethash id *channels*)))
			(alist-hash-table
			 (list (cons "from" (channel-from channel))
			       (cons "to" (channel-to channel))
			       (cons "label" (format nil "~A" (channel-weight channel)))
			       (if (channel-duplex-p channel)
				   (cons "width" 4)
				   (cons "dashes" t))))))
		    (hash-table-keys *channels*))
	    yason::*json-output*)))

(defmacro build-net-auto-gen-form ()
  `(:p "YIIIIIIIZ"))

(defmacro build-menu-html ()
  `(let ((card-root "card-root"))
     (macrolet ((%build-net-auto-gen-form ()
		  `(:p "Yiiiiiiiz")))
       (htm (:div :id card-root
		  
		  (let ((cards (list (list "c1" "Автоматична генерація" ;; "Згенерувати"
					   'build-net-auto-gen-form)
				     (list "c2" "Тестування"
					   'build-net-auto-gen-form))))
		    (loop :for (card title body) :in cards
			  :do (let ((card-id-expr (format nil "#~A" card))
				    (card-root-id-expr (format nil "#~A" card-root)))
				(htm
				 (:div :class "card"
				       (:div :class "card-header"
					     (:a :class "collapsed card-link"
						 :href card-id-expr
						 :data-toggle "collapse"
						 (str title)))
				       (:div :id card
					     :class "collapse"
					     :data-parent card-root-id-expr
					     (:div :class "card-body"
						   ;; (htm ,(macroexpand-1 (eval 'body))
						   ;; 	)
						   ))))))))))))

(defun initialize-network (request)
  (declare (ignore request))
  ;; (read-file-into-string "~/.quicklisp/local-projects/network-simulator/web-ui/index.html")
  (let* ((index-script (read-file-into-string "~/.quicklisp/local-projects/network-simulator/web-ui/index.js"))
  	 (index-css (read-file-into-string "~/.quicklisp/local-projects/network-simulator/web-ui/index.css"))
  	 (stream (make-string-output-stream)))
    (with-html-output (_ stream :indent t :prologue t)
      (:html
       (:head
  	(:title "Мережа передачі даних")
  	(:link :rel "stylesheet" :href "https://maxcdn.bootstrapcdn.com/bootstrap/3.4.1/css/bootstrap.min.css")
  	(:script :type "text/javascript" :src "https://unpkg.com/vis-network/standalone/umd/vis-network.min.js")
  	(loop :for script :in '("https://ajax.googleapis.com/ajax/libs/jquery/3.4.1/jquery.min.js"
  				"https://cdnjs.cloudflare.com/ajax/libs/popper.js/1.16.0/umd/popper.min.js"
  				"https://maxcdn.bootstrapcdn.com/bootstrap/4.4.1/js/bootstrap.min.js")
  	      :do (htm (:script :src script)))
  	(:link :rel "stylesheet" :href "https://maxcdn.bootstrapcdn.com/bootstrap/4.4.1/css/bootstrap.min.css")
  	(:style :type "text/css" (str index-css)))
       (:body
  	(:div :class "container-fluid"
  	      (:div :class "row"
  		    (:div :class "col-sm-8"
  			  (:div :id "mynetwork"))
  		    (:div :class "col-sm-4"
  			  (build-menu-html))))
  	(:script :type "text/javascript" (str index-script))
  	)))
    (get-output-stream-string stream)
    )
  )

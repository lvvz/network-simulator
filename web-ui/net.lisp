(uiop:define-package :network-simulator/web-ui/net
    (:nicknames :ns-ui-net)
  (:use :common-lisp :cl-who :alexandria :yason)
  (:use :network-simulator/network
	:network-simulator/init
	:network-simulator/utils)
  (:export #:initialize-network
	   #:get-nodes
	   #:get-edges
	   #:generate-network

	   #:prepare-edges
	   #:prepare-nodes))

(in-package :ns-ui-net)


(defparameter +network-generate-form+
  '(("satellites" "Кількість супутникових каналів" "number" 2)
    ("nodes" "Кількість комунікаційних вузлів" "number" 36)
    ("density" "Глибина мережі" "number" 3)))

(defparameter +ngi-name-accessor+ #'first)
(defparameter +ngi-type-accessor+ #'third)

(defun prepare-nodes ()
  (mapcar (lambda (id)
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
	  (hash-table-keys (network-nodes *network*))))

(defun get-nodes (request)
  (declare (ignore request))
  (with-output-to-string* (:indent t)
    (encode (prepare-nodes)
	    yason::*json-output*)))

(defun prepare-edges ()
  (mapcar (lambda (id)
	    (let ((channel (gethash id *channels*)))
	      (alist-hash-table
	       (list (cons "from" (channel-from channel))
		     (cons "to" (channel-to channel))
		     (cons "label" (format nil "~A" (channel-weight channel)))
		     (if (channel-duplex-p channel)
			 (cons "width" 4)
			 (cons "dashes" t))))))
	  (hash-table-keys *channels*)))

(defun get-edges (request)
  (declare (ignore request))
  (with-output-to-string* (:indent t)
    (encode (prepare-edges)
	    yason::*json-output*)))

(defun jquery-id (label)
  (format nil "#~A" label))

(defun create-from-element (name label type &optional default-value)
  (let ((id (symbol-name (gensym "in"))))
    `(:div :class "form-group"
	   (:label :for ,id ,label)
	   (:input :id ,id :class "from-control" :type ,type :name ,name
		   ,@(when default-value
		       `(:value ,default-value))))))

(defun create-submit-button (label &optional on-click)
  `(:button :type "submit" :class "btn btn-primary"
	    ,@(when on-click
		`(:onclick ,on-click))
	    ,label))

(defun create-net-auto-gen-form (url)
  (let* ((id (symbol-name (gensym "form")))
	 (id-expr (jquery-id id)))
    `(:div :class "container"
	   (:form :id ,id :action ,url  :method "POST"
		  ,@(mapcar (curry #'apply #'create-from-element)
			    +network-generate-form+)
		  ,(create-submit-button
		    "Згенерувати"
		    ;; (format nil "
;; console.log($(~S).serialize());
;; /*
;; $.ajax({
;;         url: ~S,
;;         type: ~S,
;;         data: $(~S).serialize(),
;;         success: function(response) {
;;         	result = $.parseJSON(response);
;;         	drawNetwork (result.nodes, result.edges);
;;     	}
;; });*/
;; " id url "POST" id)
		    ))
	   ;; (:script :type "text/javascript" ,(format nil "
;; " id))
	   )))

(defun create-net-paths-form ()
  `())

(defun create-net-packet-testing-form ()
  `())

(defun create-card (card-root card title body-builder)
  (let ((body (funcall body-builder))
	(card-id-expr (jquery-id card))
	(card-root-id-expr (jquery-id card-root)))
    `(:div :class "card"
	   (:div :class "card-header"
		 (:a :class "collapsed card-link"
		     :href ,card-id-expr
		     :data-toggle "collapse"
		     (str ,title)))
	   (:div :id ,card
		 :class "collapse show"
		 :data-parent ,card-root-id-expr
		 (:div :class "card-body" ,body)))))

(defun create-side-menu ()
  (let ((cards `(("c1" "Автоматична генерація"
		       ,(curry #'create-net-auto-gen-form "/gen-net"))
		 ("c2" "Тестування"
		       ,(curry #' create-net-auto-gen-form "/gen-net"))))
	(card-root "card-root"))
    `(:div :id ,card-root
	   ,@(mapcar (curry #'apply #'create-card card-root)
		     cards))))

(defun create-index-page (index-script index-css)
  `(:html
    (:head
     (:title "Мережа передачі даних")
     (:link :rel "stylesheet" :href "https://maxcdn.bootstrapcdn.com/bootstrap/3.4.1/css/bootstrap.min.css")
     (:script :type "text/javascript" :src "https://unpkg.com/vis-network/standalone/umd/vis-network.min.js")
     (loop :for script :in '("https://ajax.googleapis.com/ajax/libs/jquery/3.4.1/jquery.min.js"
			     "https://cdnjs.cloudflare.com/ajax/libs/popper.js/1.16.0/umd/popper.min.js"
			     "https://maxcdn.bootstrapcdn.com/bootstrap/4.4.1/js/bootstrap.min.js")
	   :do (htm (:script :src script)))
     (:link :rel "stylesheet" :href "https://maxcdn.bootstrapcdn.com/bootstrap/4.4.1/css/bootstrap.min.css")
     (:style :type "text/css" (str ,index-css)))
    (:body
     (:div :class "container-fluid"
	   (:div :class "row"
		 (:div :class "col-sm-8"
		       (:div :id "mynetwork"))
		 (:div :class "col-sm-4"
		       ,(create-side-menu))))
     (:script :type "text/javascript" (str ,index-script)))))

(defun initialize-network (request)
  (declare (ignore request))
  ;; (read-file-into-string "~/.quicklisp/local-projects/network-simulator/web-ui/index.html")
  (let ((stream (make-string-output-stream)))
    (eval `(with-html-output (_ ,stream :indent t :prologue t)
	     ,(create-index-page
	       (read-file-into-string "~/.quicklisp/local-projects/network-simulator/web-ui/index.js")
	       (read-file-into-string "~/.quicklisp/local-projects/network-simulator/web-ui/index.css"))))
    (get-output-stream-string stream)))

(defparameter +input-parsers+
  (plist-hash-table
   `("number" ,(rcurry #'parse-integer :junk-allowed t))
   :test #'equal))

(defun generate-network (request)
  (let ((stream (make-string-output-stream)))
    (apply #'init-network (mapcar (lambda (input-info)
				    (funcall (gethash (funcall +ngi-type-accessor+ input-info) +input-parsers+)
					     (tbnl:post-parameter (funcall +ngi-name-accessor+ input-info)
								  request)))
				  +network-generate-form+))
    (encode (alist-hash-table `(("nodes" . ,(prepare-nodes))
    				("edges" . ,(prepare-edges))))
	    stream)
    (get-output-stream-string stream)))

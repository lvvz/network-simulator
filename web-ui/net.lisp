(uiop:define-package :network-simulator/web-ui/net
    (:nicknames :ns-ui-net)
  (:use :common-lisp :cl-who :alexandria :yason)
  (:use :network-simulator/network
	:network-simulator/node
	:network-simulator/init
	:network-simulator/utils
	:network-simulator/routing)
  (:export #:initialize-network
	   #:get-nodes
	   #:get-edges
	   #:generate-network

	   #:prepare-edges
	   #:prepare-nodes

	   #:rt-cols
	   #:rt-data))

(in-package :ns-ui-net)


(defgeneric get-table-columns (type)
  (:method (tp)))

(defgeneric get-table-data (obj)
  (:method (obj)))


(defmacro define-tabulable-struct (name () &rest slots)
  (let ((slot-names (mapcar #'first slots))
	(slot-titles (mapcar #'second slots)))
    `(progn
       (defstruct ,name
	 ,@slot-names)
       (defmethod get-table-columns ((tp (eql ',name)))
	 (encode (list ,@(mapcar (lambda (name title)
				   (plist-hash-table `("title" ,title "field" ,(format nil "~A" name))))
				 slot-names
				 slot-titles))
		 yason::*json-output*))
       (defmethod get-table-data ((obj ,name))
	 (alist-hash-table
	  (mapcar (lambda (slot)
		    (cons (format nil "~A" (sb-mop:slot-definition-name slot))
			  (format nil "~A" (slot-value obj (sb-mop:slot-definition-name slot)))))
		  (sb-mop:class-direct-slots (class-of obj)))))
       (defmethod encode ((obj ,name) &optional (stream *standard-output*))
	 (encode (get-table-data obj) stream))
       )))


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
    `(:div :class "container-fluid"
	   (:form :id ,id :action ,url  :method "POST"
		  ,@(mapcar (curry #'apply #'create-from-element)
			    +network-generate-form+)
		  ,(create-submit-button
		    "Згенерувати"))
	   (:script :type "text/javascript" ,(format nil "customSubmit(~S, drawNetworkFromJSON);" id-expr)))))

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
		 :class "collapse"
		 :data-parent ,card-root-id-expr
		 (:div :class "card-body" ,body)))))

(defun create-table-placeholder (id)
  `(:div :class "container-fluid" :id ,id
	 ))

(defun create-side-menu ()
  (let ((cards `(("c1" "Автоматична генерація"
		       ,(curry #'create-net-auto-gen-form "/gen-net"))
		 ("c2" "Тестування"
		       ,(curry #'create-net-auto-gen-form "/gen-net"))
		 ("c-1" "Таблиця маршрутизації"
			,(curry #'create-table-placeholder "route-table"))))
	(card-root "card-root"))
    `(:div :id ,card-root
	   ,@(mapcar (curry #'apply #'create-card card-root)
		     cards))))

(defun include-css (url)
  `(:link :rel "stylesheet" :href ,url))

(defun inline-css (text)
  `(:style :type "text/css" (str ,text)))

(defun include-js (url)
  `(:script :type "text/javascript" :src ,url))

(defun inline-js (text)
  `(:script :type "text/javascript" (str ,text)))

(defun create-index-page (index-script index-css)
  `(:html
    (:head
     (:title "Мережа передачі даних")
     ,@(mapcar #'include-css
	       '("https://maxcdn.bootstrapcdn.com/bootstrap/3.4.1/css/bootstrap.min.css"
		 "https://unpkg.com/tabulator-tables@4.5.2/dist/css/tabulator.min.css"
		 "https://maxcdn.bootstrapcdn.com/bootstrap/4.4.1/css/bootstrap.min.css"))
     ,@(mapcar #'include-js
	       '("https://ajax.googleapis.com/ajax/libs/jquery/3.4.1/jquery.min.js"
		 "https://cdnjs.cloudflare.com/ajax/libs/popper.js/1.16.0/umd/popper.min.js"
		 "https://maxcdn.bootstrapcdn.com/bootstrap/4.4.1/js/bootstrap.min.js"
		 "https://unpkg.com/vis-network/standalone/umd/vis-network.min.js"
		 "https://unpkg.com/tabulator-tables@4.5.2/dist/js/tabulator.min.js"))
     ,(inline-js index-script)
     ,(inline-css index-css))
    (:body
     (:div :class "container-fluid"
	   (:div :class "row"
		 (:div :class "col-sm-8"
		       (:div :id "mynetwork"))
		 (:div :class "col-sm-4"
		       ,(create-side-menu)))))))

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


(define-tabulable-struct foo ()
  (a "A")
  (b "B"))

(define-tabulable-struct routing-entry ()
  (network "Мережа")
  (mask "Маска")
  (interface "Інт.")
  (next-router-interface "Інт. наст.")
  (next-router "Наст.")
  (metric "Метрика"))

(defun rt-cols (request)
  (declare (ignore request))
  (with-output-to-string* (:indent t)
    (get-table-columns 'routing-entry)))

(defun before-last (lst)
  (if (rest (rest lst))
      (before-last (rest lst))
      lst))

(defun prepare-rt-table-for-node (id)
  (mapcan (lambda (id-rt)
	    (mapcar (lambda (path)
		      (let* (;; (interface (gethash to-id (node-interfaces (get-node id))))
			     (next-router (first (before-last path))))
			;; (break "~A ~A ~A" next-router path (node-interfaces (get-node id)))
			
			(make-routing-entry
			 :network (channel-address (gethash (second path)
							    (node-channels (get-node (first path)))))
			 :mask "255.255.255.0"
			 :interface (format nil "Int~A"
					    (ip-address-last
					     (interface-address
					      (gethash next-router
						       (node-interfaces (get-node id))))))
			 :next-router-interface (format nil "Rt~A" (first path))
			 :next-router (interface-address
				       (gethash next-router
						(node-interfaces (get-node id))))
			 :metric (1- (1- (list-length path))))))
		    id-rt))
	  ;; (hash-table-keys (gethash id *routes-table*))
	  (hash-table-values (gethash id *routes-table*))))

(defun rt-data (request)
  (let ((node-id (parse-integer (tbnl:post-parameter "node-id" request))))
    ;; (break "~A" (gethash node-id *routes-table*))
    (with-output-to-string* (:indent t)
      (encode (prepare-rt-table-for-node node-id)
	      yason::*json-output*))))

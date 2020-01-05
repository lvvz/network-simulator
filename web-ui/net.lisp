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
	   #:rt-data

	   #:rt-shortest-paths

	   #:send-message))

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
		   (cons "group" "defaultNode")
		   ;; (cons "color"
		   ;; 	 (if (eq id (network-satellite-node-id *network*))
		   ;; 	     (alist-hash-table
		   ;; 	      (list (cons "border" "#367377")
		   ;; 		    (cons "background" "#c98c88")))
		   ;; 	     (alist-hash-table
		   ;; 	      (list (cons "border" "#367377")
		   ;; 		    (cons "background" "#439095")))))
		   )))
	  (hash-table-keys (network-nodes *network*))))

(defun get-nodes (request)
  (declare (ignore request))
  (with-output-to-string* (:indent t)
    (encode (prepare-nodes)
	    yason::*json-output*)))

(defun weight-to-width (weight)
  (floor (* 20 (/ 1 weight))))

(defun prepare-edges ()
  (mapcar (lambda (id)
	    (let ((channel (gethash id *channels*)))
	      (alist-hash-table
	       (list* (cons "id" (channel-id channel))
		      (cons "from" (channel-from channel))
		      (cons "to" (channel-to channel))
		      (cons "label" (format nil "[~A]~%~A" (channel-weight channel) (channel-address channel)))
		      ;; (cons "labelFrom" "booo")
		      ;; (cons "labelTo" "booo")
		      (if (channel-duplex-p channel)
			  (list (cons "arrows" "to, from")
				(cons "width" (weight-to-width (channel-weight channel))))
			  (list ;; (cons "dashes" t)
			   (cons "color" (plist-hash-table '("opacity" ".5")))
				(cons "width" (weight-to-width (channel-weight channel)))))))))
	  (hash-table-keys *channels*)))

(defun get-edges (request)
  (declare (ignore request))
  (with-output-to-string* (:indent t)
    (encode (prepare-edges)
	    yason::*json-output*)))

(defun jquery-id (label)
  (format nil "#~A" label))

(defparameter +input-builder-helper+
  (plist-hash-table
   (list "number" (lambda (type name id &optional default-value)
		    `(:input :id ,id :class "from-control" :type ,type :name ,name
			     ,@(when default-value
				 `(:value ,default-value))))
	 "select" (lambda (type name id options)
		    (declare (ignore type))
		    `(:select :id ,id :class "from-control" :name ,name
			      ,@(mapcar (lambda (option)
					  `(:option :value ,(first option)
						    ,(second option)))
					options))))
   :test #'equal))

(defun create-from-element (name label type &optional default-value)
  (let ((id (symbol-name (gensym "in"))))
    `(:div :class "form-group"
	   (:label :for ,id ,label)
	   ,(funcall (gethash type +input-builder-helper+)
		     type name id default-value))))

(defun create-button (label &key id type on-click)
  `(:button
    :class "btn btn-primary"
    ,@(when id
	`(:id ,id))
    ,@(when type
	`(:type ,type))
    ,@(when on-click
	`(:onclick ,on-click))
    ,label))

(defun create-form (url info
		    &key
		      button-label
		      on-click
		      (id (symbol-name (gensym "form"))))
  (let* ((id-expr (jquery-id id)))
    `(:div :class "container-fluid"
	   (:form :id ,id :action ,url  :method "POST"
		  ,@(mapcar (curry #'apply #'create-from-element)
			    info)
		  ,(when button-label
		     (create-button button-label :type "submit")))
	   ,(when on-click
	      `(:script :type "text/javascript" ,(funcall on-click id-expr))))))

(defun create-net-auto-gen-form (url)
  (create-form url +network-generate-form+
	       :button-label "Згенерувати"
	       :on-click (curry #'format nil "customSubmit(~S, drawNetworkFromJSON);")))

(defparameter +network-send-message+
  '(("from" "" "number" 0)
    ("to" "" "number" 0)
    ("mode" "Виберіть режим передачі" "select"
     (("datagram" "Дейтаграмний")
      ("logical-conn" "З логічним з'єднанням")
      ("virtual-chan" "Зі встановленням віртуального каналу")))
    ("message-size" "Розмір повідомлення, Б" "number" 4096)
    ;; ("segment-size" "Розмір сегмента, Б" "number" 1024)
    ("packet-size" "Розмір пакета, Б" "number" 512)))

(defun create-send-message-form (url)
  (create-form url +network-send-message+ 
	       :id "send-message"))

(defun create-net-paths-form ()
  `())

(defun create-net-packet-testing-form (url)
  `(:div :class "container-fluid"
	 (:div :class "row"
	       ,(create-send-message-form url))
	 (:div :class "row"
	       ,(create-button "Next node" :id "next-node"))))

(defun create-card (card-root card title body-builder)
  (let ((body (funcall body-builder))
	(card-id-expr (jquery-id card))
	(card-root-id-expr (jquery-id card-root)))
    (declare (ignore card-root-id-expr))
    `(:div :class "card"
	   (:div :class "card-header"
		 (:a :class "collapsed card-link"
		     :href ,card-id-expr
		     :data-toggle "collapse"
		     (str ,title)))
	   (:div :id ,card
		 :class "collapse"
		 ;; :data-parent ,card-root-id-expr
		 (:div :class "card-body" ,body)))))

(defun create-table-placeholder (id)
  `(:div :class "container-fluid"
	 (:div :class "row"
	       (:div :class "container-fluid" :id ,id))))

(defun create-side-menu ()
  (let ((cards `(("c1" "Автоматична генерація"
		       ,(curry #'create-net-auto-gen-form "/gen-net"))
		 ("c2" "Тестування"
		       ,(curry #'create-net-packet-testing-form "/send-message"))
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
   (list "number" (rcurry #'parse-integer :junk-allowed t)
	 "select" (compose #'make-keyword #'string-upcase))
   :test #'equal))

(defun get-post-request-parameters (info request)
  (mapcar (lambda (input-info)
	    (funcall (gethash (funcall +ngi-type-accessor+ input-info) +input-parsers+)
		     (tbnl:post-parameter (funcall +ngi-name-accessor+ input-info)
					  request)))
	  info))

(defun generate-network (request)
  (let ((stream (make-string-output-stream)))
    (apply #'init-network (get-post-request-parameters +network-generate-form+ request))
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

(defparameter +path-input+
  '(("from" "" "number")
    ("to" "" "number")))

(defun rt-shortest-paths (request)
  (destructuring-bind (from to) (get-post-request-parameters +path-input+ request)
    (let ((edges (list))
	  (nodes (make-node-map)))
      (mapc (lambda (x)
	      (maplist (lambda (nodes)
			 (when (rest nodes)
			   (push (channel-id (gethash (second nodes) (node-channels (get-node (first nodes)))))
				 edges)))
		       x))
	    (gethash to (gethash from *routes-table*)))
      (mapc (lambda (x)
	      (setf (gethash x nodes) t))
	    (apply #'append (gethash to (gethash from *routes-table*))))
      (with-output-to-string* ()
	(encode (plist-hash-table
		 (list "nodes" (hash-table-keys nodes)
		       "edges" edges))
	 yason::*json-output*)))))

(defstruct send-simulation-info
  from to message-size packet-size)

(defun create-send (from to message-size packet-size)
  (make-send-simulation-info :from from :to to :message-size message-size
			     :packet-size packet-size))

(defun simulate-packet-send-through-path (start-time path)
  (let ((visited-nodes (make-node-map)))
    (setf (gethash (first path) visited-nodes)
	  start-time)
    ))

(defgeneric send (mode e)
  (:method ((mode (eql :datagram)) info)
    (with-slots (from to message-size packet-size) info
      (let ((paths (blet (gethash from (gethash to *routes-table*)))))
	(simulate-packet-send-through-path 0 paths)))))

(defun simulate-send (from to mode message-size packet-size)
  (send mode (create-send from to message-size packet-size)))

(defun blet (it)
  (break "~S" it)
  it)

(defun send-message (request)  
  (encode (blet (apply #'simulate-send (get-post-request-parameters +network-send-message+ request)))))

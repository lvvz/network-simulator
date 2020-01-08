(uiop:define-package :network-simulator/web-ui/net
    (:nicknames :ns-ui-net)
  (:use :common-lisp :cl-who :alexandria :anaphora :yason)
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


(defmacro blet (it &optional format)
  `(let ((it ,it))
     (break ,(or format "~S") it)
     it))

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
  '(("from" "Відправник" "number" 0)
    ("to" "Отримувач" "number" 0)
    ;; ("mode" "Виберіть режим передачі" "select"
    ;;  (("dataram" "Дейтаграмний")
    ;;   ("logical-conn" "З логічним з'єднанням")
    ;;   ("virtual-chan" "Зі встановленням віртуального каналу")))
    ("max-message-size" "Максимальний розмір повідомлення, Б" "number" 4096)
    ("min-message-size" "Мінімальний розмір повідомлення, Б" "number" 64)
    ("message-measurements" "Кількість вимірів" "number" 10)
    ("mtu" "MTU, Б" "number" 1500)
    ("max-packet-count" "Максимальна кількість пакетів" "number" 32)
    ("min-packet-count" "Мінімальна кількість пакетів" "number" 1)
    ("packet-measurements" "Кількість вимірів" "number" 10)))

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
	       (:div :class "container-fluid"
		     ,(create-button "Натисніть, щоб вибрати отримувача" :id "next-node")))))

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
		 "https://visjs.github.io/vis-timeline/dist/vis-timeline-graph2d.min.css"
		 "https://maxcdn.bootstrapcdn.com/bootstrap/4.4.1/css/bootstrap.min.css"))
     ,@(mapcar #'include-js
	       '("https://ajax.googleapis.com/ajax/libs/jquery/3.4.1/jquery.min.js"
		 "https://cdnjs.cloudflare.com/ajax/libs/popper.js/1.16.0/umd/popper.min.js"
		 "https://maxcdn.bootstrapcdn.com/bootstrap/4.4.1/js/bootstrap.min.js"
		 ;; "https://visjs.github.io/vis-timeline/dist/vis.js"
		 ;; "https://unpkg.com/vis-timeline@6.3.5/dist/vis-timeline-graph2d.min.js"
		 "https://cdn.jsdelivr.net/npm/chart.js@2.9.3/dist/Chart.min.js"
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
		       ,(create-side-menu)))
	   (:div :class "row" :id "visualization")))))

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
	  (nodes (make-node-map))
	  (paths (gethash to (gethash from *routes-table*))))
      (mapc (lambda (x)
	      (maplist (lambda (nodes)
			 (when (rest nodes)
			   (push (channel-id (gethash (second nodes) (node-channels (get-node (first nodes)))))
				 edges)))
		       x))
	    paths)
      (mapc (lambda (x) (setf (gethash x nodes) t))
	    (apply #'append paths))
      (with-output-to-string* ()
	(encode (plist-hash-table
		 (list "nodes" (hash-table-keys nodes)
		       "edges" edges
		       "paths" paths))
	 yason::*json-output*)))))

(defparameter +acknowledge-packet-size+ 40)
(defparameter +syncronization-packet-size+ 40)

(defstruct send-simulation-info
  from to message-size packet-size packets-count channel-release-time
  data-packets service-packets
  message-send-time)

(defun create-send (from to message-size packet-size packets-count data-packets service-packets)
  (make-send-simulation-info :from from :to to :message-size message-size
			     :packet-size packet-size
			     :packets-count packets-count
			     :data-packets data-packets
			     :service-packets service-packets
			     :channel-release-time (make-node-map)))

(defun funcall-reversed (fn &rest args)
  (apply fn (reverse args)))

(defun reversed-lambda (fn)
  (lambda (&rest args) (apply #'funcall-reversed fn args)))

(defun -> (arg &rest fn-list)
  (reduce (reversed-lambda #'funcall) fn-list :initial-value arg :from-end nil))

(defun rfuncall (fn-list arg)
  (reduce #'funcall fn-list :initial-value arg :from-end t))

;; (defstruct busy-range
;;   begin end)

(defstruct simple-channel
  release-time)

(defstruct duplex-channel
  forward-release-time backward-release-time)

(defparameter *current-time* nil)

(defun add-weight-to-time (time channel packet-size)
  (+ (aif time
	  (max it *current-time*)
	  *current-time*)
     (* packet-size (channel-weight channel))))

(defparameter *send-direction* :forward)

(defgeneric release-time (rt)
  (:method ((rt simple-channel))
    (simple-channel-release-time rt))
  (:method ((rt duplex-channel))
    (case *send-direction*
      (:forward (duplex-channel-forward-release-time rt))
      (:backward (duplex-channel-backward-release-time rt)))))

(defgeneric update-release-time (channel-rt nrt)
  (:method ((channel-rt simple-channel) nrt)
    (setf (simple-channel-release-time channel-rt) nrt))
  (:method ((channel-rt duplex-channel) nrt)
    (case *send-direction*
      (:forward (setf (duplex-channel-forward-release-time channel-rt) nrt))
      (:backward (setf (duplex-channel-backward-release-time channel-rt) nrt)))))

(defgeneric new-release-time (channel-rt channel packet-size)
  (:method ((channel-rt simple-channel) channel packet-size)
    (add-weight-to-time (simple-channel-release-time channel-rt)
			channel
			packet-size))
  (:method ((channel-rt duplex-channel) channel packet-size)
    (add-weight-to-time (case *send-direction*
			  (:forward (duplex-channel-forward-release-time channel-rt))
			  (:backward (duplex-channel-backward-release-time channel-rt)))
			channel
			packet-size)))

(defun update-channel-release-time (channel channel-release-time packet-size)
  (unless (gethash (channel-id channel)
		   channel-release-time)
    (setf (gethash (channel-id channel)
		   channel-release-time)
	  (if (channel-duplex-p channel)
	      (make-duplex-channel)
	      (make-simple-channel))))
  (update-release-time (gethash (channel-id channel)
				channel-release-time)
		       (new-release-time (gethash (channel-id channel)
						  channel-release-time)
					 channel
					 packet-size)))

(defparameter *send-time* nil)

(defun simulate-packet-send-through-path-r (path channel-release-time packet-size)
  ;; (blet *current-time*)
  (aif (rest path)
       (let* ((channel (gethash (second path)
				(-> path #'first #'get-node #'node-channels)))
	      (*current-time* ;; (blet (update-channel-release-time channel channel-release-time)
		;; 	    "new: ~S")
		(update-channel-release-time channel channel-release-time packet-size)))
	 (simulate-packet-send-through-path-r it channel-release-time packet-size))
       (setf *send-time* *current-time*)))

(defun simulate-packet-send-through-path (path channel-release-time packet-size)
  (let ((*current-time*
	  (or (aand ;; (blet 
	       ;; 	  "chrt: ~S")
	       (gethash
		(channel-id
		 (gethash (second path)
			  (-> path #'first #'get-node #'node-channels)))
		channel-release-time)
	       (let ((*send-direction* (case *send-direction*
					 (:forward :backward)
					 (:backward :forward))))
		 (release-time it)))
	      0)))
    (simulate-packet-send-through-path-r path channel-release-time packet-size)))

(defun send-datagram-with-ack (info path channel-release-time packet-size)
  (simulate-packet-send-through-path path channel-release-time packet-size)
  (let ((*send-direction* :backward))
    (simulate-packet-send-through-path (reverse path) channel-release-time +acknowledge-packet-size+))
  (incf (send-simulation-info-data-packets info)
	packet-size)
  (incf (send-simulation-info-service-packets info)
	+acknowledge-packet-size+))

(defun set-send-time (info)
  (with-slots (from to channel-release-time) info
    (let ((paths (gethash from (gethash to *routes-table*))))
      (setf (send-simulation-info-message-send-time info)
	    (apply
	     #'max
	     (mapcar (lambda (path)
		       (or (aand (gethash
				  (channel-id
				   (gethash (second path)
					    (-> path
						#'first #'get-node #'node-channels)))
				  channel-release-time)
				 (release-time it))
			   0))
		     (mapcar #'reverse paths)))))))

(defun send-message-packets-via-paths (info paths)
  (with-slots (message-size packets-count packet-size channel-release-time) info
    (let ((packets-index 0))
      (loop :while (< packets-index packets-count)
	    :do (dolist (path paths)
		  (when (< packets-index packets-count)
		    (send-datagram-with-ack info path channel-release-time packet-size)
		    (incf packets-index)))))))

(defun send-message-packets (info)
  (with-slots (from to) info
    (send-message-packets-via-paths info (gethash from (gethash to *routes-table*)))))

(defun establish-connection (info path)
  (with-slots (channel-release-time) info
    (simulate-packet-send-through-path path channel-release-time
				       +syncronization-packet-size+)
    (simulate-packet-send-through-path (reverse path) channel-release-time
				       +acknowledge-packet-size+)
    (simulate-packet-send-through-path path channel-release-time
				       +acknowledge-packet-size+)
    (incf (send-simulation-info-service-packets info)
	  +syncronization-packet-size+)
    (incf (send-simulation-info-service-packets info)
	  +acknowledge-packet-size+)
    (incf (send-simulation-info-service-packets info)
	  +acknowledge-packet-size+)))

(defun finish-connection (info path)
  (with-slots (channel-release-time) info
    (simulate-packet-send-through-path path channel-release-time
				       +syncronization-packet-size+)
    (simulate-packet-send-through-path (reverse path) channel-release-time
				       +acknowledge-packet-size+)
    (simulate-packet-send-through-path path channel-release-time
				       +syncronization-packet-size+)
    (simulate-packet-send-through-path (reverse path) channel-release-time
				       +syncronization-packet-size+)
    (incf (send-simulation-info-service-packets info)
	  +syncronization-packet-size+)
    (incf (send-simulation-info-service-packets info)
	  +acknowledge-packet-size+)
    (incf (send-simulation-info-service-packets info)
	  +syncronization-packet-size+)
    (incf (send-simulation-info-service-packets info)
	  +syncronization-packet-size+)))

(defgeneric send (mode e)
  (:method ((mode (eql :datagram)) info)
    (send-message-packets info))
  (:method ((mode (eql :logical-conn)) info)
    (with-slots (from to) info
      (let ((path (first (gethash from (gethash to *routes-table*)))))
	(establish-connection info path)
	(send-message-packets info)
	(finish-connection info path)
	)))
  (:method ((mode (eql :virtual-chan)) info)
    (with-slots (from to) info
      (let ((path (first (gethash from (gethash to *routes-table*)))))
	(establish-connection info path)
	(send-message-packets-via-paths info (list path))
	(finish-connection info path)
	)))
  (:method :after (mode info)
    (set-send-time info)))

(defun rapply (composer fn-list &rest args)
  (apply composer (mapcar (rcurry #'apply args) fn-list)))

(defun rrapply (composer fn-list &rest args)
  (apply composer (mapcar (apply #'rcurry #'rapply args) fn-list)))

(defun dorange (start step count fn)
  (dotimes (i (1+ count))
    (funcall fn
	     (floor (+ start (* i step)))
	     i)))

(defun create-x-y-json (id x y)
  (plist-hash-table (list "x" x "y" y "id" id) :test #'equal))

(defun simulate-send (mode from to
		      max-message-size min-message-size message-measurements
		      mtu
		      max-packet-count min-packet-count packet-measurements)
  from to ;; mode
  max-message-size min-message-size message-measurements mtu max-packet-count min-packet-count packet-measurements
  (list
   (let ((report (list))
	 (packets-count min-packet-count)
	 (message-size-step (/ (- max-message-size min-message-size)
			       message-measurements)))
     (dorange min-message-size message-size-step message-measurements
	      (lambda (message-size i)
		(let* ((packet-size (floor message-size packets-count))
		       (info (create-send from to message-size packet-size packets-count 0 0)))
		  (send mode info)
		  (push (create-x-y-json i message-size (send-simulation-info-message-send-time info))
			report))))
     report)
   (let ((report (list))
	 (message-size max-message-size))
     (dorange min-packet-count
	      (/ (- max-packet-count min-packet-count)
		 packet-measurements)
	      packet-measurements
	      (lambda (packets-count i)
		(let* ((packet-size (floor message-size packets-count))
		       (info (create-send from to message-size packet-size packets-count 0 0)))
		  (send mode info)
		  (push (create-x-y-json i packets-count (send-simulation-info-message-send-time info))
			report))))
     report)))

(defmacro ht (&rest plist)
  (plist-hash-table plist))

(defun send-message (request)
  (let ((mode-to-label (plist-hash-table
			(list :datagram '("Дейтаграмний режим" "red")
			      :logical-conn '("Режим зі встановленням зв'язку" "blue")
			      :virtual-chan '("Режим зі встановленням віртуального каналу" "green")))))
    (labels ((%send (mode)
	       (apply #'simulate-send mode
		      (get-post-request-parameters +network-send-message+ request))))
      (with-output-to-string* (:indent t)
	(let* ((keys (hash-table-keys mode-to-label))
	       (report (mapcar #'%send keys))
	       (report ;; (alist-hash-table (mapcar #'cons keys report))
		 (apply #'mapcar #'list report)))
	  (encode (mapcar (lambda (report)
			    (mapcar (lambda (mode report)
				      (plist-hash-table (list "label" (first (gethash mode mode-to-label))
							      "borderColor" (second (gethash mode mode-to-label))
							      "data" ;; (%send mode)
							      report
							      )))
				    keys report))
			  report)
		  yason::*json-output*))))))

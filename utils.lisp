(uiop:define-package :network-simulator/utils
    (:nicknames :ns-utils)
  (:use :common-lisp :alexandria :anaphora)
  (:export #:make-node-map
	   #:make-integer-generator
	   #:make-random-generator-from
	   #:make-random-generator-from-range
	   #:make-random-boolean-generator))

(in-package :network-simulator/utils)


(defun make-node-map (&optional size)
  (if size
      (make-hash-table :test #'eq :size size)
      (make-hash-table :test #'eq)))

(defun hash-table-keys-vector (hash-table)
  (make-array (hash-table-count hash-table)
	      :adjustable t
	      :fill-pointer (hash-table-count hash-table)
	      :initial-contents (hash-table-keys hash-table)))

(defgeneric drop-element-and-order (i sequence)
  (:method (i (seq array))
    (setf (aref seq i)
	  (aref seq (1- (length seq))))))

(defun make-integer-generator ()
  (let ((i 0))
    (lambda () (prog1 i
		 (incf i)))))

(defun make-random-generator-from-range (a b)
  (lambda () (+ a (random (1+ (- b a))))))

(defun make-random-boolean-generator ()
  (lambda () (eq 1 (random 2))))

(defgeneric make-random-generator-from (values)
  (:method ((values array))
    (let ((values-count (length values)))
      (lambda () (aref values (random values-count))))))

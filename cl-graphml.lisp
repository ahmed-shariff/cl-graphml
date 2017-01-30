
(in-package :cl-user)
(defpackage :cl-graphml
  (:use
   :cl
   :plump
   :alexandria)
  (:import-from
   :plump
   :get-elements-by-tag-name
   :attribute
   :text
   :first-child
   :next-element
   :first-element
   :child-elements)
  (:export
   :get-elements-by-attributes-and-keys
   :get-file-as-string))

					;(ql:quickload :plump)
					;(children (plump:parse (get-file-as-string "d:/WorkFiles/ifs_component_dependency_visualizer/Sample/sample.graphml")))

(in-package :cl-graphml)

(defun get-file-as-string (path)
  "returns the content of a file as a string"
  (with-output-to-string (out)
    (with-open-file (file path)
      (labels ((x (stream) 
		 (let ((line (read-line stream nil :eof)))
		   (unless (eq line :eof)
		     (format out (remove #\Return line))
		     (x stream)))))
	(x file)))
    out))

;;sample call
;; (GET-EDGES-BY-ATTRIBUTES-AND-KEYS
;;  (PLUMP-PARSER:PARSE
;;   (GET-file-AS-STRING
;;    "sample.graphml"))
;;  :attributes-list (("source" "trninv" "trnper") ("target" "trngeo"))
;;  :key-list (("type" "abc" ("entity_level" t))))

(defmacro get-elements-by-attributes-and-keys (node &key attributes-list key-list (node-tag "edge"))
  "returns plump-dom elements from a graphml file. 
The first parameter is node from which node are to be extracted.
the attributes-list is used to provide the list of attributes. The first element of each element
of the list must be the name of the attribute to check for. The rest are the values that can be
used to filter the returned elements based on the perticular attribute. Each value can be either
a string representing the value, or a list, where the first element is the string representing 
the value, and the second is a generalized boolean, where if not true, any plump-dom element that
has the perticular value for the perticular attribute will be filtered out. Otherwise any plump-dom
that has any value as specified for the attribute will be returned. The first element of the attributes
list can be a non-list generalized boolean. If it is a non-list value, when true, it will return
a list of plump-dom elements that have atleast one attibute specified with a value requested. 
If the first element in the attributes list is not a non-list value or is a non-list value and is
false, returned plump-dom elements will have a values as provided for all attributes.
Similarly the key-list filters based on the keys elements of the node. The list works similarly to 
the list of attributes-list. The node-tag allows to filter based on the tag of the plump-dom elements."
  ;(print (eval (first (first key-list))))
  `(let ((finds '())
	 (edges (get-elements-by-tag-name ,node ,node-tag))
	 ,@(when (and key-list (car key-list))
	     `((functions (make-hash-table :test 'equalp)))))
     ,@(get-key-comp-fn-list key-list)
     (labels ((traverse (edges)
		(let ((edge (car edges)))
		  (when (and edge
			     ,@(when (and attributes-list (car attributes-list))
				 (if (listp (first attributes-list))
				     `((and ,@(attribute-comparison-macro-code attributes-list)))
				     (if (first attributes-list)
					 `((or ,@(attribute-comparison-macro-code (cdr attributes-list))))
					 `((and ,@(attribute-comparison-macro-code (cdr attributes-list)))))))
			     ,@(when (and key-list (car key-list))
				 (if (and (not (listp (first key-list)))
					  (first key-list))
				     (key-comparison-macro-code-ORing)
				     (key-comparison-macro-code-ANDing))))
		    (push edge finds)))
		(when (cdr edges)
		  (traverse (cdr edges)))))
	      (traverse edges))
     finds))


;;hashtable of functions which will be used to compare keys will be returned by this function
(defun get-key-comp-fn-list (key-list)
  (loop for key in key-list
	when (listp key)
	  collect `(setf (gethash ,(first key) functions)
			 ,(if (cddr key)
			      `(lambda (val)
				 (let ((matching-key-value (find val
								 ,(cdr key)
								 :test #'string-equal
								 :key #'(lambda (v)
									  (if (listp v)
									      (first v)
									      v)))))
				   (if matching-key-value
				       (if (listp matching-key-value)
					   (second matching-key-value)
					   t)
				       nil)))
			      `(lambda (val)
				 (string-equal ,(second key)
					       val))))))



;;function used in the macro get-edges-by-attributes-and-keys
(defun key-comparison-macro-code-ORing ()
  `((labels ((data-node-traverse (node)
	       (if node
		   (let* ((key (attribute node "key"))
			  (matching-key-fn (gethash key functions)))
		     (if (if matching-key-fn
			     (funcall (coerce matching-key-fn 'function)
				      (text
				       (first-child node)))
			     nil)
			 t
			 (data-node-traverse (next-element node))))
		   nil)))
      ;; the 'edge' come from the macro get-elements-by-attributes-and-keys.
      (data-node-traverse (first-element edge)))))


(defun key-comparison-macro-code-ANDing ()
  `((labels ((data-node-traverse (node)
	       (if node
		   (let* ((key (attribute node "key"))
			  (matching-key-fn (gethash key functions)))
		     ;; here the node that don't have the required value fails
		     ;; only when the string it has does not match.
		     ;; i.e. for the node the fail, it must have the attribute
		     ;; with a value not expected.
					;(print matching-key-fn)
		     (if (if matching-key-fn
			     (funcall (coerce matching-key-fn 'function)
				      (text
				       (first-child node)))
			     t)
			 (data-node-traverse (next-element node))
			 nil))
		   t)))
      ;; the 'edge' come from the macro get-elements-by-attributes-and-keys.
      (data-node-traverse (first-element edge)))))

;;function used in the macro get-edges-by-attributes-and-keys
(defun attribute-comparison-macro-code (attributes-list)
  (loop for attributes-tuple in attributes-list
	collect (if (cddr attributes-tuple)
		    `(let ((found-val (find (attribute edge ,(first attributes-tuple))
					    ',(cdr attributes-tuple)
					    :test #'string-equal
					    :key #'(lambda (v)
						     (if (listp v)
							 (first v)
							 v)))))
		       (if found-val 
			   (if (listp found-val)
			       (second found-val)
			       t)
			   nil))
		    `(string-equal ,(second attributes-tuple)
				   (attribute edge ,(first attributes-tuple))))))

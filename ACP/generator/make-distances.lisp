
(defpackage pddl.builder
  (:use :cl
	:guicho-utilities
        :cl-ppcre
        :optima
        :optima.extra
	:iterate
	:alexandria
	:guicho-a*))

(in-package :pddl.builder)

(defun make-bases (basenum)
  (iter (for i below basenum)
        (collect
            (concatenate-symbols 'b i))))

(defun print-list (lst)
  (dolist (r lst lst)
    (print r)))
(defun make-reachable (arms positions)
  (map-product (lambda (arm position)
		 `(reachable ,arm ,position))
	       (ensure-list arms)
	       (ensure-list positions)))

(defun make-dists (position-tree)
  (multiple-value-bind (a index-hash)
      (%get-costs
       (%get-adjacency position-tree)
       (remove-duplicates (flatten position-tree)))
    (%build-dists a index-hash)))

(defclass symbol-node (searchable-bidirectional-node)
  ((sym :type symbol :initarg :sym :accessor sym)
   (complementary-edge-class :initform 'symbol-edge)))

(defmethod print-object ((n symbol-node) s)
  (print-unreadable-object (n s :type t)
    (format s "~w ~w" :sym (sym n))))

(defclass symbol-edge (searchable-edge)
  ((complementary-node-class :initform 'symbol-node)))

(defmethod generic-eq ((s1 symbol-node) (s2 symbol-node))
  (eq (sym s1) (sym s2)))

(defmethod heuristic-cost-between ((s1 symbol-node) (s2 symbol-node))
  0 ;; dijkstra search
  )

(defmethod cost ((e symbol-edge))
  1)

(defmethod generate-nodes ((s symbol-node))
  nil)

(defun %get-costs (adjacencies positions)
  (let ((a (make-array (list (length positions)
			     (length positions))
		       :element-type 'fixnum))
	(index-hash (make-hash-table)))
    (iter (for p in positions)
	  (for i from 0)
	  (setf (gethash p index-hash) i))
    
    (let ((node-hash (make-hash-table)))
      (dolist (p positions)
	(setf (gethash p node-hash)
	      (make-instance 'symbol-node :sym p)))
      (dolist (pair adjacencies)
	(destructuring-bind (p q) pair
	  (connect (gethash p node-hash)
		   (gethash q node-hash))))
      (dolist (p1 positions)
	(dolist (p2 positions)
	  (setf (aref a
		      (gethash p1 index-hash)
		      (gethash p2 index-hash))
		(iter (for node
			   initially
			   (a*-search (gethash p1 node-hash)
				      (gethash p2 node-hash))
			   then (parent node))
		      (until (eq (sym node) p1))
		      ;; (format t "~%going backward... ~w" node)
		      (counting (cost node)))))))
    (values a
	    index-hash
	    (sort (hash-table-alist index-hash) #'< :key #'cdr))))

(defun %build-dists (a index-hash)
  (iter
    outer
    (for (p i) in-hashtable index-hash)
    (iter (for (q j) in-hashtable index-hash)
	  (in outer
	      (collect
		  `(= (move-cost ,p ,q)
		      ,(if (= i j)
			   1000
			   (1+ (aref a i j)))))))))


(defun make-adjacency (a b randomness)
  (let ((dist (if (plusp randomness)
		  (1+ (random randomness)) 1)))
    (list 
	  `(= (move-cost ,b ,a) ,dist)
	  `(adjacent ,a ,b)
	  `(adjacent ,b ,a))))

(defun make-non-adjacency (a b)
  (list `(= (move-cost ,a ,b) 1000)))

(defun %get-adjacency (position-tree)
  (let ((acc nil))
    (walk-tree
     (let ((prev nil))
       (lambda (branch cont)
	 (if prev
	     (typecase branch
	       (list
		(push (list (first prev) (car branch)) acc)
		(push (car branch) prev)
		(funcall cont (cdr branch))
		(pop prev))
	       (symbol
		(push (list (first prev) branch) acc)
		(setf (car prev) branch)))
	     ;; first layer
	     (typecase branch
	       (list
		(prog2
		    (push (car branch) prev)
		    (funcall cont (cdr branch))
		  (pop prev)))
	       (symbol (error "illegal argument"))))))
     position-tree)
    acc))

(defun make-linear-jobs (jobspecs &optional
			 (lower-limit 1)
			 (upper-limit 3))
  (let ((acc '((= (job-cost nothing-done) 0))))
    (let ((prev 'nothing-done))
      (dolist (job jobspecs acc)
	 (destructuring-bind
	       (name place &optional component/s tray)
	     job
	   (push `(job-available-at ,name ,place) acc)
	   (push `(depends ,prev ,name) acc)
	   (push `(= (job-cost ,name)
		     ,(+ lower-limit
			 (let ((d (- upper-limit lower-limit)))
			   (if (plusp d) (random d) 0)))) acc)
	   (when component/s
	     (unless tray
	       (error "which is the tray for ~a ?" component/s))
             (iter (for component in (ensure-list component/s))
                   (push `(uses ,name ,component) acc)
                   (push `(at ,component ,tray) acc)))
	   (setf prev name))))))

(defun make-initial-arms (arms positions)
  (mappend
   (lambda (arm pos)
     `((free ,arm) (at ,arm ,pos) (arm-present ,pos)))
   (ensure-list arms)
   (ensure-list positions)))

(defun make-initial-bases (base-names)
  (let ((acc nil))
    (dolist (base (ensure-list base-names) (nreverse acc))
      (push `(at ,base carry-in) acc)
      (push `(finished nothing-done ,base) acc))))

(defun make-goal-bases (base-names last-job)
  (let ((acc nil))
    (dolist (base (ensure-list base-names) (nreverse acc))
      (push `(at ,base carry-out) acc)
      (push `(finished ,last-job ,base) acc))))


(in-package :pddl.builder)

(defun write-models-many (fn
                          &key
                          (format-control "p~4,,,'0@a.pddl")
                          (size-list '(1 2 4 16 64 256 1024)))
  (let ((snapshot (make-random-state)))
    (dolist (i size-list)
      (let ((*random-state* (make-random-state snapshot)))
        (write-model fn
                     #'(lambda (i)
                         (format nil format-control i))
                     i)))))

(defun write-model (modelfn pathnamefn basenum)
  (with-open-file (s (funcall pathnamefn basenum)
		     :direction :output
		     :if-exists :supersede
		     :if-does-not-exist :create)
    (let ((*package* (find-package :pddl.builder)))
      (write (funcall modelfn basenum) :stream s))))

(defun model2a (basenum)
  (let ((bases (iter (for i below basenum)
		     (collect
			 (concatenate-symbols 'b i)))))
    `(define (problem ,(concatenate-symbols
			'cell-assembly-model2a
			basenum))
       (:domain cell-assembly)
       (:objects arm1
		 arm2 - arm
		 ,@bases - base
		 part-a
		 part-b
		 part-c - component
		 
		 tray-a
		 tray-b
		 tray-c - tray
		 
		 table1
		 table2 - table
		 
		 gasket-machine
		 screw-machine-a
		 oiling-machine
		 screw-machine-c
		 inspection-machine  - machine
		 
		 insert-gasket  - machine-job
		 attatch-a      - job ;; at table1
		 screw-a        - machine-job
		 oil-cylinder   - machine-job
		 attatch-b      - job ;; at table2
		 attatch-c      - job ;; at table2
		 screw-c        - machine-job
		 inspect-base   - machine-job
		 )
       (:init
   ;;;;;;;;;;;;;;;; ATTRIBUTES ;;;;;;;;;;;;;;;;
	;; 
	;; cost initialization
	(= (total-cost) 0)   ; !!! do not remove this
	(= (loading-cost) 1) ; !!! do not remove this

	;; arm attributes
	,@(make-reachable
	   'arm1 '(gasket-machine
		   inspection-machine
		   
		   table-in
		   table-out
		   table1
		   table2
		   
		   tray-a))

	,@(make-reachable
	   'arm2 '(screw-machine-a
		   oiling-machine
		   screw-machine-c
		   
		   table1
		   table2
		   
		   tray-b
		   tray-c))

	;; position attributes
	,@(make-dists  '(table-in
			 gasket-machine
			 table1
			 screw-machine-a
			 tray-b
			 oiling-machine
			 tray-c
			 screw-machine-c
			 table2
			 inspection-machine
			 table-out
			 tray-a
			 table-in))
	
	;; conveyor attributes
	(connected carry-in table-in)   ; !!! do not remove this
	(connected table-out carry-out) ; !!! do not remove this

	;; job and component attributes
	
	,@(make-linear-jobs
	   '((insert-gasket gasket-machine)
	     (attatch-a table1 part-a tray-a)
	     (screw-a screw-machine-a)
	     (oil-cylinder oiling-machine)
	     (attatch-b table2 part-b tray-b)
	     (attatch-c table2 part-c tray-c)
	     (screw-c screw-machine-c)
	     (inspect-base inspection-machine)) 2 4)
	
   ;;;;;;;;;;;;;;;; INITIAL STATES ;;;;;;;;;;;;;;;;
	;; 
   ;;;; Bases ;;;;;;;;;
	;; 
	;; All bases are at CARRY-IN
	;; Base and jobs. All bases must have finished NOTHING-DONE

	,@(make-initial-bases bases)

   ;;;; Arms ;;;;;;;;;;;;;;;;
	,@(make-initial-arms '(arm1 arm2)
			     '(tray-a oiling-machine)))
       (:goal (and
	       ,@(make-goal-bases bases 'inspect-base)
	       ))
       (:metric minimize (total-cost)))))



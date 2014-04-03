
(in-package :pddl.builder)

(defun write-model2b (max)
  (iter (for i from 1 to max)
	(write-model #'model2b
		     #'(lambda (i)
			 (format nil "model2b~a.pddl" i))
		     i)))

(defun model2b (basenum)
  (let ((bases (iter (for i below basenum)
		     (collect
		      (concatenate-symbols 'b i)))))
    `(define (problem ,(concatenate-symbols
			'cell-assembly-model2b
			basenum))
	 (:domain cell-assembly)
       (:objects arm - arm
		 ,@bases - base
		 part-a
		 part-b
		 part-c - component
		 
		 tray-a
		 tray-b
		 tray-c - tray
		 
		 table1 - table
		 
		 screw-machine-a
		 screw-machine-c  - machine
		 
		 attatch-a      - job ;; at table-in
		 screw-a        - machine-job
		 attatch-b      - job ;; at table1
		 attatch-c      - job ;; at table1
		 screw-c        - machine-job
		 )
       (:init
   ;;;;;;;;;;;;;;;; ATTRIBUTES ;;;;;;;;;;;;;;;;
	;; 
	;; cost initialization
	(= (total-cost) 0)   ; !!! do not remove this
	(= (loading-cost) 1) ; !!! do not remove this

	;; arm attributes
	,@(make-reachable
	   'arm '(screw-machine-c
		  screw-machine-a
		  table-in
		  table-out
		  table1		   
		  tray-a		   
		  tray-b		   
		  tray-c))

	;; position attributes
	,@(make-dists  '(table-in
			 table1
			 screw-machine-a
			 tray-a
			 tray-b
			 tray-c
			 screw-machine-c
			 table-out
			 table-in))
	
	;; conveyor attributes
	(connected carry-in table-in)   ; !!! do not remove this
	(connected table-out carry-out) ; !!! do not remove this

	;; job and component attributes
	
	,@(make-linear-jobs
	   '((attatch-a table-in part-a tray-a)
	     (screw-a screw-machine-a)
	     (attatch-b table1 part-b tray-b)
	     (attatch-c table1 part-c tray-c)
	     (screw-c screw-machine-c)) 2 4)
	
   ;;;;;;;;;;;;;;;; INITIAL STATES ;;;;;;;;;;;;;;;;
	;; 
   ;;;; Bases ;;;;;;;;;
	;; 
	;; All bases are at CARRY-IN
	;; Base and jobs. All bases must have finished NOTHING-DONE

	,@(make-initial-bases bases)

   ;;;; Arms ;;;;;;;;;;;;;;;;
	,@(make-initial-arms 'arm 'table-in))
       (:goal (and ,@(make-goal-bases bases 'screw-c)))
       (:metric minimize (total-cost)))))



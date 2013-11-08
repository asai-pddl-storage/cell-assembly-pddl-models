
(in-package :pddl.builder)

(defun model3a (basenum)
  (let ((bases (iter (for i below basenum)
		     (collect
			 (concatenate-symbols 'b i)))))
    `(define (problem ,(concatenate-symbols
			'cell-assembly-model3a
			basenum))
       (:domain cell-assembly)
       (:objects a1 a2 a3 - arm
		 ,@bases - base
		 p1 p2 p3 - component
		 t1 t2 t3 - tray
                 tb-for-p1
                 tb-for-p2
                 tb-for-p3
		 tb13 tb23 tb12 - table
		 m1 m2 m3 m4 m5 m6 - machine
		 j1 - machine-job
		 j2  - job
		 j3 - machine-job
		 j4 - machine-job
		 j5  - job
                 j6 - machine-job
		 j7  - job
		 j8 - machine-job
		 j9 - machine-job
		 )
       (:init
   ;;;;;;;;;;;;;;;; ATTRIBUTES ;;;;;;;;;;;;;;;;
	;; 
	;; cost initialization
	(= (total-cost) 0)   ; !!! do not remove this
	(= (loading-cost) 1) ; !!! do not remove this

	;; arm attributes
	,@(make-reachable 'a1 '(table-in
                                  t1 tb-for-p1 tb12 tb13 m3 m5))
	,@(make-reachable 'a2 '(t2 tb-for-p2 tb12 tb23 m1 m4))
        ,@(make-reachable 'a3 '(table-out
                                  t3 tb-for-p3 tb13 tb23 m2 m6))

	;; position attributes
	,@(make-dists  '(table-in m3
                         (tb12 m1 t2 tb-for-p2 m4 tb23 tb12)
                         (tb13 tb23 m2 table-out t3 tb-for-p3 m6 tb13)
                         t1 m5 tb-for-p1 table-in))
	
	;; conveyor attributes
	(connected carry-in table-in)   ; !!! do not remove this
	(connected table-out carry-out) ; !!! do not remove this

	;; job and component attributes
	
	,@(make-linear-jobs
	   '(;; (insert-gasket gasket-machine)
	     ;; (attatch-c table2 part-c tray-c)
             (j1 m1)
             (j2 tb-for-p1 p1 t1)
             (j3 m2)
             (j4 m3)
             (j5 tb-for-p2 p2 t2)
             (j6 m4)
             (j7 tb-for-p3 p3 t3)
             (j8 m5)
             (j9 m6)
             ) 2 4)
	
   ;;;;;;;;;;;;;;;; INITIAL STATES ;;;;;;;;;;;;;;;;
	;; 
   ;;;; Bases ;;;;;;;;;
	;; 
	;; All bases are at CARRY-IN
	;; Base and jobs. All bases must have finished NOTHING-DONE

	,@(make-initial-bases bases)

   ;;;; Arms ;;;;;;;;;;;;;;;;
	,@(make-initial-arms '(a1 a2 a3)
			     '(t1 m4 tb23)))
       (:goal (and
	       ,@(make-goal-bases bases 'j9)
	       ))
       (:metric minimize (total-cost)))))



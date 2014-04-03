
(in-package :pddl.builder)

(defun model3b (basenum)
  (let ((bases (iter (for i below basenum)
		     (collect
			 (concatenate-symbols 'b i)))))
    `(define (problem ,(concatenate-symbols
			'cell-assembly-model3b
			basenum))
         (:domain cell-assembly)
       (:objects a1 a2 a3 a4 - arm
		 ,@bases - base
		 p1 p2 p3 p4 - component
		 t1 t2 t3 t4 - tray
		 tb12 tb23 tb34 - table
		 m1 m2 m3 m4 - machine
		 j1 - job
		 j2 - machine-job
		 j3 - job
		 j4 - machine-job
                 j5 - job
		 j6 - machine-job
		 j7 - job
		 j8 - machine-job
		 )
       (:init
   ;;;;;;;;;;;;;;;; ATTRIBUTES ;;;;;;;;;;;;;;;;
	;; 
	;; cost initialization
	(= (total-cost) 0)   ; !!! do not remove this
	(= (loading-cost) 1) ; !!! do not remove this

	;; arm attributes
	,@(make-reachable 'a1 '(table-in
                                t1 tb12 m1))
	,@(make-reachable 'a2 '(tb12 t2 tb23 m2))
        ,@(make-reachable 'a3 '(tb23 table-out
                                t3 tb34 m3))
	,@(make-reachable 'a4 '(t4 tb34 m4))

	;; position attributes
	,@(make-dists '(table-in m1 t1
                        (tb12 m2 t2
                         (tb23 m3 t3 table-out
                          (tb34 m4 t4 tb34)
                          tb23)
                         tb12)
                        table-in))
	
	;; conveyor attributes
	(connected carry-in table-in)   ; !!! do not remove this
	(connected table-out carry-out) ; !!! do not remove this

	;; job and component attributes
	,@(make-linear-jobs
           '(;; (insert-gasket gasket-machine)
             ;; (attatch-c table2 part-c tray-c)
             (j1 tb12 p1 t1)
             (j2 m1)
             (j3 tb23 p2 t2)
             (j4 m2)
             (j5 tb34 p3 t3)
             (j6 m3)
             (j7 tb34 p4 t4)
             (j8 m4)
             ) 2 4)
	
   ;;;;;;;;;;;;;;;; INITIAL STATES ;;;;;;;;;;;;;;;;
	;; 
   ;;;; Bases ;;;;;;;;;
	;; 
	;; All bases are at CARRY-IN
	;; Base and jobs. All bases must have finished NOTHING-DONE

	,@(make-initial-bases bases)

   ;;;; Arms ;;;;;;;;;;;;;;;;
	,@(make-initial-arms '(a1 a2 a3 a4)
			     '(table-in tb12 tb23 tb34)))
       (:goal (and
	       ,@(make-goal-bases bases 'j8)
	       ))
       (:metric minimize (total-cost)))))



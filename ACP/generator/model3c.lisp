
(in-package :pddl.builder)

(defun model3c (basenum)
  (let ((bases (iter (for i below basenum)
		     (collect
			 (concatenate-symbols 'b i)))))
    `(define (problem ,(concatenate-symbols
			'cell-assembly-model3c
			basenum))
         (:domain cell-assembly)
       (:objects a1 a2 a3 a4 a5 - arm
		 ,@bases - base
		 p1 p2 p3 p4 p5 p6 - component
		 t1 t2 t3 t4 t5 t6  - tray
                 tb-for-234
                 tb-for-1
                 tb-for-56
		 tb12 tb23 tb24 tb35 tb45 - table
		 m1 m2 m3 m4 m5 - machine
                 j1 j2 j3 j4 j5 j6 - job
                 mj1 mj2 mj3 mj4 mj5 - machine-job
		 )
       (:init
   ;;;;;;;;;;;;;;;; ATTRIBUTES ;;;;;;;;;;;;;;;;
	;; 
	;; cost initialization
	(= (total-cost) 0)   ; !!! do not remove this
	(= (loading-cost) 1) ; !!! do not remove this

	;; arm attributes
	,@(make-reachable 'a1 '(table-in m1 m2 tb12))
	,@(make-reachable 'a2 '(tb12 tb23 tb24 m4))
        ,@(make-reachable 'a3 '(tb23 t2 t3 t4 tb-for-234 tb35))
	,@(make-reachable 'a4 '(tb24 t1 t5 t6 tb-for-1 tb-for-56 tb45))
        ,@(make-reachable 'a5 '(tb35 tb45 m3 m5 table-out))

	;; position attributes
	,@(make-dists '(table-in m1
                        (tb12
                         (tb23
                          t4 tb-for-234 t2 t3
                          (tb35 m3 table-out m5 tb45 tb35)
                          tb23)
                         (tb24
                          tb45 tb-for-56 tb-for-1 t6 t5 t1
                          tb24)
                         m4
                         tb12)
                        m2
                        table-in))
	
	;; conveyor attributes
	(connected carry-in table-in)   ; !!! do not remove this
	(connected table-out carry-out) ; !!! do not remove this

	;; job and component attributes
	,@(make-linear-jobs
           '(;; (insert-gasket gasket-machine)
             ;; (attatch-c table2 part-c tray-c)
             (mj1 m1)
             (mj2 m2)
             (j1 tb-for-1 p1 t1)
             (j2 tb-for-234 p2 t2)
             (j3 tb-for-234 p3 t3)
             (mj3 m3)             
             (j4 tb-for-234 p4 t4)
             (mj4 m4)
             (j5 tb-for-56 p5 t5)
             (j6 tb-for-56 p6 t6)
             (mj5 m5)
             ) 2 4)
	
   ;;;;;;;;;;;;;;;; INITIAL STATES ;;;;;;;;;;;;;;;;
	;; 
   ;;;; Bases ;;;;;;;;;
	;; 
	;; All bases are at CARRY-IN
	;; Base and jobs. All bases must have finished NOTHING-DONE

	,@(make-initial-bases bases)

   ;;;; Arms ;;;;;;;;;;;;;;;;
	,@(make-initial-arms '(a1 a2 a3 a4 a5)
			     '(table-in tb12 tb23 tb24 tb45)))
       (:goal (and
	       ,@(make-goal-bases bases 'mj5)
	       ))
       (:metric minimize (total-cost)))))



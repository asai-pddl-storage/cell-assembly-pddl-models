(define (domain cell-assembly)
  (:requirements :strips :typing
		 :negative-preconditions
		 :action-costs)
  (:types arm		       ;
	  position	       ; Where the arms is potentially able to go
	  holdable
	  job - object
	  
	  table - position     ; The place to attach components with arms.
	  container - position ; Has infinite capacity to store things.
	  
	  conveyor - container ; Slide things in/away. Infinite length.
	  tray - container     ; Part trays.
	  
	  machine - table      ; Tables which does specific jobs by itself.
	  machine-job - job    ; Jobs which should be done on machines.
	  base - holdable      ;
	  component - holdable ; Things which should be attatched on tables.
	  )
  (:constants carry-in - conveyor  ; the start of the base
	      carry-out - conveyor ; the goal of the base
	      table-in - table	   ; always connected to the conveyor
	      table-out - table	   ; always connected to the conveyor
	      nothing-done - job)  ; general start state of any base
  (:predicates 

   ;; arm attributes
   (reachable ?arm - arm ?to - position)
   ;; position attributes
   (connected ?from ?to - position) ;; by conveyor
   ;; job attributes
   (depends ?prev-job ?job - job)
   (job-available-at ?job - job ?pos - table)
   (uses ?job - job ?component - component)
   
   ;; state description
   (at ?obj - object ?pos - position)
   (arm-present ?pos - position)
   (base-present ?pos - position)
   
   (hold ?arm - arm ?holdable - holdable)
   (free ?arm - arm) ;; instead of (not (exists (?base) (hold ?arm ?base)))

   ;; (used ?component - component)
   
   (finished ?job - job ?base - base))

  (:functions
   (total-cost)
   (loading-cost)        ; the cost to set and eject base
   (job-cost ?job - job) ; the cost to do each job
   
   ;; slidng bases costs nothing

   (move-cost ?from ?to - position) - number)

  (:action move-arm
	   ;;   Moves the arm (?arm) from the source
	   ;; (?from) to the destination (?to). The occupied and not-
	   ;; occupied propositions enforce a mutual exclusion con-
	   ;; straint so that only one arm can occupy any given location
	   ;; at a time.
	   :parameters (?arm - arm ?from - position ?to - position)
	   :precondition (and (at ?arm ?from)
			      (not (arm-present ?to))
			      (free ?arm)
			      (reachable ?arm ?to))
	   :effect (and (at ?arm ?to)
			(arm-present ?to)
			(not (at ?arm ?from))
			(not (arm-present ?from))
			(increase (total-cost) (move-cost ?from ?to))))

  (:action move-arm-holding
	   ;;   Moves the arm (?arm) from the source
	   ;; (?from) to the destination (?to). The occupied and not-
	   ;; occupied propositions enforce a mutual exclusion con-
	   ;; straint so that only one arm can occupy any given location
	   ;; at a time.
	   :parameters (?arm - arm
		        ?from - position ?to - position
			?thing - holdable)
	   :precondition (and (at ?arm ?from)
			      (not (arm-present ?to))
			      (hold ?arm ?thing)
			      (reachable ?arm ?to))
	   :effect (and (at ?arm ?to)
			(arm-present ?to)
			(not (at ?arm ?from))
			(not (arm-present ?from))
			(increase (total-cost) (move-cost ?from ?to))))
  
  (:action eject-base
	   ;; Eject Base: Uses an arm (?arm) to grasp and pick up a
	   ;; base (?base) from a machine or a table (?pos). Resets
	   ;; the mutual exclusion constraint enforcing the rule that
	   ;; at most 1 base can be at a location (notbaseplaced ?pos)
	   :parameters (?base - base ?arm - arm ?pos - table)
	   :precondition (and (at ?base ?pos)
			      (at ?arm ?pos)
			      (free ?arm))
	   :effect (and (hold ?arm ?base)
			(not (free ?arm))
			(not (base-present ?pos))
			(not (at ?base ?pos))
			(increase (total-cost) (loading-cost))))
  
  (:action set-base
	   ;; Set Base: Commands an arm (?arm) that is holding
	   ;; a particular base (?base) to set the base on a machine
	   ;; or table (?pos). Each machine/table has a mutual
	   ;; exclusion constraint ensuring at most 1 base is placed
	   ;; on it (notbaseplaced).
	   :parameters (?base - base ?arm - arm ?pos - table)
	   :precondition (and (hold ?arm ?base)
			      (at ?arm ?pos)
			      (not (base-present ?pos)))
	   :effect (and (at ?base ?pos)
			(free ?arm)
			(not (hold ?arm ?base))
			(base-present ?pos)
			(increase (total-cost) (loading-cost))))

  (:action slide-base-in
	   ;; Slide Base: Uses a slide device to move a base. 
	   ;; MOD : CARRY-IN device only.
	   ;; MOD : It DOES care if the destination is already used.
	   :parameters (?base - base ?from - conveyor ?to - table)
	   :precondition (and (at ?base ?from)
			      (connected ?from ?to)
			      (not (base-present ?to)))
	   :effect (and (at ?base ?to)
			(not (at ?base ?from))
			(base-present ?to)
			(increase (total-cost) (loading-cost))))

  (:action slide-base-out
	   ;; Slide Base: Uses a slide device to move a base. 
	   ;; MOD : CARRY-OUT device only.
	   ;; MOD : It doesn't care if the destination is already used.
	   ;; MOD : After the action the base acts like it had disappeard.
	   :parameters (?base - base ?from - table ?to - conveyor)
	   :precondition (and (at ?base ?from)
			      (connected ?from ?to))
	   :effect (and (at ?base ?to)
			(not (at ?base ?from))
			(not (base-present ?from))
			(increase (total-cost) (loading-cost))))
  
  (:action pickup-component
	   ;; Pick Parts by Arm: Use an arm (?arm) to pick up a part
	   ;; (?part). The part will later be used by a BaseAssem-
	   ;; blePickedPartsXByArm action (see below).
	   ;; 
	   ;; NOTE: basically, we assume there are unlimited number of
	   ;; components in trays. That's why this action lacks the
	   ;; delete effect on the place of the component.
	   :parameters (?component - component ?arm - arm ?pos - tray)
	   :precondition (and (free ?arm)
			      (at ?arm ?pos)
			      (at ?component ?pos))
	   :effect (and (hold ?arm ?component)
			(not (free ?arm))
			(increase (total-cost) (loading-cost))))
  
  (:action assemble-with-machine
	   ;; Base Assemble by Machine: Use a machine (?pos) to
	   ;; perform an assembly operation (e.g., tighten the screw) on
	   ;; a base (?base).

	   ;; The ordering constraints which are determined when the
	   ;; object is designed are encoded as a set of ordering proposi-
	   ;; tions, finished_Step_X and unfihished_Step_X, which rep-
	   ;; resent whether an assembly step X has been performed
	   ;; already.
	   :parameters (?base - base
			?machine - machine
			?job - machine-job
			?prev-job - job)
	   :precondition (and ;; machine specification
			  (job-available-at ?job ?machine)
			  ;; state specification
			  (not (finished ?job ?base))
			  (at ?base ?machine)
			  ;; job dependency specification (linear)
			  (depends ?prev-job ?job)
			  (finished ?prev-job ?base))
	   :effect (and (finished ?job ?base)
			(increase (total-cost) (job-cost ?job))))

  (:action assemble-with-arm
	   ;; Base Assemble Picked Parts by Arm: Uses an arm
	   ;; (?arm) to attach a part (?part) to a base.
	   ;; 
	   ;; NOTE: components are not distinguished between each
	   ;;       other
	   :parameters (?component - component
			?job ?prev-job - job
			?base - base
			?arm - arm
			?pos - table)
	   :precondition (and
			  ;; job specification
			  (job-available-at ?job ?pos)
			  (uses ?job ?component)
			  ;; state specification
			  (hold ?arm ?component)
			  (at ?arm ?pos)
			  (at ?base ?pos)
			  ;; job dependency specification (linear)
			  (depends ?prev-job ?job)
			  (finished ?prev-job ?base))
	   :effect (and (finished ?job ?base)
			(free ?arm)
			(not (hold ?arm ?component))
			(increase (total-cost) (job-cost ?job)))))


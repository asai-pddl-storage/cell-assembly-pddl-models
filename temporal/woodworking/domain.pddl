;; modified temporal domain of woodworking ipc2008.

(define (domain woodworking-tempo-infinity)
    (:requirements :typing :durative-actions)
  (:types
   ;; 色、木材種別、木材、機械
   acolour awood woodobj machine 
   ;; 表面状態 ワニスなどの状態
   surface treatmentstatus
   ;; 板のサイズ規格、パーツサイズ規格
   apartsize - object
   highspeed-saw glazer grinder immersion-varnisher
   planer saw spray-varnisher - machine
   board part - woodobj)

  (:constants
   verysmooth smooth rough - surface
   ;; ニス、光ってる、なにもされてない、色がついてる?
   varnished glazed untreated colourfragments - treatmentstatus
   ;; 色
   natural - acolour
   small medium large - apartsize)

  (:predicates
   ;; 機械が使用中
   (idle ?machine - machine)
   ;; 切り出されていない。
   (unused ?obj - part)
   ;; 機械の中に入っておらず、または切り出されており、手で操作することができる。
   (available ?obj - woodobj)

   (surface-condition ?obj - woodobj ?surface - surface)
   (treatment ?obj - part ?treatment - treatmentstatus)
   (colour ?obj - part ?colour - acolour)

   ;; 木の種類。
   (wood ?obj - woodobj ?wood - awood)
   (goalsize ?p - part ?size - apartsize)
   (in-highspeed-saw ?b - board ?m - highspeed-saw)
   (empty ?m - highspeed-saw)
   ;; 機械に装填されている塗料の色
   (has-colour ?machine - machine ?colour - acolour)
   (contains-part ?b - board ?p - part)
   ;; grindによる順序を規定
   (grind-treatment-change ?old ?new - treatmentstatus)
   (is-smooth ?surface - surface))

  (:functions (total-cost) - number
              (spray-varnish-cost ?obj - part) - number
              (glaze-cost ?obj - part) - number
              (grind-cost ?obj - part) - number
              (plane-cost ?obj - part) - number)

  (:durative-action do-immersion-varnish
                    :parameters (?x - part ?m - immersion-varnisher 
                                    ?newcolour - acolour ?surface - surface)
                    :duration (= ?duration 10)
                    :condition (and
                                (at start (idle ?m))
                                (at start (available ?x))
                                (at start (surface-condition ?x ?surface))
                                (at start (is-smooth ?surface))
                                (at start (has-colour ?m ?newcolour))
                                (at start (treatment ?x untreated)))
                    :effect (and 
                             (at start (not (idle ?m)))
                             (at start (not (available ?x)))
                             (at start (not (treatment ?x untreated)))
                             (at start (not (colour ?x natural)))
                             (at end (idle ?m))
                             (at end (available ?x))
                             (at end (treatment ?x varnished))
                             (at end (colour ?x ?newcolour))))

  (:durative-action do-spray-varnish
                    :parameters (?x - part ?m - spray-varnisher 
                                    ?newcolour - acolour ?surface - surface)
                    :duration (= ?duration (spray-varnish-cost ?x))
                    :condition (and
                                (at start (idle ?m))
                                (at start (available ?x))
                                (at start (surface-condition ?x ?surface))
                                (at start (is-smooth ?surface))
                                (at start (has-colour ?m ?newcolour))
                                (at start (treatment ?x untreated)))
                    :effect (and 
                             (at start (not (idle ?m)))
                             (at start (not (available ?x)))
                             (at start (not (treatment ?x untreated)))
                             (at start (not (colour ?x natural)))
                             (at end (idle ?m))
                             (at end (available ?x))
                             (at end (treatment ?x varnished))
                             (at end (colour ?x ?newcolour))))

  (:durative-action do-glaze
                    :parameters (?x - part ?m - glazer 
                                    ?newcolour - acolour)
                    :duration (= ?duration (glaze-cost ?x))
                    :condition (and
                                (at start (idle ?m))
                                (at start (available ?x))
                                (at start (has-colour ?m ?newcolour))
                                (at start (treatment ?x untreated)))
                    :effect (and 
                             (at start (not (idle ?m)))
                             (at start (not (available ?x)))
                             (at start (not (treatment ?x untreated)))
                             (at start (not (colour ?x natural)))
                             (at end (idle ?m))
                             (at end (available ?x))
                             (at end (treatment ?x glazed))
                             (at end (colour ?x ?newcolour))))

  (:durative-action do-grind
                    :parameters (?x - part ?m - grinder ?oldsurface - surface
                                    ?oldcolour - acolour 
                                    ?oldtreatment ?newtreatment - treatmentstatus) 
                    :duration (= ?duration (grind-cost ?x))
                    :condition (and 
                                (at start (idle ?m))
                                (at start (available ?x))
                                (at start (surface-condition ?x ?oldsurface))
                                (at start (is-smooth ?oldsurface)) 
                                (at start (colour ?x ?oldcolour))
                                (at start (treatment ?x ?oldtreatment))
                                (at start (grind-treatment-change ?oldtreatment ?newtreatment)))
                    :effect (and
                             (at start (not (idle ?m)))
                             (at start (not (available ?x)))
                             (at start (not (surface-condition ?x ?oldsurface)))
                             (at start (not (treatment ?x ?oldtreatment)))
                             (at start (not (colour ?x ?oldcolour)))
                             (at end (idle ?m))
                             (at end (available ?x))
                             (at end (surface-condition ?x verysmooth))
                             (at end (treatment ?x ?newtreatment))
                             (at end (colour ?x natural))))

  (:durative-action do-plane
                    :parameters (?x - part ?m - planer ?oldsurface - surface
                                    ?oldcolour - acolour ?oldtreatment - treatmentstatus) 
                    :duration (= ?duration (plane-cost ?x))
                    :condition (and 
                                (at start (idle ?m))
                                (at start (available ?x))
                                (at start (surface-condition ?x ?oldsurface))
                                (at start (treatment ?x ?oldtreatment))
                                (at start (colour ?x ?oldcolour)))
                    :effect (and
                             (at start (not (idle ?m)))
                             (at start (not (available ?x)))
                             (at start (not (surface-condition ?x ?oldsurface)))
                             (at start (not (treatment ?x ?oldtreatment)))
                             (at start (not (colour ?x ?oldcolour)))
                             (at end (idle ?m))
                             (at end (available ?x))
                             (at end (surface-condition ?x smooth))
                             (at end (treatment ?x untreated))
                             (at end (colour ?x natural))))
  (:durative-action load-highspeed-saw
                    :parameters (?b - board ?m - highspeed-saw)
                    :duration (= ?duration 30)
                    :condition (and
                                (at start (idle ?m))
                                (at start (empty ?m))
                                (at start (available ?b)))
                    :effect (and
                             (at start (not (idle ?m)))
                             (at start (not (available ?b)))
                             (at start (not (empty ?m)))
                             (at end (idle ?m))
                             (at end (in-highspeed-saw ?b ?m))))
  
  (:durative-action unload-highspeed-saw
                    :parameters (?b - board ?m - highspeed-saw)
                    :duration (= ?duration 10)
                    :condition (and
                                (at start (idle ?m))
                                (at start (in-highspeed-saw ?b ?m)))
                    :effect (and
                             (at start (not (idle ?m)))
                             (at end (available ?b))
                             (at end (not (in-highspeed-saw ?b ?m)))
                             (at end (empty ?m))
                             (at end (idle ?m))))
  
  ;; 以下、board size に関する条件を取り除く。
  (:durative-action cut-board-small
                    :parameters (?b - board ?p - part ?m - highspeed-saw ?w - awood
                                    ?surface - surface)
                    :duration (= ?duration 10)
                    :condition (and
                                (at start (idle ?m))
                                (at start (unused ?p))
                                (at start (goalsize ?p small))
                                (at start (in-highspeed-saw ?b ?m))
                                (at start (wood ?b ?w))
                                (at start (surface-condition ?b ?surface)))
                    :effect (and
                             (at start (not (idle ?m)))
                             (at start (not (unused ?p)))
                             (at end (idle ?m))
                             (at end (available ?p))
                             (at end (wood ?p ?w))
                             (at end (surface-condition ?p ?surface))
                             (at end (colour ?p natural)) 
                             (at end (treatment ?p untreated))))
  (:durative-action cut-board-medium
                    :parameters (?b - board ?p - part ?m - highspeed-saw ?w - awood
                                    ?surface - surface)
                    :duration (= ?duration 10)
                    :condition (and
                                (at start (idle ?m))
                                (at start (unused ?p))
                                (at start (goalsize ?p medium))
                                (at start (in-highspeed-saw ?b ?m))
                                (at start (wood ?b ?w))
                                (at start (surface-condition ?b ?surface)))
                    :effect (and
                             (at start (not (idle ?m)))
                             (at start (not (unused ?p)))
                             (at end (idle ?m))
                             (at end (available ?p))
                             (at end (wood ?p ?w))
                             (at end (surface-condition ?p ?surface))
                             (at end (colour ?p natural)) 
                             (at end (treatment ?p untreated))))
  (:durative-action cut-board-large
                    :parameters (?b - board ?p - part ?m - highspeed-saw ?w - awood
                                    ?surface - surface)
                    :duration (= ?duration 10)
                    :condition (and
                                (at start (idle ?m))
                                (at start (unused ?p))
                                (at start (goalsize ?p large))
                                (at start (in-highspeed-saw ?b ?m))
                                (at start (wood ?b ?w))
                                (at start (surface-condition ?b ?surface)))
                    :effect (and
                             (at start (not (idle ?m)))
                             (at start (not (unused ?p)))
                             (at end (idle ?m))
                             (at end (available ?p))
                             (at end (wood ?p ?w))
                             (at end (surface-condition ?p ?surface))
                             (at end (colour ?p natural)) 
                             (at end (treatment ?p untreated))))
  (:durative-action do-saw-small
                    :parameters (?b - board ?p - part ?m - saw ?w - awood
                                    ?surface - surface)
                    :duration (= ?duration 30)
                    :condition (and 
                                (at start (idle ?m))
                                (at start (unused ?p))
                                (at start (goalsize ?p small))
                                (at start (available ?b))
                                (at start (wood ?b ?w))
                                (at start (surface-condition ?b ?surface)))
                    :effect (and
                             (at start (not (idle ?m)))
                             (at start (not (unused ?p)))
                             (at start (not (available ?b)))
                             (at end (idle ?m))
                             (at end (available ?p))
                             (at end (available ?b))
                             (at end (wood ?p ?w))
                             (at end (surface-condition ?p ?surface))
                             (at end (colour ?p natural)) 
                             (at end (treatment ?p untreated))))
  (:durative-action do-saw-medium
                    :parameters (?b - board ?p - part ?m - saw ?w - awood
                                    ?surface - surface)
                    :duration (= ?duration 30)
                    :condition (and 
                                (at start (idle ?m))
                                (at start (unused ?p))
                                (at start (goalsize ?p medium))
                                (at start (available ?b))
                                (at start (wood ?b ?w))
                                (at start (surface-condition ?b ?surface)))
                    :effect (and
                             (at start (not (idle ?m)))
                             (at start (not (unused ?p)))
                             (at start (not (available ?b)))
                             (at end (idle ?m))
                             (at end (available ?p))
                             (at end (available ?b))
                             (at end (wood ?p ?w))
                             (at end (surface-condition ?p ?surface))
                             (at end (colour ?p natural)) 
                             (at end (treatment ?p untreated))))
  (:durative-action do-saw-large
                    :parameters (?b - board ?p - part ?m - saw ?w - awood
                                    ?surface - surface)
                    :duration (= ?duration 30)
                    :condition (and 
                                (at start (idle ?m))
                                (at start (unused ?p))
                                (at start (goalsize ?p large))
                                (at start (available ?b))
                                (at start (wood ?b ?w))
                                (at start (surface-condition ?b ?surface)))
                    :effect (and
                             (at start (not (idle ?m)))
                             (at start (not (unused ?p)))
                             (at start (not (available ?b)))
                             (at end (idle ?m))
                             (at end (available ?p))
                             (at end (available ?b))
                             (at end (wood ?p ?w))
                             (at end (surface-condition ?p ?surface))
                             (at end (colour ?p natural)) 
                             (at end (treatment ?p untreated)))))

;; manually converted from the temporal Woodworking
;;

(define (domain woodworking-loop-tempo-converted)
  (:requirements :typing :action-costs)
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

   (immersion-varnishing-now ?x - part ?m - immersion-varnisher 
                             ?newcolour - acolour ?surface - surface)
   (spray-varnishing-now ?x - part ?m - spray-varnisher 
                         ?newcolour - acolour ?surface - surface)
   (glazing-now ?x - part ?m - glazer 
                ?newcolour - acolour)
   (grinding-now ?x - part ?m - grinder ?oldsurface - surface
                 ?oldcolour - acolour 
                 ?oldtreatment ?newtreatment - treatmentstatus)
   (planing-now ?x - part ?m - planer ?oldsurface - surface
                ?oldcolour - acolour ?oldtreatment - treatmentstatus)
   (loading-now ?b - board ?m - highspeed-saw)
   (unloading-now ?b - board ?m - highspeed-saw)
   (cutting-now ?b - board ?p - part ?m - highspeed-saw ?w - awood
                ?surface - surface ?s - apartsize)
   (sawing-now ?b - board ?p - part ?m - saw ?w - awood
               ?surface - surface ?s - apartsize)
   
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

  ;; immersion: (液体に)浸すこと.
  ;; varnish : ニス，ワニス.
  ;; 浸すことによって塗装+ニス。
  ;; 大きさによってコストが変わらないみたいだ。
  ;; smooth でないといけない。
  (:action do-immersion-varnish
           :parameters (?x - part ?m - immersion-varnisher 
                           ?newcolour - acolour ?surface - surface)
           :precondition (and
                          (idle ?m)
                          (available ?x)
                          (has-colour ?m ?newcolour)
                          (surface-condition ?x ?surface)
                          (is-smooth ?surface)
                          (treatment ?x untreated))
           :effect (and
                    (increase (total-cost) 10)
                    (not (idle ?m))
                    (not (available ?x))
                    (not (treatment ?x untreated))
                    (not (colour ?x natural))
                    (immersion-varnishing-now ?x ?m ?newcolour ?surface)))
  (:action do-immersion-varnish-end
           :parameters (?x - part ?m - immersion-varnisher 
                           ?newcolour - acolour ?surface - surface)
           :precondition (immersion-varnishing-now ?x ?m ?newcolour ?surface)
           :effect (and
                    (not (immersion-varnishing-now ?x ?m ?newcolour ?surface))
                    (idle ?m)
                    (available ?x)
                    (treatment ?x varnished)
                    (colour ?x ?newcolour)))

  ;; スプレーによって塗装+ニス。
  ;; 効果は do-immersion-varnish と同じ。
  ;; smooth でないといけない。
  (:action do-spray-varnish
           :parameters (?x - part ?m - spray-varnisher 
                           ?newcolour - acolour ?surface - surface)
           :precondition (and
                          (idle ?m)
                          (available ?x)
                          (has-colour ?m ?newcolour)
                          (surface-condition ?x ?surface)
                          (is-smooth ?surface)
                          (treatment ?x untreated))
           :effect (and 
                    (increase (total-cost) (spray-varnish-cost ?x))
                    (not (idle ?m))
                    (not (available ?x))
                    (not (treatment ?x untreated))
                    (not (colour ?x natural))
                    (spray-varnishing-now ?x ?m ?newcolour ?surface)))
  (:action do-spray-varnish-end
           :parameters (?x - part ?m - spray-varnisher 
                           ?newcolour - acolour ?surface - surface)
           :precondition (spray-varnishing-now ?x ?m ?newcolour ?surface)
           :effect (and 
                    (not (spray-varnishing-now ?x ?m ?newcolour ?surface))
                    (idle ?m)
                    (available ?x)
                    (treatment ?x varnished)
                    (colour ?x ?newcolour)))

  ;;〈紙・布・皮などに〉光滑剤を塗る，つやつけをする.
  ;; smooth でなくてもいい。色+glazed状態
  (:action do-glaze
           :parameters (?x - part ?m - glazer 
                           ?newcolour - acolour)
           :precondition (and
                          (idle ?m)
                          (available ?x)
                          (has-colour ?m ?newcolour)
                          (treatment ?x untreated))
           :effect (and 
                    (increase (total-cost) (glaze-cost ?x))
                    (not (idle ?m))
                    (not (available ?x))
                    (not (treatment ?x untreated))
                    (not (colour ?x natural))
                    (glazing-now ?x ?m ?newcolour)))
  (:action do-glaze-end
           :parameters (?x - part ?m - glazer 
                           ?newcolour - acolour)
           :precondition (glazing-now ?x ?m ?newcolour)
           :effect (and
                    (not (glazing-now ?x ?m ?newcolour))
                    (idle ?m)
                    (available ?x)
                    (treatment ?x glazed)
                    (colour ?x ?newcolour)))

  ;; 削ってverysmoothに。塗装をやり直し。色は消える。
  ;; 表面は grind-treatment-change に従って順番に変化していく。
  (:action do-grind
           :parameters (?x - part ?m - grinder ?oldsurface - surface
                           ?oldcolour - acolour 
                           ?oldtreatment ?newtreatment - treatmentstatus)
           :precondition (and 
                          (idle ?m)
                          (available ?x)
                          (surface-condition ?x ?oldsurface)
                          (is-smooth ?oldsurface)
                          (colour ?x ?oldcolour)
                          (treatment ?x ?oldtreatment)
                          (grind-treatment-change ?oldtreatment ?newtreatment))
           :effect (and
                    (increase (total-cost) (grind-cost ?x))
                    (not (idle ?m))
                    (not (available ?x))
                    (not (surface-condition ?x ?oldsurface))
                    (not (treatment ?x ?oldtreatment))
                    (not (colour ?x ?oldcolour))
                    (grinding-now ?x ?m ?oldsurface ?oldcolour ?oldtreatment ?newtreatment)))

  (:action do-grind-end
           :parameters (?x - part ?m - grinder ?oldsurface - surface
                           ?oldcolour - acolour 
                           ?oldtreatment ?newtreatment - treatmentstatus) 
           :precondition (grinding-now ?x ?m ?oldsurface ?oldcolour ?oldtreatment ?newtreatment)
           :effect (and
                    (not (grinding-now ?x ?m ?oldsurface ?oldcolour ?oldtreatment ?newtreatment))
                    (idle ?m)
                    (available ?x)
                    (surface-condition ?x verysmooth)
                    (treatment ?x untreated)
                    (colour ?x natural)))

  ;; 削ってsmoothに。
  (:action do-plane
           :parameters (?x - part ?m - planer ?oldsurface - surface
                           ?oldcolour - acolour ?oldtreatment - treatmentstatus) 
           :precondition (and 
                          (idle ?m)
                          (available ?x)
                          (surface-condition ?x ?oldsurface)
                          (treatment ?x ?oldtreatment)
                          (colour ?x ?oldcolour))
           :effect (and
                    (increase (total-cost) (plane-cost ?x))
                    (not (idle ?m))
                    (not (available ?x))
                    (not (surface-condition ?x ?oldsurface))
                    (not (treatment ?x ?oldtreatment))
                    (not (colour ?x ?oldcolour))
                    (planing-now ?x ?m ?oldsurface ?oldcolour ?oldtreatment)))
  (:action do-plane-end
           :parameters (?x - part ?m - planer ?oldsurface - surface
                           ?oldcolour - acolour ?oldtreatment - treatmentstatus) 
           :precondition (planing-now ?x ?m ?oldsurface ?oldcolour ?oldtreatment)
           :effect (and
                    (not (planing-now ?x ?m ?oldsurface ?oldcolour ?oldtreatment))
                    (idle ?m)
                    (available ?x)
                    (surface-condition ?x smooth)
                    (treatment ?x untreated)
                    (colour ?x natural)))

  ;; のこぎりで切る機械に入れる。
  (:action load-highspeed-saw
           :parameters (?b - board ?m - highspeed-saw)
           :precondition (and
                          (idle ?m)
                          (empty ?m)
                          (available ?b))
           :effect (and
                    (increase (total-cost) 30)
                    (not (idle ?m))
                    (not (available ?b))
                    (not (empty ?m))
                    (loading-now ?b ?m)))
  (:action load-highspeed-saw-end
           :parameters (?b - board ?m - highspeed-saw)
           :precondition (loading-now ?b ?m)
           :effect (and
                    (not (loading-now ?b ?m))
                    (idle ?m)
                    (in-highspeed-saw ?b ?m)))
  
  ;; のこぎりで切る機械から取り出す。
  (:action unload-highspeed-saw
           :parameters (?b - board ?m - highspeed-saw)
           :precondition (and (idle ?m)
                              (in-highspeed-saw ?b ?m))
           :effect (and
                    (increase (total-cost) 10)
                    (not (idle ?m))
                    (unloading-now ?b ?m)))
  (:action unload-highspeed-saw-end
           :parameters (?b - board ?m - highspeed-saw)
           :precondition (and (not (idle ?m))
                              (in-highspeed-saw ?b ?m))
           :effect (and
                    (not (unloading-now ?b ?m))
                    (idle ?m)
                    (available ?b)
                    (not (in-highspeed-saw ?b ?m))
                    (empty ?m)))
  
  ;; のこぎり機械に入っている場合、小さく切る。
  ;; do-saw-small と違って、時間は10しかかからない。
  ;; 
  (:action cut-board-small
           :parameters (?b - board ?p - part ?m - highspeed-saw ?w - awood
                           ?surface - surface)
           :precondition (and
                          (idle ?m)
                          (unused ?p)
                          (goalsize ?p small)
                          (in-highspeed-saw ?b ?m)
                          (wood ?b ?w)
                          (surface-condition ?b ?surface))
           :effect (and
                    (not (idle ?m))
                    (not (unused ?p))
                    (increase (total-cost) 10)
                    (cutting-now ?b ?p ?m ?w ?surface small)))
  (:action cut-board-small-end
           :parameters (?b - board ?p - part ?m - highspeed-saw ?w - awood
                           ?surface - surface)
           :precondition (cutting-now ?b ?p ?m ?w ?surface small)
           :effect (and
                    (not (cutting-now ?b ?p ?m ?w ?surface small))
                    (idle ?m)
                    (available ?p)
                    (wood ?p ?w)
                    (surface-condition ?p ?surface)
                    (colour ?p natural)
                    (treatment ?p untreated)))
  (:action cut-board-medium
           :parameters (?b - board ?p - part ?m - highspeed-saw ?w - awood
                           ?surface - surface)
           :precondition (and
                          (idle ?m)
                          (unused ?p)
                          (goalsize ?p medium)
                          (in-highspeed-saw ?b ?m)
                          (wood ?b ?w)
                          (surface-condition ?b ?surface))
           :effect (and
                    (not (idle ?m))
                    (not (unused ?p))
                    (increase (total-cost) 10)
                    (cutting-now ?b ?p ?m ?w ?surface medium)))
  (:action cut-board-medium-end
           :parameters (?b - board ?p - part ?m - highspeed-saw ?w - awood
                           ?surface - surface)
           :precondition (cutting-now ?b ?p ?m ?w ?surface medium)
           :effect (and
                    (not (cutting-now ?b ?p ?m ?w ?surface medium))
                    (idle ?m)
                    (available ?p)
                    (wood ?p ?w)
                    (surface-condition ?p ?surface)
                    (colour ?p natural)
                    (treatment ?p untreated)))
  (:action cut-board-large
           :parameters (?b - board ?p - part ?m - highspeed-saw ?w - awood
                           ?surface - surface)
           :precondition (and
                          (idle ?m)
                          (unused ?p)
                          (goalsize ?p large)
                          (in-highspeed-saw ?b ?m)
                          (wood ?b ?w)
                          (surface-condition ?b ?surface))
           :effect (and
                    (not (idle ?m))
                    (not (unused ?p))
                    (increase (total-cost) 10)
                    (cutting-now ?b ?p ?m ?w ?surface large)))
  (:action cut-board-large-end
           :parameters (?b - board ?p - part ?m - highspeed-saw ?w - awood
                           ?surface - surface)
           :precondition (cutting-now ?b ?p ?m ?w ?surface large)
           :effect (and
                    (not (cutting-now ?b ?p ?m ?w ?surface large))
                    (idle ?m)
                    (available ?p)
                    (wood ?p ?w)
                    (surface-condition ?p ?surface)
                    (colour ?p natural)
                    (treatment ?p untreated)))

  ;; 手で板を切り出す。板が大きかろうが小さかろうが30かかる
  ;; 板の大きさは小さくなる。(size_after になる)
  ;; ここらへんは do-saw-small/medium/large全部同じ。
  ;; largeを切りだそうとすると板が3段階小さくなる。
  (:action do-saw-small
           :parameters (?b - board ?p - part ?m - saw ?w - awood
                           ?surface - surface) 
           :precondition (and 
                          (idle ?m)
                          (unused ?p)
                          (goalsize ?p small)
                          (available ?b)
                          (wood ?b ?w)
                          (surface-condition ?b ?surface))
           :effect (and
                    (not (idle ?m))
                    (not (unused ?p))
                    (not (available ?b))
                    (increase (total-cost) 30)
                    (sawing-now ?b ?p ?m ?w ?surface small)))
  (:action do-saw-small-end
           :parameters (?b - board ?p - part ?m - saw ?w - awood
                           ?surface - surface) 
           :precondition (sawing-now ?b ?p ?m ?w ?surface small)
           :effect (and
                    (not (sawing-now ?b ?p ?m ?w ?surface small))
                    (idle ?m)
                    (available ?b)
                    (available ?p)
                    (wood ?p ?w)
                    (surface-condition ?p ?surface)
                    (colour ?p natural)
                    (treatment ?p untreated)))
  (:action do-saw-medium
           :parameters (?b - board ?p - part ?m - saw ?w - awood
                           ?surface - surface) 
           :precondition (and 
                          (idle ?m)
                          (unused ?p)
                          (goalsize ?p medium)
                          (available ?b)
                          (wood ?b ?w)
                          (surface-condition ?b ?surface))
           :effect (and
                    (not (idle ?m))
                    (not (unused ?p))
                    (not (available ?b))
                    (increase (total-cost) 30)
                    (sawing-now ?b ?p ?m ?w ?surface medium)))
  (:action do-saw-medium-end
           :parameters (?b - board ?p - part ?m - saw ?w - awood
                           ?surface - surface) 
           :precondition (sawing-now ?b ?p ?m ?w ?surface medium)
           :effect (and
                    (not (sawing-now ?b ?p ?m ?w ?surface medium))
                    (idle ?m)
                    (available ?b)
                    (available ?p)
                    (wood ?p ?w)
                    (surface-condition ?p ?surface)
                    (colour ?p natural) 
                    (treatment ?p untreated)))
  (:action do-saw-large
           :parameters (?b - board ?p - part ?m - saw ?w - awood
                           ?surface - surface) 
           :precondition (and 
                          (idle ?m)
                          (unused ?p)
                          (goalsize ?p large)
                          (available ?b)
                          (wood ?b ?w)
                          (surface-condition ?b ?surface))
           :effect (and
                    (not (idle ?m))
                    (not (unused ?p))
                    (not (available ?b))
                    (increase (total-cost) 30)
                    (sawing-now ?b ?p ?m ?w ?surface large)))
  (:action do-saw-large-end
           :parameters (?b - board ?p - part ?m - saw ?w - awood
                           ?surface - surface) 
           :precondition (sawing-now ?b ?p ?m ?w ?surface large)
           :effect (and
                    (not (sawing-now ?b ?p ?m ?w ?surface large))
                    (idle ?m)
                    (available ?b)
                    (available ?p)
                    (wood ?p ?w)
                    (surface-condition ?p ?surface)
                    (colour ?p natural) 
                    (treatment ?p untreated)))
  )

;;;Engine

(defpackage :rl.c-api
  (:documentation "API to use C functions")
  (:use :cffi :cl-user)
  (:nicknames :api :sdl)
  (:shadowing-import-from :cffi :foreign-pointer)
  (:export :key-to-event :get-key-event :process-key
	   :render-present :render-clear
	   :cpos :color :rect :gramma
	   :x :y
	   :red :green :blue
	   :char :start :size
	   :draw-gramma :fill-cell
	   :draw-frame :draw-rectangle
	   :with-foreign-temporary))

(defpackage :rl.random
  (:documentation "Just random utils")
  (:nicknames :rnd)
  (:use :cl-user :cl)
  (:export :dices :dices-string :throw-dices :bernoulli :fbernoulli :interval))

(defpackage :rl.coordinates
  (:documentation "All related to geometry")
  (:nicknames :geom)
  (:use :cl-user :common-lisp)
  (:export :raw-line :cell-line :trace-line :cast-ray :doray :cell-find-if
	   :coordinates :pos
	   :rect :start :end :size
	   :x :y :make-pos :add :sub :make-rect :distance-point
	   :right-slice :left-slice :upper-slice :lower-slice :transpose :negate
	   :pref
	   :in-bound-p
	   :distance :pythagorean-distance
	   :neighbours :neighbours-delta
	   :doarea :rect-scale :rect-center
	   :dorectangle :ray-reduce
	   :compass-rose :make-compass-rose
	   :rose-north
	   :rose-west
	   :rose-south
	   :rose-east))

(defpackage :rl.user-interface
  (:use :common-lisp :cl-user :rl.coordinates)
  (:nicknames :rl.ui :ui)
  (:import-from :cl-user :def-symbol-map)
  (:export
   :static-gramma
   :color :make-color
   :gramma :gramma-code :make-gramma :augment-gramma :gramma-color :grammap
   :cell-color :layer-color
   :gui-string :make-gui-string :gui-string-p :gstr-length)
  (:intern :draw-map :draw :make-scrollable :draw-terrain :get-marked-list
   :draw-rectangle :draw-string :draw-gramma :draw-frame :draw-simple-gramma
   :chars))

(defpackage :rl.names
  (:use :cl :cl-user)
  (:export
   :make-names :names-plural :names-singular :names-concrete
   :get-plural-name :get-concrete-name :get-singular-name
   :get-full-name :get-name
   :get-description :get-gramma
   :name :description :gramma)
  (:import-from :ui :gramma))

(defpackage :rl.sound
  (:use :cl :cl-user)
  (:export :snd :sound-intensity :sound-message))

(defpackage :rl.combat
  (:use :cl :cl-user)
  (:export :dmg :damage-count :dodgesp
   :protection
   :damage :hit-check
	   :basic-damage
	   :calculate-attack))

(defpackage :rl.entity
  (:documentation "Entities existing on level")
  (:use :common-lisp :cl-user :rl.coordinates :rl.names :rl.combat)
  (:export
   :entity :proto-creature :corpse :item-stack :proto-trap :proto-actor
   :can-move-p 
   :get-pos :get-hp :get-max-hp :get-speed :get-damage
   :alivep :seesp
   :corpse-owner :spawn-corpse
   :get-gramma :get-name :get-description :get-full-name
   :get-weapon :get-damage :get-protection :get-memory :get-dodge-bonus
   :get-message-buffer
   :get-state :set-state
   :get-melee-damage
   :take-damage :decrease-health :die
   :perform-movement :on-position-change
   :make-creature)
  (:intern :report-death))

;;;Map & level
(defpackage :rl.terrain
  (:use :cl :cl-user :rl.names)
  (:export :obstaclep :solidp :blockedp
   :door :openp :open-door :close-door
   :attack-cell :close-door
   :destroyedp :interact
   :terrain :hit-terrain
   :map-cell :attack-cell
   :interact-with-cell :terrain-interact
   :terrain-name :terrain-gramma
   :terrain-decorator :decorated
   :define-terrain))

(defpackage :rl.map
  (:use :common-lisp :cl-user :rl.coordinates :rl.names)
  (:nicknames :map)
  (:export  :make-map-array :*map* :+size+ :+map-rect+ :in-map-bound-p :grant-on-map :map-array
   :initiate-map
	    :pos-gramma :pos-name :obstaclep :solidp :blockedp :litp
	    :door :openp :open-door :close-door
	    :search-terrain :attack-cell :destroyedp
   :decorate-terrain
	    :interact
   :pos-cell
	    :map-cell
   :shadowcast
	    :litp
   :fill-circle
	    :terrain :hit-terrain
	    :terrain-obstaclep :terrain-solidp :terrain-blockingp
   :terrain-interact
	    :terrain-name :terrain-gramma
	    :define-terrain :terrain-decorator
	    :decorated
	    :decorator-instance)
  (:intern :update-lights))

(defpackage :rl.light
  (:use :common-lisp :cl-user :rl.map :rl.coordinates)
  (:export 
   :replace-light
   :create-light
   :remove-light
   :update-lights)
  (:import-from :map :litp :update-lights))

(defpackage :rl.level
  (:use :common-lisp :cl-user :rl.coordinates :rl.map :rl.entity)
  (:nicknames :level)
  (:export :*actor* :get-entities :add-entity :remove-entity
	   :obstaclep :solidp
	   :do-entities
	   :pos-entities-alist
	   :get-entities-gramma
	   :lee-map :make-lee-map :lee :near-step :way-distance))

(defpackage :rl.generator
  (:use :common-lisp :cl-user :rl.coordinates :rl.level :rl.map)
  (:export :make-room-list :add-room :generate
	   :sector-tree :make-sector-tree
	   :vertical :horizontal
	   :tree-first-branch :tree-second-branch
	   :sector-north :sector-south
	   :sector-west :sector-east))

(defpackage :rl.perception
  (:use :common-lisp :cl-user :rl.coordinates :rl.entity :rl.ui :rl.map)
  (:nicknames :rl.fov)
  (:export :fov :fov-info :make-fov :update-fov :get-gramma :get-plan-gramma :visiblep :seenp :get-center :get-fov :fov-entity-positions :visible-entities
	   :vision-visiblep :fov-pos-description
	   :vision :fov-marks :mark-gramma)
  (:intern :make-mark :mark :fov-shadowcast)
  (:shadow :get-gramma :update-fov))

;;;Items
(defpackage :rl.item
  (:use :cl :cl-user :rl.entity :rl.names :rl.combat)
  (:export
   :item :melee :weapon :usable :armor
   :protection :get-protection
   :weight :condition
   :get-weight :get-condition
   :melee-speed :ranged-speed
   :melee-damage :get-melee-damage
   :ranged-damage :get-ranged-damage
   :same-item-p :stack-item :stack-count
   :merge-stacks :item-stack
   :make-item :make-stack :make-free-item
   :make-stack
   :free-item :copy-item
   :get-gramma :get-description))

(defpackage :rl.inventory
  (:use :cl :cl-user :rl.item)
  (:export :inventory-backpack :add-stack :take-stack :remove-stack
	   :get-gear :equip-slot :put-off-slot))

;;Other

(defpackage :rl.message
  (:use :cl :cl-user :rl.sound :rl.ui :rl.names :rl.entity)
  (:export :make-message-buffer :add-message
	   :msg :message-report :simple-message :simple-note
	   :message-buffer
   :get-printed :pop-unprinted
   :report-death
   :report-attack
   :report-hit
   :report-reflection)
  (:import-from :rl.entity :report-death))

(defpackage :rl.ui-lang
  (:use :rl.ui :cl :cl-user :rl.coordinates :rl.message)
  (:export
   :change-layer
   :define-panel
   :lookup-fov :make-lookup-fov :make-target-fov :fov-description :move-focus :target-fov :get-map-focus :next-target
   :field :rect :pos :str :line 
   :page :catalogue :text :plain
   :section
   :views :framed :background :standard-frame
   :call-handler :controller-let :controller :controller-body :control :control-scrollable :exit
   :handle
   :end-reached-p :last-page-p
   :scrollable :simple-menu :menu :alphabetic-menu :buffer :complex-menu
   :define-view :elem-view
   :string-select
   :view-instance :selected-description :instance
   :horizontal-split :vertical-split
   :text-source :sources-append :buffer-source)
  (:import-from :rl.ui :draw-rectangle :draw-string :draw-gramma :draw-frame :draw-simple-gramma :chars))

(defpackage :rl.event
  (:use :cl :cl-user :rl.entity :geom :rl.message)
  (:export :add-event :take-turn :event
	   :make-turn :make-update :make-thunk-event :update-entity
	   :process-events :exec :entity
	   :event-energy
	   :event :turn :action
	   :move-random! :move-accurate! :try-to-move!
	   :interact-with-cell!
	   :hit))

(defpackage :rl.state
  (:use :cl :cl-user :rl.event)
  (:export :state :make-state :standard-state :time-state :dead
	   :state-execute :state-duration :init-state))

(defpackage :rl.ai
  (:use :rl.entity :rl.state :rl.event :cl :cl-user)
  (:export :jump :continuate :go-next :ai-state
	   :action-execute :valid-action-p
	   :wander :move-to-point :move-dir :follov :fight))

(defpackage :rl.effects
  (:export :make-effect))

;;General package

(define-extension :rl.game (:rl.random :rl.coordinates :rl.map
				       :rl.level :rl.combat :rl.generator
			    :rl.perception :rl.item :rl.inventory :rl.event
				       :rl.entity :rl.state :rl.sound :rl.ai :rl.effects :rl.names)
  (:use :cl :cl-user)
  (:documentation "Package for game definitions")
  (:shadowing-import-from :rl.entity :get-gramma)
  (:export
   :add-items
   :add-item
   :add-creature
   :add-trap
   :add-effect
   :add-event
   :build-info))

(define-extension :roguelisp (:rl.game)
  (:documentation "General package. Contains all symbols from other packages")
  (:use)
  (:nicknames :rl :rlisp))

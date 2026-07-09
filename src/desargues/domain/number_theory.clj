(ns desargues.domain.number-theory
  "Value objects and entities for number theory visualizations.
   
   Records:
   - PrimeFactorization: A number and its prime factorization
   - DotGrid2D: 2D grid of dot positions
   - DotGrid3D: 3D grid of dot positions  
   - FactorizationGroup: A group of elements with color and depth
   - NestedFactorization: Complete factorization with grouping levels
   - VisualizationConfig: Configuration for visualization appearance"
  (:require [desargues.domain.number-theory-services :as svc]
            [desargues.domain.protocols :as p]))

;; =============================================================================
;; Value Objects (immutable, equality by value)
;; =============================================================================

(defrecord PrimeFactorization [n factors]
  ;; n: the original number
  ;; factors: map of {prime exponent}, e.g., {2 3, 3 1} for 24
  )

(defrecord DotGrid2D [n rows cols positions]
  ;; n: number of dots
  ;; rows, cols: grid dimensions
  ;; positions: seq of [x y 0] coordinates
  )

(defrecord DotGrid3D [n dims positions]
  ;; n: number of dots
  ;; dims: [x-dim y-dim z-dim]
  ;; positions: seq of [x y z] coordinates
  )

(defrecord FactorizationGroup [elements color depth label]
  ;; elements: vector of dot indices or nested FactorizationGroups
  ;; color: keyword like :blue, :red, or [r g b] vector
  ;; depth: nesting depth (0 = innermost)
  ;; label: optional label for the group
  )

(defrecord GroupingLevel [level-num groups description]
  ;; level-num: 0-indexed level number
  ;; groups: the grouping structure at this level
  ;; description: human-readable description
  )

(defrecord NestedFactorization [n factorization positions levels arrangement]
  ;; n: the original number
  ;; factorization: PrimeFactorization record
  ;; positions: seq of [x y z] dot positions
  ;; levels: vector of GroupingLevel records
  ;; arrangement: {:type :grid-2d/:grid-3d :dims [...]}
  )

(defrecord VisualizationConfig [colors spacing dot-radius border-width
                                animation-speed level-pause prefer-3d]
  ;; colors: vector of colors for nesting levels, e.g., [:blue :red :green]
  ;; spacing: distance between dots
  ;; dot-radius: radius of each dot
  ;; border-width: stroke width of grouping rectangles
  ;; animation-speed: animation duration multiplier
  ;; level-pause: pause duration between levels
  ;; prefer-3d: boolean, force 3D even for simple factorizations
  )

;; =============================================================================
;; Factory Functions
;; =============================================================================

(defn create-factorization
  "Create a PrimeFactorization from a number."
  [n]
  (let [{:keys [factors]} (svc/prime-factorize n)]
    (->PrimeFactorization n factors)))

(defn create-dot-grid-2d
  "Create a 2D dot grid for the given dimensions."
  [rows cols & {:keys [spacing] :or {spacing 0.5}}]
  (let [n (* rows cols)
        positions (vec (svc/compute-grid-positions-2d rows cols :spacing spacing))]
    (->DotGrid2D n rows cols positions)))

(defn create-dot-grid-3d
  "Create a 3D dot grid for the given dimensions."
  [x-dim y-dim z-dim & {:keys [spacing] :or {spacing 0.6}}]
  (let [n (* x-dim y-dim z-dim)
        positions (vec (svc/compute-grid-positions-3d x-dim y-dim z-dim :spacing spacing))]
    (->DotGrid3D n [x-dim y-dim z-dim] positions)))

(defn create-grouping-level
  "Create a GroupingLevel record."
  [level-num groups & {:keys [description]}]
  (let [desc (or description
                 (cond
                   (zero? level-num) "Individual dots"
                   (and (= (count groups) 1) (sequential? (first groups)))
                   "Final grouping (all dots)"
                   :else (str "Level " level-num ": " (count groups) " groups")))]
    (->GroupingLevel level-num groups desc)))

(defn create-nested-factorization
  "Create a complete NestedFactorization from a number.
   
   Options:
   - :spacing - dot spacing (default 0.5 for 2D, 0.6 for 3D)
   - :prefer-3d - force 3D arrangement"
  [n & {:keys [spacing prefer-3d]}]
  (let [factorization (create-factorization n)
        fact-map (svc/prime-factorize n)
        tree (svc/factorization-to-tree fact-map)
        arrangement-type (if prefer-3d
                           :grid-3d
                           (svc/optimal-arrangement fact-map))
        arrangement (assoc (:arrangement tree) :type arrangement-type)
        positions (vec (svc/compute-dot-positions tree :spacing (or spacing 0.5)))
        raw-levels (svc/build-grouping-levels fact-map)
        levels (vec (map-indexed create-grouping-level raw-levels))]
    (->NestedFactorization n factorization positions levels arrangement)))

(def default-config
  "Default visualization configuration."
  (->VisualizationConfig
   [:blue :red :green :yellow :purple] ; colors
   0.5 ; spacing
   0.1 ; dot-radius
   3 ; border-width
   1.0 ; animation-speed
   0.5 ; level-pause
   false)) ; prefer-3d

(defn create-config
  "Create a VisualizationConfig with overrides from defaults."
  [& {:keys [colors spacing dot-radius border-width
             animation-speed level-pause prefer-3d]
      :or {colors [:blue :red :green :yellow :purple]
           spacing 0.5
           dot-radius 0.1
           border-width 3
           animation-speed 1.0
           level-pause 0.5
           prefer-3d false}}]
  (->VisualizationConfig colors spacing dot-radius border-width
                         animation-speed level-pause prefer-3d))

;; =============================================================================
;; Utility Functions
;; =============================================================================

(defn factorization->latex
  "Get LaTeX representation of a factorization."
  [fact]
  (svc/factorization->latex {:n (:n fact) :factors (:factors fact)}))

(defn factorization->str
  "Get string representation of a factorization."
  [fact]
  (svc/factorization->str {:n (:n fact) :factors (:factors fact)}))

(defn level-count
  "Get the number of grouping levels in a nested factorization."
  [nested-fact]
  (count (:levels nested-fact)))

(defn get-level
  "Get a specific grouping level from a nested factorization."
  [nested-fact level-num]
  (get (:levels nested-fact) level-num))

(defn dots-at-level
  "Get the dot indices grouped at a specific level.
   Level 0 returns individual dot indices."
  [nested-fact level-num]
  (:groups (get-level nested-fact level-num)))

(defn is-2d?
  "Check if a nested factorization uses 2D arrangement."
  [nested-fact]
  (= :grid-2d (get-in nested-fact [:arrangement :type])))

(defn is-3d?
  "Check if a nested factorization uses 3D arrangement."
  [nested-fact]
  (= :grid-3d (get-in nested-fact [:arrangement :type])))

(defn grid-dimensions
  "Get the grid dimensions of a nested factorization."
  [nested-fact]
  (get-in nested-fact [:arrangement :dims]))

(defn color-for-level
  "Get the color for a specific nesting level from config."
  [config level-num]
  (let [colors (:colors config)
        idx (mod level-num (count colors))]
    (nth colors idx)))

(extend-protocol p/IFactorizable
  PrimeFactorization
  (prime-factors [this] (:factors this))
  (factorization-tree [this]
    (svc/factorization-to-tree {:n (:n this) :factors (:factors this)})))

(extend-protocol p/IDotArrangement
  DotGrid2D
  (to-dots [this] (:positions this))
  (dimensions [this] [(:rows this) (:cols this)])
  (arrangement-type [this] :grid-2d)
  DotGrid3D
  (to-dots [this] (:positions this))
  (dimensions [this] (:dims this))
  (arrangement-type [this] :grid-3d))

(extend-protocol p/INestedGrouping
  NestedFactorization
  (nesting-depth [this] (count (:levels this)))
  (children [this] (:levels this))
  (grouping-levels [this] (:levels this)))

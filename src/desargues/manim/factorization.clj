(ns desargues.manim.factorization
  "Manim bindings for factorization visualizations.
   
   Provides:
   - Dot grid creation (2D and 3D)
   - Grouping with colored rectangles
   - Nested grouping hierarchies
   - Animation helpers for building up factorizations"
  (:require [desargues.manim.core :as core]
            [desargues.manim.mobjects :as m]
            [desargues.manim.animations :as a]
            [desargues.domain.number-theory :as nt]
            [desargues.domain.number-theory-services :as nts]
            [libpython-clj2.python :as py]))

;; =============================================================================
;; Color Constants
;; =============================================================================

(def level-colors
  "Default colors for nesting levels (from inner to outer)."
  {:blue "#58C4DD"
   :red "#FC6255"
   :green "#83C167"
   :yellow "#FFFF00"
   :purple "#9A72AC"
   :teal "#5CD0B3"
   :orange "#FF862F"
   :pink "#FF69B4"})

(defn get-color
  "Get a Manim color from keyword or hex string."
  [color-key]
  (cond
    (keyword? color-key) (get level-colors color-key (core/get-constant "BLUE"))
    (string? color-key) color-key
    :else (core/get-constant "BLUE")))

(defn color-for-level
  "Get color for a specific nesting level."
  [level & {:keys [colors] :or {colors [:blue :red :green :yellow :purple]}}]
  (let [idx (mod level (count colors))]
    (get-color (nth colors idx))))

;; =============================================================================
;; Dot Creation
;; =============================================================================

(defn create-dot
  "Create a single dot at position.
   
   Options:
   - :radius - dot radius (default 0.1)
   - :color - dot color (default white)
   - :fill-opacity - fill opacity (default 1.0)"
  [position & {:keys [radius color fill-opacity]
               :or {radius 0.1 fill-opacity 1.0}}]
  (let [dot (m/dot :radius radius)]
    (m/move-to dot (vec position))
    (when color
      (m/set-fill dot (get-color color) :opacity fill-opacity))
    dot))

(defn create-dot-3d
  "Create a 3D sphere at position.
   
   Options:
   - :radius - sphere radius (default 0.08)
   - :color - sphere color"
  [position & {:keys [radius color]
               :or {radius 0.08}}]
  (let [sphere (m/sphere :radius radius)]
    (m/move-to sphere (vec position))
    (when color
      (m/set-color sphere (get-color color)))
    sphere))

;; =============================================================================
;; Dot Arrays and Grids
;; =============================================================================

(defn create-dot-array
  "Create n dots arranged in a line.
   
   Options:
   - :spacing - distance between dots (default 0.5)
   - :direction - :horizontal or :vertical (default :horizontal)
   - :color - dot color
   - :radius - dot radius"
  [n & {:keys [spacing direction color radius]
        :or {spacing 0.5 direction :horizontal radius 0.1}}]
  (let [positions (nts/compute-linear-positions n :spacing spacing :direction direction)
        dots (mapv #(create-dot % :radius radius :color color) positions)]
    (apply m/vgroup dots)))

(defn create-dot-grid-2d
  "Create a 2D grid of dots.
   
   Options:
   - :spacing - distance between dots (default 0.5)
   - :color - dot color
   - :radius - dot radius (default 0.1)"
  [rows cols & {:keys [spacing color radius]
                :or {spacing 0.5 radius 0.1}}]
  (let [positions (nts/compute-grid-positions-2d rows cols :spacing spacing)
        dots (mapv #(create-dot % :radius radius :color color) positions)]
    (apply m/vgroup dots)))

(defn create-dot-grid-3d
  "Create a 3D grid of spheres.
   
   Options:
   - :spacing - distance between spheres (default 0.6)
   - :color - sphere color
   - :radius - sphere radius (default 0.08)"
  [x-dim y-dim z-dim & {:keys [spacing color radius]
                        :or {spacing 0.6 radius 0.08}}]
  (let [positions (nts/compute-grid-positions-3d x-dim y-dim z-dim :spacing spacing)
        spheres (mapv #(create-dot-3d % :radius radius :color color) positions)]
    (apply m/vgroup spheres)))

(defn create-dot-grid
  "Create a grid of dots, dispatching on dimensionality.
   2D: (create-dot-grid rows cols & opts).
   3D: (create-dot-grid x y z :3d true & opts)."
  [& args]
  (let [[dims opts] (split-with number? args)
        opts-map (apply hash-map opts)]
    (if (:3d opts-map)
      (apply create-dot-grid-3d (concat dims (mapcat identity (dissoc opts-map :3d))))
      (apply create-dot-grid-2d (concat dims (mapcat identity opts-map))))))

(defn create-dots-for-number
  "Create dots arranged according to the number's factorization.
   
   - Primes: single horizontal line (no natural grouping)
   - Prime powers: 2D grid with p columns (shows grouping by p)
   - Multiple primes: 2D grid with dimensions based on factors
   
   Options:
   - :spacing - dot spacing
   - :color - dot color
   - :radius - dot radius
   - :prefer-3d - force 3D arrangement"
  [n & {:keys [spacing color radius prefer-3d]
        :or {spacing 0.5 radius 0.1 prefer-3d false}}]
  (let [fact (nts/prime-factorize n)
        arrangement (nts/optimal-arrangement fact)
        arr-type (if prefer-3d :grid-3d (:type arrangement))
        dims (:dims arrangement)]
    (case arr-type
      :linear
      (create-dot-array n :spacing spacing :color color :radius radius)

      :grid-2d
      (let [[rows cols] dims]
        (create-dot-grid-2d rows cols :spacing spacing :color color :radius radius))

      :grid-3d
      (let [[x y z] (take 3 (concat dims (repeat 1)))]
        (create-dot-grid-3d x y z
                            :spacing (or spacing 0.6)
                            :color color
                            :radius (or radius 0.08)))
      ;; Default fallback
      (create-dot-array n :spacing spacing :color color :radius radius))))

;; =============================================================================
;; Grouping with Rectangles
;; =============================================================================

(defn create-grouping-rectangle
  "Create a colored rectangle around a VGroup of dots.
   
   Options:
   - :color - rectangle stroke color (default blue)
   - :buff - padding around the group (default 0.15)
   - :stroke-width - line width (default 3)
   - :corner-radius - rounded corners (default 0.1)"
  [vgroup & {:keys [color buff stroke-width corner-radius]
             :or {color :blue buff 0.15 stroke-width 3 corner-radius 0.1}}]
  (let [rect (m/surrounding-rectangle vgroup
                                      :buff buff
                                      :corner_radius corner-radius)]
    (m/set-stroke rect :color (get-color color) :width stroke-width)
    (m/set-fill rect (get-color color) :opacity 0.0)
    rect))

(defn group-dots-with-rectangle
  "Group dots at given indices with a colored rectangle.
   Returns {:group VGroup :rectangle SurroundingRectangle}"
  [all-dots indices & {:keys [color buff] :or {color :blue buff 0.15}}]
  (let [submobjects (py/get-attr all-dots "submobjects")
        selected-dots (mapv #(nth submobjects %) indices)
        group (apply m/vgroup selected-dots)
        rect (create-grouping-rectangle group :color color :buff buff)]
    {:group group :rectangle rect}))

;; =============================================================================
;; Nested Grouping Hierarchies
;; =============================================================================

(defn- flatten-indices
  "Recursively flatten nested indices to get all leaf indices."
  [nested]
  (if (number? nested)
    [nested]
    (mapcat flatten-indices nested)))

(defn create-level-rectangles
  "Create rectangles for a single grouping level.
   
   The buff (padding) increases with level-num to create non-overlapping shells.
   - Level 0 (innermost): tight to dots (base-buff)
   - Each subsequent level adds buff-increment for visual separation
   
   Returns vector of {:indices [...] :rectangle SurroundingRectangle}"
  [all-dots level-groups color level-num & {:keys [base-buff buff-increment]
                                            :or {base-buff 0.06 buff-increment 0.18}}]
  (let [submobjects (vec (py/get-attr all-dots "submobjects"))
        ;; Increase buff for each level to create concentric shells
        ;; Level 0: 0.06, Level 1: 0.24, Level 2: 0.42, etc.
        level-buff (+ base-buff (* level-num buff-increment))]
    (vec
     (for [group level-groups
           :let [indices (flatten-indices group)]
           :when (> (count indices) 1)] ; Only create rect if more than one dot
       (let [selected-dots (mapv #(nth submobjects %) indices)
             group-vg (apply m/vgroup selected-dots)
             rect (create-grouping-rectangle group-vg :color color :buff level-buff)]
         {:indices indices :rectangle rect})))))

(defn create-factorization-hierarchy
  "Create the complete visual hierarchy for a prime factorization.
   
   Returns:
   {:n n
    :dots VGroup of all dots
    :levels [{:level-num int
              :groups [{:indices [...] :rectangle rect}]
              :color color}]
    :arrangement {:type :grid-2d/:grid-3d :dims [...]}}"
  [n & {:keys [colors spacing radius prefer-3d]
        :or {colors [:blue :red :green :yellow :purple]
             spacing 0.5
             radius 0.1
             prefer-3d false}}]
  (let [nested-fact (nt/create-nested-factorization n :spacing spacing :prefer-3d prefer-3d)
        dots (create-dots-for-number n :spacing spacing :radius radius :prefer-3d prefer-3d)
        levels (mapv
                (fn [level-record idx]
                  (let [groups (:groups level-record)
                        color (nth colors (mod idx (count colors)))]
                    {:level-num idx
                     :description (:description level-record)
                     :groups (create-level-rectangles dots groups color idx)
                     :color color}))
                (rest (:levels nested-fact)) ; Skip level 0 (individual dots)
                (range))]
    {:n n
     :dots dots
     :levels levels
     :arrangement (:arrangement nested-fact)
     :factorization (:factorization nested-fact)}))

;; =============================================================================
;; 3D Grouping Support
;; =============================================================================

(defn create-3d-grouping-box
  "Create a translucent 3D box around a group of spheres.
   
   Options:
   - :color - box color
   - :opacity - fill opacity (default 0.15)
   - :stroke-opacity - edge opacity (default 0.5)"
  [group & {:keys [color opacity stroke-opacity]
            :or {opacity 0.15 stroke-opacity 0.5}}]
  ;; For 3D, we use a Prism or Cube scaled to fit the group
  (let [;; Get bounding box of the group
        center (m/get-center group)
        width (+ (m/get-width group) 0.2)
        height (+ (m/get-height group) 0.2)
        cube (m/cube :side_length (max width height))]
    (m/move-to cube center)
    (when color
      (m/set-fill cube (get-color color) :opacity opacity)
      (m/set-stroke cube :color (get-color color) :opacity stroke-opacity))
    cube))

;; =============================================================================
;; Animation Helpers
;; =============================================================================

(defn animate-dots-creation
  "Animate the creation of dots with staggered effect.
   
   Options:
   - :lag-ratio - stagger between dots (default 0.1)
   - :run-time - total animation time"
  [dots-vgroup & {:keys [lag-ratio run-time]
                  :or {lag-ratio 0.1}}]
  (let [submobjects (vec (py/get-attr dots-vgroup "submobjects"))
        animations (mapv a/grow-from-center submobjects)]
    (apply a/lagged-start (concat animations [:lag_ratio lag-ratio]))))

(defn animate-rectangle-creation
  "Animate the creation of a grouping rectangle."
  [rectangle & {:keys [run-time] :or {run-time 0.5}}]
  (a/create rectangle :run_time run-time))

(defn animate-grouping-level
  "Animate the formation of all rectangles at a grouping level.
   
   Options:
   - :run-time - time per rectangle (default 0.5)
   - :lag-ratio - stagger between rectangles (default 0.3)"
  [level & {:keys [run-time lag-ratio]
            :or {run-time 0.5 lag-ratio 0.3}}]
  (let [rectangles (mapv :rectangle (:groups level))
        animations (mapv #(a/create % :run_time run-time) rectangles)]
    (if (seq animations)
      (apply a/lagged-start (concat animations [:lag_ratio lag-ratio]))
      nil)))

(defn animate-factorization-buildup
  "Create animation sequence that builds up all levels.
   Returns a vector of animations to be played sequentially.
   
   Options:
   - :dots-lag - stagger for dot creation (default 0.05)
   - :level-run-time - time per level (default 1.0)
   - :level-lag - stagger within level (default 0.2)"
  [hierarchy & {:keys [dots-lag level-run-time level-lag]
                :or {dots-lag 0.05 level-run-time 1.0 level-lag 0.2}}]
  (let [dots (:dots hierarchy)
        levels (:levels hierarchy)
        dots-anim (animate-dots-creation dots :lag-ratio dots-lag)
        level-anims (keep #(animate-grouping-level %
                                                   :run-time level-run-time
                                                   :lag-ratio level-lag)
                          levels)]
    (vec (cons dots-anim level-anims))))

(defn build-level-equation
  "Build the intermediate equation string for a given grouping level.
   Uses nested parentheses to show grouping structure.
   
   For 8 = 2³:
   - Level 0 (4 pairs): '8 = 2 \\cdot (4)'
   - Level 1 (2 groups of pairs): '8 = 2 \\cdot (2 \\cdot (2))'
   - Level 2 (1 final group): '8 = 2 \\cdot (2 \\cdot (2 \\cdot (1))) = 2^{3}'
   
   For 12 = 2² × 3:
   - Level 0: '12 = 2 \\cdot (6)'
   - Level 1: '12 = 2 \\cdot (2 \\cdot (3))'
   - Level 2: '12 = 2 \\cdot (2 \\cdot (3 \\cdot (1))) = 2^{2} \\times 3'"
  [n factors level-num]
  (let [sorted-primes (sort (keys factors))
        total-levels (dec (apply + (vals factors)))] ;; levels = total factors - 1
    (cond
      ;; Simple case: single prime power (like 8 = 2³)
      (= (count factors) 1)
      (let [[p e] (first factors)
            completed-factors (inc level-num)
            remaining (- e completed-factors)
            remaining-val (long (Math/pow p remaining))]
        (if (<= remaining 0)
          ;; Final level - show full factorization with nested form
          (let [nested (reduce (fn [inner _] (str p " \\cdot (" inner ")"))
                               "1"
                               (range e))]
            (str n " = " nested " = " p "^{" e "}"))
          ;; Intermediate - show nested parentheses
          (let [nested (reduce (fn [inner _] (str p " \\cdot (" inner ")"))
                               (str remaining-val)
                               (range completed-factors))]
            (str n " = " nested))))

      ;; Multiple primes - build progressively with nesting
      :else
      (let [;; Get all prime factors in order
            all-primes (vec (mapcat (fn [[p e]] (repeat e p)) (sort factors)))
            completed (inc level-num)
            revealed (take completed all-primes)
            remaining (drop completed all-primes)
            remaining-val (if (empty? remaining) 1 (reduce * 1 remaining))]
        (if (empty? remaining)
          ;; Final - show nested form and compact form
          (let [nested (reduce (fn [inner p] (str p " \\cdot (" inner ")"))
                               "1"
                               (reverse all-primes))]
            (str n " = " nested " = " (nts/factorization->latex {:n n :factors factors})))
          ;; Intermediate - nested parentheses
          (let [nested (reduce (fn [inner p] (str p " \\cdot (" inner ")"))
                               (str remaining-val)
                               (reverse (vec revealed)))]
            (str n " = " nested)))))))

(defn animate-level-highlight
  "Animate highlighting a specific nesting level (pulse effect).
   
   Options:
   - :scale-factor - pulse scale (default 1.1)
   - :run-time - animation time (default 0.3)"
  [hierarchy level-num & {:keys [scale-factor run-time]
                          :or {scale-factor 1.1 run-time 0.3}}]
  (let [level (nth (:levels hierarchy) level-num nil)]
    (when level
      (let [rectangles (mapv :rectangle (:groups level))
            animations (mapv #(a/indicate % :scale_factor scale-factor :run_time run-time)
                             rectangles)]
        (apply a/animation-group animations)))))

;; =============================================================================
;; High-Level Scene Helpers
;; =============================================================================

(defn add-hierarchy-to-scene!
  "Add all elements of a factorization hierarchy to a scene."
  [scene hierarchy]
  (core/add! scene (:dots hierarchy))
  (doseq [level (:levels hierarchy)]
    (doseq [{:keys [rectangle]} (:groups level)]
      (core/add! scene rectangle))))

(defn play-factorization-buildup!
  "Play the complete factorization buildup animation in a scene.
   
   Options:
   - :dots-lag - stagger for dots
   - :level-pause - pause between levels (default 0.5)
   - :show-equations - show intermediate equations (default false)
   - :equation-position - where to show equations (default DOWN)"
  [scene hierarchy & {:keys [dots-lag level-pause show-equations equation-position]
                      :or {dots-lag 0.05 level-pause 0.5 show-equations false}}]
  (let [dots (:dots hierarchy)
        levels (:levels hierarchy)
        n (:n hierarchy)
        fact (:factorization hierarchy)]
    ;; Create dots
    (core/play! scene (animate-dots-creation dots :lag-ratio dots-lag))
    (core/wait! scene 0.5)
    ;; Build up each level with optional equations
    (doseq [[idx level] (map-indexed vector levels)]
      (when-let [anim (animate-grouping-level level)]
        (core/play! scene anim)
        (when show-equations
          ;; Show intermediate equation for this level
          (let [eq-str (build-level-equation n (:factors fact) idx)
                eq-tex (m/math-tex eq-str)]
            (m/to-edge eq-tex (or equation-position (core/get-constant "DOWN")) :buff 0.5)
            (core/play! scene (a/fade-in eq-tex))
            (core/wait! scene level-pause)
            (core/play! scene (a/fade-out eq-tex))))
        (when-not show-equations
          (core/wait! scene level-pause))))))

(defn create-factorization-scene
  "Create a complete scene showing the factorization of n.
   
   Options:
   - :title - show title with equation (default true)
   - :colors - level colors
   - :prefer-3d - force 3D
   - :show-equations - show intermediate equations at each level (default true)
   - :quality - render quality"
  [n & {:keys [title colors prefer-3d show-equations quality]
        :or {title true
             colors [:blue :red :green :yellow :purple]
             prefer-3d false
             show-equations true
             quality "medium_quality"}}]
  (let [hierarchy (create-factorization-hierarchy n :colors colors :prefer-3d prefer-3d)
        fact (:factorization hierarchy)
        latex-str (nt/factorization->latex fact)]
    (core/create-scene-class
     (str "Factorization" n)
     (fn [self]
       (when title
         (let [title-tex (m/math-tex latex-str)]
           (m/to-edge title-tex (core/get-constant "UP"))
           (core/add! self title-tex)
           (core/play! self (a/write title-tex))
           (core/wait! self 0.5)))
       (play-factorization-buildup! self hierarchy
                                    :show-equations show-equations
                                    :level-pause 1.0)
       (core/wait! self 2)))))

;; =============================================================================
;; Convenience Functions
;; =============================================================================

(defn visualize-factorization!
  "One-liner to visualize a number's factorization.
   
   Options:
   - :colors - level colors
   - :prefer-3d - force 3D
   - :show-title - show equation (default true)
   - :quality - render quality"
  [n & {:keys [colors prefer-3d show-title quality]
        :or {colors [:blue :red :green :yellow :purple]
             prefer-3d false
             show-title true
             quality "medium_quality"}}]
  (let [scene-class (create-factorization-scene n
                                                :title show-title
                                                :colors colors
                                                :prefer-3d prefer-3d
                                                :show-equations true
                                                :quality quality)
        scene (scene-class)] ;; Instantiate the class
    (core/render-scene! scene)))

(defn factorization-mobjects
  "Get all mobjects for a factorization without rendering.
   Useful for composing into larger scenes.
   
   Returns {:dots VGroup :rectangles [rects by level] :title MathTex}"
  [n & {:keys [colors prefer-3d show-title]
        :or {colors [:blue :red :green :yellow :purple]
             prefer-3d false
             show-title true}}]
  (let [hierarchy (create-factorization-hierarchy n :colors colors :prefer-3d prefer-3d)
        fact (:factorization hierarchy)
        rectangles-by-level (mapv (fn [level]
                                    (mapv :rectangle (:groups level)))
                                  (:levels hierarchy))
        title (when show-title
                (m/math-tex (nt/factorization->latex fact)))]
    {:dots (:dots hierarchy)
     :rectangles rectangles-by-level
     :title title
     :hierarchy hierarchy}))

(defn render-multiple-examples!
  "Render factorization videos for multiple numbers.
   
   Options:
   - :numbers - vector of numbers to visualize (default [7 8 12 16 24])
   - :colors - level colors
   - :quality - render quality"
  [& {:keys [numbers colors quality]
      :or {numbers [7 8 12 16 24]
           colors [:blue :red :green :yellow :purple]
           quality "medium_quality"}}]
  (doseq [n numbers]
    (println (str "\n=== Rendering " n " ==="))
    (let [fact (nts/prime-factorize n)
          arrangement (nts/optimal-arrangement fact)]
      (println "Factorization:" (nts/factorization->str fact))
      (println "Arrangement:" arrangement))
    (let [scene-class (create-factorization-scene n
                                                  :title true
                                                  :colors colors
                                                  :show-equations true)
          scene-instance (scene-class)
          output-name (str "Factorization" n)]
      (core/render-scene! scene-instance {:output-file output-name})))
  (println "\n=== All renderings complete! ==="))

(ns desargues.domain.number-theory-services
  "Domain services for number theory computations.
   
   Pure functions for:
   - Prime factorization
   - Factorization tree structures
   - Dot position computation
   - Grouping level generation")

;; =============================================================================
;; Prime Factorization
;; =============================================================================

(defn prime-factorize
  "Compute prime factorization of n.
   Returns {:n n :factors {prime exponent ...}}
   
   Example: (prime-factorize 24) => {:n 24 :factors {2 3, 3 1}}"
  [n]
  {:pre [(pos-int? n)]}
  (let [original-n n]
    (loop [n n
           factor 2
           factors {}]
      (cond
        (= n 1) {:n original-n :factors factors}
        (> (* factor factor) n) {:n original-n
                                 :factors (if (> n 1)
                                            (update factors n (fnil inc 0))
                                            factors)}
        (zero? (mod n factor)) (recur (quot n factor)
                                      factor
                                      (update factors factor (fnil inc 0)))
        :else (recur n (if (= factor 2) 3 (+ factor 2)) factors)))))

(defn factorization->str
  "Convert factorization to readable string.
   Example: {:n 24 :factors {2 3, 3 1}} => \"24 = 2^3 × 3\""
  [{:keys [n factors]}]
  (let [terms (->> factors
                   (sort-by key)
                   (map (fn [[p e]]
                          (if (= e 1)
                            (str p)
                            (str p "^" e)))))]
    (str n " = " (clojure.string/join " × " terms))))

(defn factorization->latex
  "Convert factorization to LaTeX string.
   Example: {:n 24 :factors {2 3, 3 1}} => \"24 = 2^{3} \\times 3\""
  [{:keys [n factors]}]
  (let [terms (->> factors
                   (sort-by key)
                   (map (fn [[p e]]
                          (if (= e 1)
                            (str p)
                            (str p "^{" e "}")))))]
    (str n " = " (clojure.string/join " \\times " terms))))

;; =============================================================================
;; Factorization Tree Structure
;; =============================================================================

(defn- build-tree-for-power
  "Build nested grouping structure for p^e.
   Returns a tree where each level groups by p.
   
   Example for 2^3 (8 dots):
   Level 0: 8 individual atoms (indices 0-7)
   Level 1: 4 groups of 2 [[0 1] [2 3] [4 5] [6 7]]
   Level 2: 2 groups of 2 groups [[[0 1] [2 3]] [[4 5] [6 7]]]
   Level 3: 1 group of 2 groups of 2 groups"
  [p e start-idx]
  (let [total (long (Math/pow p e))
        atoms (vec (range start-idx (+ start-idx total)))]
    (loop [level 0
           groups atoms]
      (if (>= level e)
        {:atoms atoms
         :levels (vec (take (inc e) (iterate #(partition p %) atoms)))}
        (recur (inc level)
               (vec (partition p groups)))))))

(defn factorization-to-tree
  "Convert factorization to nested grouping structure.
   
   For n = p1^e1 × p2^e2, we create a structure where:
   - Each prime power forms its own grouping hierarchy
   - The overall structure combines these hierarchies
   
   Returns {:n n 
            :total-dots n
            :prime-trees {p {:exponent e :tree tree-structure}}
            :arrangement {:type :grid-2d/:grid-3d :dims [...]}}
   
   A :grid-3d arrangement has exactly three dims whose product is n.
   
   Example for 12 = 2^2 × 3:
   - 4 dots for 2^2, grouped as 2 pairs
   - 3 columns of these groups (for factor of 3)
   - Total: 12 dots in 4×3 arrangement"
  [{:keys [n factors] :as factorization}]
  (let [sorted-factors (sort-by key factors)
        prime-trees (into {}
                          (map (fn [[p e]]
                                 [p {:exponent e
                                     :power (long (Math/pow p e))
                                     :tree (build-tree-for-power p e 0)}])
                               sorted-factors))
        num-primes (count factors)
        arrangement (cond
                      (= num-primes 0) {:type :single :dims [1]}
                      (= num-primes 1) (let [[p e] (first factors)]
                                         (if (<= e 2)
                                           {:type :grid-2d :dims [(long (Math/pow p e)) 1]}
                                           {:type :grid-3d :dims [p p (long (Math/pow p (- e 2)))]}))
                      (= num-primes 2) (let [[[p1 e1] [p2 e2]] (vec sorted-factors)]
                                         {:type :grid-2d
                                          :dims [(long (Math/pow p1 e1))
                                                 (long (Math/pow p2 e2))]})
                      :else (let [[a b & more] (mapv (fn [[p e]] (long (Math/pow p e))) sorted-factors)]
                              {:type :grid-3d :dims [a b (reduce * more)]}))]
    {:n n
     :factors factors
     :total-dots n
     :prime-trees prime-trees
     :arrangement arrangement}))

;; =============================================================================
;; Dot Position Computation
;; =============================================================================

(defn compute-grid-positions-2d
  "Compute 2D positions for a rows × cols grid of dots.
   Centers the grid at origin.
   
   Options:
   - :spacing - distance between dot centers (default 0.5)"
  [rows cols & {:keys [spacing] :or {spacing 0.5}}]
  (let [x-offset (* (dec cols) spacing 0.5)
        y-offset (* (dec rows) spacing 0.5)]
    (for [row (range rows)
          col (range cols)]
      [(- (* col spacing) x-offset)
       (- y-offset (* row spacing))
       0])))

(defn compute-grid-positions-3d
  "Compute 3D positions for a x × y × z grid of spheres.
   Centers the grid at origin.
   
   Options:
   - :spacing - distance between sphere centers (default 0.6)"
  [x-dim y-dim z-dim & {:keys [spacing] :or {spacing 0.6}}]
  (let [x-offset (* (dec x-dim) spacing 0.5)
        y-offset (* (dec y-dim) spacing 0.5)
        z-offset (* (dec z-dim) spacing 0.5)]
    (for [z (range z-dim)
          y (range y-dim)
          x (range x-dim)]
      [(- (* x spacing) x-offset)
       (- (* y spacing) y-offset)
       (- (* z spacing) z-offset)])))

(defn compute-linear-positions
  "Compute positions for n dots in a line.
   Centers the line at origin."
  [n & {:keys [spacing direction] :or {spacing 0.5 direction :horizontal}}]
  (let [offset (* (dec n) spacing 0.5)]
    (for [i (range n)]
      (case direction
        :horizontal [(- (* i spacing) offset) 0 0]
        :vertical [0 (- offset (* i spacing)) 0]))))

(defn compute-dot-positions
  "Compute dot positions based on factorization structure.
   
   Uses the arrangement type from factorization-to-tree to determine layout."
  [{:keys [n arrangement] :as tree} & {:keys [spacing] :or {spacing 0.5}}]
  (let [{:keys [type dims]} arrangement]
    (case type
      :single [[0 0 0]]
      :grid-2d (let [[rows cols] (if (= (count dims) 2)
                                   dims
                                   [(first dims) 1])]
                 (vec (compute-grid-positions-2d rows cols :spacing spacing)))
      :grid-3d (let [[x y z] (take 3 (concat dims (repeat 1)))]
                 (vec (compute-grid-positions-3d x y z :spacing spacing)))
      ;; Default to linear
      (vec (compute-linear-positions n :spacing spacing)))))

;; =============================================================================
;; Grouping Level Generation
;; =============================================================================

(defn- partition-indices
  "Partition a sequence of indices into groups of size n."
  [n indices]
  (vec (partition n indices)))

(defn build-grouping-levels
  "Build the sequence of grouping levels for visualization.
   
   For 8 = 2^3:
   Level 0: [0 1 2 3 4 5 6 7] (8 individual dots)
   Level 1: [[0 1] [2 3] [4 5] [6 7]] (4 pairs)
   Level 2: [[[0 1] [2 3]] [[4 5] [6 7]]] (2 groups of 2 pairs)
   Level 3: [[[[0 1] [2 3]] [[4 5] [6 7]]]] (1 group of everything)
   
   Returns vector of levels, each level contains the grouping structure."
  [{:keys [n factors] :as factorization}]
  (if (= n 1)
    [[[0]]]
    (let [sorted-factors (sort-by key factors)
          ;; Start with individual dot indices
          initial (vec (range n))
          ;; Build levels by progressively grouping
          levels (loop [levels [initial]
                        current initial
                        remaining-factors sorted-factors]
                   (if (empty? remaining-factors)
                     levels
                     (let [[p e] (first remaining-factors)
                           ;; Apply e levels of grouping by p
                           new-levels (loop [lvls levels
                                             curr current
                                             exp e]
                                        (if (zero? exp)
                                          lvls
                                          (let [grouped (vec (map vec (partition p curr)))]
                                            (recur (conj lvls grouped)
                                                   grouped
                                                   (dec exp)))))]
                       (recur new-levels
                              (last new-levels)
                              (rest remaining-factors)))))]
      levels)))

(defn grouping-level-description
  "Generate human-readable description of a grouping level."
  [level-num total-levels group-sizes]
  (cond
    (zero? level-num) "Individual dots"
    (= level-num (dec total-levels)) "Final grouping (all dots)"
    :else (str "Level " level-num ": " (count group-sizes) " groups")))

;; =============================================================================
;; Optimal Arrangement Selection
;; =============================================================================

(defn optimal-arrangement
  "Determine optimal arrangement based on factorization.
   
   - 1 (no prime factors): a single dot
   - Primes: linear (a 2D grid would imply grouping that doesn't exist)
   - Prime powers: 2D grid showing the grouping structure
   - Multiple primes: 2D or 3D grid based on factors
   
   For 2D grids with multiple primes, the smallest prime's power goes in columns
   so that row-major grouping by 2 creates horizontal pairs. A 3D grid's third
   dimension carries every remaining prime power, so the dims always multiply
   to n.
   
   Returns {:type :single/:linear/:grid-2d/:grid-3d :dims [rows cols ...]}"
  [{:keys [factors n] :as factorization}]
  (let [num-primes (count factors)
        total-factors (apply + (vals factors))
        sorted-factors (sort-by key factors)]
    (cond
      (zero? num-primes)
      {:type :single :dims [1]}

      ;; Prime number - must be linear (no natural grouping)
      (and (= num-primes 1) (= (first (vals factors)) 1))
      {:type :linear :dims [n]}

      ;; Prime power (like 8 = 2³) - arrange as rows of p
      (= num-primes 1)
      (let [[p e] (first factors)
            ;; Arrange as p columns, p^(e-1) rows
            cols p
            rows (long (Math/pow p (dec e)))]
        {:type :grid-2d :dims [rows cols]})

      ;; Two distinct primes - natural 2D grid
      ;; Put the smallest prime power in columns for row-major grouping
      (= num-primes 2)
      (let [[[p1 e1] [p2 e2]] (vec sorted-factors)
            ;; p1 is the smallest prime, put p1^e1 in COLUMNS
            ;; so grouping by 2s creates horizontal pairs within rows
            cols (long (Math/pow p1 e1))
            rows (long (Math/pow p2 e2))]
        {:type :grid-2d :dims [rows cols]})

      ;; Three or more primes - 3D arrangement; the rest fold into the last axis
      :else
      (let [[a b & more] (mapv (fn [[p e]] (long (Math/pow p e))) sorted-factors)]
        {:type :grid-3d :dims [a b (reduce * more)]}))))

;; =============================================================================
;; Utility Functions
;; =============================================================================

(defn is-prime?
  "Check if n is prime."
  [n]
  (and (> n 1)
       (let [factors (:factors (prime-factorize n))]
         (and (= (count factors) 1)
              (= (first (vals factors)) 1)))))

(defn is-prime-power?
  "Check if n is a power of a prime (p^k for k ≥ 1)."
  [n]
  (let [{:keys [factors]} (prime-factorize n)]
    (= (count factors) 1)))

(defn prime-base
  "If n is a prime power, return the base prime. Otherwise nil."
  [n]
  (let [{:keys [factors]} (prime-factorize n)]
    (when (= (count factors) 1)
      (first (keys factors)))))

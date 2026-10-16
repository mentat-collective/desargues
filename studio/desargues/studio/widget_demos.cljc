(ns desargues.studio.widget-demos
  "Demos for the widget kinds that are not sliders (desargues.player.controls):
   each is an ordinary parametric construct (fn [params] (fn [stage] ...)) on
   the desargues.scene facade, so it still renders an mp4, and its controls
   are buttons, a select, a text field and a toggle.

     :turing  a Turing machine (the 3-state busy beaver) driven by Step, Run
              and Reset buttons; params hold only the step count.
     :kepler  the five Platonic solids of Kepler's Mysterium Cosmographicum:
              a select picks the solid, a slider turns it, a toggle shows its
              inscribed and circumscribed spheres.
     :goedel  Goedel numbering: a text field takes a formula in Nagel and
              Newman's alphabet; a symbol outside it is refused and the
              message shows beside the field.

   The registry `demos` has the shape of desargues.studio.objects-demos/demos,
   so the JavaFX stage (studio/desargues/studio/repl.clj) and the browser page
   (browser/desargues/browser/main.cljs) open them the same way. Host-free
   .cljc."
  (:require [clojure.string :as str]
            [desargues.player.controls :as controls]
            [desargues.scene :as s]))

(defn- title! [stage txt]
  (s/play! stage (s/appear (-> (s/tex txt :font-size 40 :color :white) (s/move-to [0 3.5]))
                           :run-time 0.5)))

(defn- show-all!
  "Bring every shape in `shapes` in at once, then hold."
  [stage shapes]
  (s/play! stage (s/together (mapv #(s/appear % :run-time 0.4) shapes)))
  (s/hold! stage 1))

(defn- fixed3 [x] (/ (Math/round (* 1000.0 x)) 1000.0))

;; ── Turing machine: Step / Run / Reset buttons ───────────────────────────────

(def busy-beaver
  "The 3-state, 2-symbol busy beaver: [state read] -> [write move next]. It
   halts after 14 steps with six 1s on the tape."
  {[:a 0] [1  1 :b]  [:a 1] [1  1 :halt]
   [:b 0] [0  1 :c]  [:b 1] [1  1 :b]
   [:c 0] [1 -1 :c]  [:c 1] [1 -1 :a]})

(def machine-start {:tape {} :head 0 :state :a :step 0})

(defn tm-step
  "One step of machine `m` under `rules`; a halted machine stays put."
  [rules {:keys [tape head state] :as m}]
  (if (= :halt state)
    m
    (let [[w mv nx] (rules [state (get tape head 0)])]
      (-> m
          (assoc-in [:tape head] w)
          (update :head + mv)
          (assoc :state nx)
          (update :step inc)))))

(defn run-machine
  "Machine `rules` after `n` steps from a blank tape (or once it halted)."
  [rules n]
  (nth (iterate (partial tm-step rules) machine-start) (max 0 n)))

(defn halting-steps
  "How many steps `rules` takes to halt from a blank tape, or nil when it
   runs past `limit`."
  [rules limit]
  (->> (iterate (partial tm-step rules) machine-start)
       (take (inc limit))
       (some #(when (= :halt (:state %)) (:step %)))))

(def ^:private bb-halt (halting-steps busy-beaver 1000))

(def turing-controls
  "Three buttons over one param, :steps. Step advances one step, Run jumps to
   the halt, Reset is a preset back to the blank tape."
  [{:id :step  :kind :button :label "Step"
    :on-press (fn [ps] (update ps :steps #(min bb-halt (inc (or % 0)))))}
   {:id :run   :kind :button :label "Run to halt"
    :on-press (fn [ps] (assoc ps :steps bb-halt))}
   {:id :reset :kind :button :label "Reset" :set {:steps 0}}])

(defn- rule-text [rules]
  (str/join "   " (for [st [:a :b :c] r [0 1]
                        :let [[w mv nx] (rules [st r])]]
                    (str (str/upper-case (name st)) r " -> " w (if (pos? mv) "R" "L")
                         (if (= :halt nx) "H" (str/upper-case (name nx)))))))

(defn turing-demo
  "The busy beaver after `:steps` steps: twelve tape cells, the head under
   its cell, the state and the step count."
  [{:keys [steps] :or {steps 0}}]
  (let [{:keys [tape head state step]} (run-machine busy-beaver (or steps 0))
        cells  (range -5 7)
        x-of   #(- (* 1.0 %) 0.5)
        halted (= :halt state)]
    (fn [stage]
      (title! stage "\\text{Turing machine: the 3-state busy beaver}")
      (show-all!
       stage
       (concat
        (for [i cells
              :let [one? (= 1 (get tape i 0))]]
          (cond-> (-> (s/rectangle :width 0.9 :height 0.9 :color :white)
                      (s/move-to [(x-of i) 0.8]))
            one? (s/fill! :gold :opacity 0.35)))
        (for [i cells]
          (-> (s/text (str (get tape i 0)) :font-size 36 :color :white)
              (s/move-to [(x-of i) 0.8])))
        [(-> (s/path [[(- (x-of head) 0.25) -0.2] [(+ (x-of head) 0.25) -0.2] [(x-of head) 0.25]]
                     :closed? true :color :red :width 3)
             (s/fill! :red :opacity 0.8))
         (-> (s/text (if halted "halted" (str "state " (str/upper-case (name state))))
                     :font-size 30 :color (if halted :green :red))
             (s/move-to [(x-of head) -0.7]))
         (-> (s/text (str "step " step " of " bb-halt "    1s on the tape: "
                          (count (filter #{1} (vals tape))))
                     :font-size 32 :color :white)
             (s/move-to [0 -1.8]))
         (-> (s/text (rule-text busy-beaver) :font-size 24 :color :grey)
             (s/move-to [0 -2.8]))])))))

;; ── Kepler's solids: select, slider, toggle ──────────────────────────────────

(def ^:private phi (/ (+ 1.0 (Math/sqrt 5.0)) 2.0))

(defn- cyclic
  "The three cyclic permutations of [a b c]."
  [[a b c]]
  [[a b c] [b c a] [c a b]])

(defn- signs
  "Every sign choice of the non-zero coordinates of `v`."
  [v]
  (reduce (fn [acc i]
            (if (zero? (v i))
              acc
              (mapcat (fn [u] [u (update u i -)]) acc)))
          [v] (range 3)))

(def solid-vertices
  "Vertices of each Platonic solid (any scale; centred on the origin)."
  {:tetrahedron  [[1 1 1] [1 -1 -1] [-1 1 -1] [-1 -1 1]]
   :cube         (vec (signs [1 1 1]))
   :octahedron   (vec (mapcat signs (cyclic [1 0 0])))
   :icosahedron  (vec (mapcat signs (cyclic [0 1 phi])))
   :dodecahedron (vec (concat (signs [1 1 1])
                              (mapcat signs (cyclic [0 (/ 1.0 phi) phi]))))})

(def in-out-ratio
  "Inradius over circumradius: the gap Kepler fitted between planetary spheres."
  (let [r (Math/sqrt (/ (+ 5.0 (* 2.0 (Math/sqrt 5.0))) 15.0))]
    {:tetrahedron (/ 1.0 3.0) :cube (/ 1.0 (Math/sqrt 3.0)) :octahedron (/ 1.0 (Math/sqrt 3.0))
     :dodecahedron r :icosahedron r}))

(defn- dist [a b] (Math/sqrt (reduce + (map #(let [d (- %1 %2)] (* d d)) a b))))

(defn solid-edges
  "Edges of `solid` as pairs of vertex indices: the closest vertex pairs."
  [solid]
  (let [vs    (solid-vertices solid)
        pairs (for [i (range (count vs)) j (range (inc i) (count vs))] [i j])
        d     #(dist (vs (first %)) (vs (second %)))
        lo    (apply min (map d pairs))]
    (filterv #(< (d %) (* lo 1.001)) pairs)))

(defn- project
  "Orthographic view of `p`: turned `yaw` about the vertical, tilted `pitch`."
  [[x y z] yaw pitch]
  (let [x1 (+ (* x (Math/cos yaw)) (* z (Math/sin yaw)))
        z1 (- (* z (Math/cos yaw)) (* x (Math/sin yaw)))]
    [x1 (- (* y (Math/cos pitch)) (* z1 (Math/sin pitch)))]))

(def kepler-controls
  "A select (the solid, in Kepler's order from Saturn inwards), a slider (the
   turn) and a toggle (the spheres)."
  [{:id :solid :kind :select :label "Platonic solid"
    :options [{:value :cube         :label "cube (Saturn / Jupiter)"}
              {:value :tetrahedron  :label "tetrahedron (Jupiter / Mars)"}
              {:value :dodecahedron :label "dodecahedron (Mars / Earth)"}
              {:value :icosahedron  :label "icosahedron (Earth / Venus)"}
              {:value :octahedron   :label "octahedron (Venus / Mercury)"}]
    :value :cube}
   {:id :turn :label "turn" :min 0.0 :max 6.28 :value 0.5}
   {:id :spheres :kind :toggle :label "inscribed and circumscribed spheres" :value true}])

(defn kepler-demo
  "One Platonic solid, wireframe, turned by `:turn`; with `:spheres` its
   circumscribed and inscribed spheres (circles in an orthographic view)."
  [{:keys [solid turn spheres] :or {solid :cube turn 0.5 spheres true}}]
  (let [R     2.6
        c     [0 -0.4]
        vs    (mapv (fn [v] (let [k (/ R (dist v [0 0 0]))]
                              (mapv + c (project (mapv #(* k %) v) turn 0.45))))
                    (solid-vertices solid))
        es    (solid-edges solid)
        ratio (in-out-ratio solid)]
    (fn [stage]
      (title! stage "\\text{Kepler: } \\textit{Mysterium Cosmographicum}")
      (show-all!
       stage
       (concat
        (for [[i j] es] (s/line (vs i) (vs j) :color :gold :width 3))
        (for [v vs] (-> (s/dot :color :white :radius 0.05) (s/move-to v)))
        (when spheres
          [(-> (s/circle :radius R :color :blue :width 2) (s/move-to c))
           (-> (s/circle :radius (* R ratio) :color :teal :width 2) (s/move-to c))])
        [(-> (s/text (str (name solid) ": " (count vs) " vertices, " (count es)
                          " edges, r/R = " (fixed3 ratio))
                     :font-size 28 :color :white)
             (s/move-to [0 -3.5]))])))))

;; ── Goedel numbering: a text field with a parser ─────────────────────────────

(def godel-codes
  "Nagel and Newman's alphabet, typed in ASCII: ~ not, | or, > implies,
   E exists, then = 0 s ( ) , + *, and the variables x y z."
  {"~" 1 "|" 2 ">" 3 "E" 4 "=" 5 "0" 6 "s" 7 "(" 8 ")" 9 "," 10 "+" 11 "*" 12
   "x" 13 "y" 17 "z" 19})

(def ^:private glyph
  {"~" "\\sim" "|" "\\vee" ">" "\\supset" "E" "\\exists" "*" "\\times"})

(defn parse-formula
  "The formula typed in `s` (whitespace dropped), or {:error msg} when it is
   empty or uses a symbol outside godel-codes."
  [s]
  (let [f   (str/replace (str s) #"\s" "")
        bad (distinct (remove godel-codes (map str f)))]
    (cond (empty? f) {:error "type a formula, e.g. 0=0"}
          (seq bad)  {:error (str "not in Goedel's alphabet: " (str/join " " bad))}
          :else      f)))

(defn primes
  "The first `n` primes."
  [n]
  (->> (iterate inc 2)
       (filter (fn [k] (not-any? #(zero? (mod k %)) (take-while #(<= (* % %) k) (range 2 k)))))
       (take n)))

(defn- times
  "Little-endian decimal digits `ds` times the small integer `k`."
  [ds k]
  (loop [ds (seq ds) carry 0 out []]
    (cond ds           (let [v (+ (* (first ds) k) carry)]
                         (recur (next ds) (quot v 10) (conj out (mod v 10))))
          (pos? carry) (recur nil (quot carry 10) (conj out (mod carry 10)))
          :else        out)))

(defn godel-number
  "The Goedel number of formula `f` as a decimal string: the product of the
   i-th prime raised to the code of the i-th symbol. Exact on every host
   (no bignums: decimal digits are multiplied by hand)."
  [f]
  (let [codes (map (comp godel-codes str) f)
        ds    (reduce (fn [ds [p c]] (reduce (fn [ds _] (times ds p)) ds (range c)))
                      [1] (map vector (primes (count codes)) codes))]
    (apply str (reverse ds))))

(defn- shorten [digits]
  (if (<= (count digits) 36)
    digits
    (str (subs digits 0 30) "... (" (count digits) " digits)")))

(def goedel-controls
  "One text field; parse-formula refuses symbols outside the alphabet."
  [{:id :formula :kind :text :label "formula: ~ | > E = 0 s ( ) , + * x y z"
    :value "0=0" :parse parse-formula}])

(defn goedel-demo
  "Formula `:formula` symbol by symbol: its code, the prime power it becomes,
   and the Goedel number, the product of those powers."
  [{:keys [formula] :or {formula "0=0"}}]
  (let [syms  (mapv str formula)
        n     (count syms)
        dx    (min 1.2 (/ 12.0 (max 1 n)))
        x-of  #(* dx (- % (/ (dec n) 2.0)))
        ps    (vec (primes n))]
    (fn [stage]
      (title! stage "\\text{G\\\"odel numbering}")
      (show-all!
       stage
       (concat
        (for [[i c] (map-indexed vector syms)]
          (-> (s/tex (glyph c c) :font-size 48 :color :gold) (s/move-to [(x-of i) 1.8])))
        (for [[i c] (map-indexed vector syms)]
          (-> (s/text (str (godel-codes c)) :font-size 28 :color :white)
              (s/move-to [(x-of i) 0.9])))
        (for [[i c] (map-indexed vector syms)]
          (-> (s/tex (str (ps i) "^{" (godel-codes c) "}") :font-size 30 :color :teal)
              (s/move-to [(x-of i) 0.0])))
        [(-> (s/text (str "= " (shorten (godel-number formula))) :font-size 34 :color :white)
             (s/move-to [0 -1.3]))
         (-> (s/text "codes (Nagel & Newman): ~1 |2 >3 E4 =5 0 6 s7 (8 )9 ,10 +11 *12 x13 y17 z19"
                     :font-size 22 :color :grey)
             (s/move-to [0 -2.8]))])))))

;; ── registry ─────────────────────────────────────────────────────────────────

(def demos
  {:turing {:title "Turing machine (buttons)"     :construct turing-demo :controls turing-controls}
   :kepler {:title "Kepler's solids (select)"     :construct kepler-demo :controls kepler-controls}
   :goedel {:title "Goedel numbering (text)"      :construct goedel-demo :controls goedel-controls}})

(defn defaults
  "The default params of demo `k`: every control that holds a value."
  [k]
  (controls/defaults (get-in demos [k :controls])))

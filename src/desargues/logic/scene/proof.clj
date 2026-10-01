(ns desargues.logic.scene.proof
  "The proof flow: a checked proof (desargues.logic.proof) as numbered lines,
   Pinter-style element chasing on screen.

   The goal is pinned at the top. Each line appears with its number, its
   claim as TeX on the left (indented, with a vertical bar per open
   subproof) and its reason on the right in a muted colour. A :def line first
   highlights, in the line it comes from, the subterm the definition
   unfolds. A :use line first shows a callout with the cited result and its
   substitution, which then folds into the new line. At most (:visible
   layout) lines stay on screen: older ones scroll up and vanish. The last
   line is outlined, green with a ∎ when the proof checks, red otherwise.

     (beats proof report & {:keys [library]})  ; pure, report = check's :ok
     (flow proof & {:keys [library]})          ; Result of beats
     (construct proof & {:keys [library pace]})
     (record scene-name proof & {:keys [library pace]})

   `reason` (the right-hand column) and `annotate` (what a rule shows around
   its line) are OPEN on the line's :by. Played by
   desargues.logic.scene.beats/play!."
  (:require [clojure.string :as str]
            [desargues.logic.formula :as f]
            [desargues.logic.proof :as p]
            [desargues.logic.proof.library :as lib]
            [desargues.logic.proof.scope :as sc]
            [desargues.logic.scene.beats :as b]
            [desargues.logic.tex :as lt]
            [desargues.scene :as s]
            [desargues.scene.data :as rec]
            [hive-dsl.result :as r]))

(def layout
  "Geometry of the flow, in world units and px."
  {:goal-y 3.5 :goal-size 26 :rule-y 3.05 :margin 6.8
   :top 2.55 :h 0.68 :visible 7
   :num-x -6.55 :num-size 22
   :claim-left -6.05 :claim-right 2.35 :indent 0.42 :max-depth 6 :claim-size 28
   :reason-x 4.75 :reason-width 4.5 :reason-size 20
   :callout-y -2.95 :callout-h 1.3 :callout-size 24
   :verdict-y -2.95 :verdict-size 24})

;; ---------------------------------------------------------------------------
;; Reading a proof (pure)

(defn refs
  "A line's :from as a vector of line indices."
  [from]
  (cond (nil? from) [] (sequential? from) (vec from) :else [from]))

(defn depths
  "The subproof depth each line is drawn at: the depth after the line enters
   its scope, so an assumption sits inside its subproof and a discharge
   outside it. Clamped to [0, (:max-depth layout)]."
  [lines]
  (mapv (fn [scope i line] (-> (:depth (sc/enter scope i line)) (max 0) (min (:max-depth layout))))
        (sc/scopes lines) (range) lines))

(defn at-path
  "The subterm of form at path (argument indices from the root)."
  [form path]
  (reduce (fn [g i] (nth (rest g) i)) form path))

(defn changed-path
  "The path of the smallest subterm of a that b changes: descend while both
   are the same operator with exactly one differing argument."
  [a b]
  (loop [a a b b path []]
    (if (and (seq? a) (seq? b) (= (first a) (first b)) (= (count a) (count b)))
      (let [diffs (keep-indexed (fn [i [x y]] (when (not= x y) i)) (map vector (rest a) (rest b)))]
        (if (= 1 (count diffs))
          (let [i (first diffs)]
            (recur (nth (rest a) i) (nth (rest b) i) (conj path i)))
          path))
      path)))

(defn- logical? [g]
  (or (f/compound? g) (and (seq? g) (contains? '#{forall exists exists!} (first g)))))

(defn defined-term
  "The term whose definition carries line a to line b: the changed subterm
   on the side that is not already logic; for a membership (in x C), the
   class C."
  [a b]
  (let [path (changed-path a b)
        sa (at-path a path)
        sb (at-path b path)
        d (if (and (logical? sa) (not (logical? sb))) sb sa)]
    (if (and (seq? d) (= 'in (first d)) (seq? (nth d 2 nil))) (nth d 2) d)))

(defn- generic
  "The term's operator applied to placeholder letters: (subset X Y)."
  [t]
  (if (seq? t) (cons (first t) (take (count (rest t)) '[X Y Z W V])) t))

;; ---------------------------------------------------------------------------
;; Reasons: open on :by

(defn- from-tex [ns] (str "\\text{, from } " (str/join ", " ns)))

(defn- range-tex [[a b]] (str "\\text{, from } " a "\\text{--}" b))

(defn substitution-tex
  "TeX for a substitution {var term}: x := t, ..., in variable order."
  [with]
  (str/join ",\\ " (for [[v t] (sort-by (comp str key) with)] (str (lt/->TeX v) " := " (lt/->TeX t)))))

(defmulti reason
  "[line ctx] -> TeX for the reason of line; ctx {:lines :library}."
  (fn [line _ctx] (:by line)))

(defmethod reason :default [{:keys [by from]} _]
  (str "\\text{" (name by) "}" (when (seq (refs from)) (from-tex (refs from)))))

(defmethod reason :hyp [_ _] "\\text{hypothesis}")
(defmethod reason :assume [_ _] "\\text{assumption}")
(defmethod reason :refl [_ _] "\\text{reflexivity}")

(defmethod reason :discharge [{:keys [from]} _] (str "\\text{discharge}" (range-tex (refs from))))

(defmethod reason :def [{:keys [claim from]} {:keys [lines]}]
  (let [j (first (refs from))]
    (str "\\text{def. of }" (lt/->TeX (generic (defined-term (:claim (nth lines j)) claim))) (from-tex [j]))))

(defmethod reason :taut [{:keys [from]} _] (str "\\text{propositional}" (from-tex (refs from))))

(defmethod reason :use [{:keys [result from]} _] (str "\\text{by Ex. " result "}" (from-tex (refs from))))

(defmethod reason :cite [{:keys [result]} _] (str "\\text{by Ex. " result "}"))

(defmethod reason :inst [{:keys [from with]} _]
  (str "\\forall\\text{-elim, }" (substitution-tex with) (from-tex (refs from))))

(defmethod reason :gen [{:keys [from vars]} _]
  (str "\\forall\\text{ over arbitrary }" (str/join ", " (map lt/->TeX vars)) (from-tex (refs from))))

(defmethod reason :witness [{:keys [from]} _] (str "\\exists\\text{-intro}" (from-tex (refs from))))

(defmethod reason :choose [{:keys [from with]} _]
  (str "\\text{choose }" (str/join ", " (map lt/->TeX (vals with))) (from-tex (refs from))))

(defmethod reason :exists-elim [{:keys [from]} _] (str "\\exists\\text{-elim}" (range-tex (refs from))))

(defmethod reason :subst [{:keys [from]} _] (str "\\text{equals for equals}" (from-tex (refs from))))

(defmethod reason :model [{:keys [from]} _] (str "\\text{no finite counterexample}" (from-tex (refs from))))

;; ---------------------------------------------------------------------------
;; Items (pure)

(defn slot-y [k] (- (:top layout) (* k (:h layout))))

(defn shown-slot
  "The slot of line i while line n is the newest."
  [i n]
  (- i (max 0 (- n (dec (:visible layout))))))

(defn- on-screen? [i n] (<= 0 (shown-slot i n)))

(defn- claim-item
  "The claim of line i drawn at depth d, slot k, left-aligned."
  [i latex d k color]
  (let [{:keys [claim-left claim-right indent claim-size]} layout
        left (+ claim-left (* d indent))
        it {:id [:claim i] :kind :tex :latex latex :at [0 0] :size claim-size :color color}
        it (assoc it :size (b/fit-size it (- claim-right left)))]
    (assoc it :at [(+ left (/ (b/width it) 2)) (slot-y k)])))

(defn- reason-item
  "The reason of line i at height y, fitted to the reason column, no larger
   than the flow's common reason size (:reason-size ctx)."
  [{:keys [lines rows] :as ctx} i y]
  (let [{:keys [reason-x reason-width reason-size]} layout
        ok? (:ok? (get rows i) true)
        why (cond->> (reason (nth lines i) ctx) (not ok?) (str "\\times\\ "))
        it {:id [:reason i] :kind :tex :latex why :at [reason-x y] :size reason-size
            :color (if ok? :grey :red)}]
    (assoc it :size (min (b/fit-size it reason-width) (:reason-size ctx reason-size)))))

(defn- line-items
  "Every item of line i at slot k."
  [{:keys [lines depths] :as ctx} i k]
  (let [{:keys [num-x num-size claim-left indent h]} layout
        d (depths i)
        y (slot-y k)]
    (into [{:id [:num i] :kind :tex :latex (str i) :at [num-x y] :size num-size :color :grey}
           (claim-item i (lt/->TeX (:claim (nth lines i))) d k :white)
           (reason-item ctx i y)]
          (for [e (range 1 (inc d))
                :let [x (+ claim-left (* (dec e) indent) 0.12)]]
            {:id [:bar i e] :kind :line :from [x (+ y (/ h 2))] :to [x (- y (/ h 2))]
             :color :grey :width 2}))))

(defn- line-ids [ctx i] (mapv :id (line-items ctx i 0)))

(defn- scroll
  "Beat making room for line n: older lines glide up a slot, the oldest goes."
  [ctx n]
  (let [gone (- n (:visible layout))
        centre (fn [it] (let [[[x0 y0] [x1 y1]] (b/extent it)] [(/ (+ x0 x1) 2.0) (/ (+ y0 y1) 2.0)]))]
    {:beat :par
     :beats [{:beat :hide :ids (line-ids ctx gone)}
             {:beat :move :to (into {} (for [i (range (inc gone) n)
                                             it (line-items ctx i (shown-slot i n))]
                                         [(:id it) (centre it)]))}]}))

;; ---------------------------------------------------------------------------
;; What a rule shows around its line: open on :by

(defmulti annotate
  "[ctx n line] -> {:before [beat] :entry [beat] :after [beat]} for line n,
   every key optional; :entry replaces the plain appearance of the line."
  (fn [_ctx _n line] (:by line)))

(defmethod annotate :default [_ _ _] {})

(defmethod annotate :def [{:keys [lines depths]} n {:keys [claim from]}]
  (let [j (first (refs from))
        src (:claim (nth lines j))
        k (shown-slot j n)
        plain (fn [latex] (claim-item j latex (depths j) k :white))]
    (if (on-screen? j n)
      {:before [{:beat :swap :ids [[:claim j]]
                 :items [(plain (lt/->TeX (lt/highlight src (changed-path src claim) :gold)))]}
                {:beat :hold :seconds 0.5}]
       :after [{:beat :swap :ids [[:claim j]] :items [(plain (lt/->TeX src))]}]}
      {})))

(defn- callout-items
  "The callout of a :use line: a box, the cited result, its substitution."
  [library {:keys [result with]}]
  (let [{:keys [margin callout-y callout-h callout-size]} layout
        stmt (some-> library (lib/statement result))
        fit (fn [it] (assoc it :size (b/fit-size it (- (* 2 margin) 0.4))))]
    [{:id [:callout :box] :kind :rect :center [0.0 callout-y] :size [(* 2 margin) callout-h]
      :color :gold :width 2}
     (fit {:id [:callout :statement] :kind :tex :at [0.0 (+ callout-y (* 0.25 callout-h))]
           :size callout-size :color :white
           :latex (str "\\text{Ex. " result ": }"
                       (if stmt (lt/->TeX stmt) "\\text{not in the library}"))})
     (fit {:id [:callout :with] :kind :tex :at [0.0 (- callout-y (* 0.25 callout-h))]
           :size callout-size :color :gold
           :latex (if (seq with) (substitution-tex with) "\\text{as stated}")})]))

(defmethod annotate :use [{:keys [library] :as ctx} n {:keys [from] :as line}]
  (let [cited (filterv #(on-screen? % n) (refs from))
        cited-ids (mapv (fn [j] [:claim j]) cited)
        callout (callout-items library line)
        statement (second callout)
        half (/ (b/width statement) 2)
        claim (first (filter #(= [:claim n] (:id %)) (line-items ctx n (shown-slot n n))))
        [[x0 _] _] (b/extent claim)
        target [(min (- (:margin layout) half) (+ x0 half)) (second (:at claim))]]
    {:before [{:beat :par :beats [{:beat :show :items callout}
                                  {:beat :recolor :ids cited-ids :color :gold}]}
              {:beat :hold :seconds 1.2}]
     :entry [{:beat :move :to {(:id statement) target}}
             {:beat :par :beats [{:beat :hide :ids (mapv :id callout)}
                                 {:beat :show :items (line-items ctx n (shown-slot n n))}
                                 {:beat :recolor :ids cited-ids :color :white}]}]}))

;; ---------------------------------------------------------------------------
;; Beats (pure)

(defn- line-beats [ctx n line]
  (let [{:keys [before entry after]} (annotate ctx n line)]
    (concat (when (>= n (:visible layout)) [(scroll ctx n)])
            before
            (or entry [{:beat :show :items (line-items ctx n (shown-slot n n))}])
            after)))

(defn- verdict-text [{:keys [ok? concludes? lines]}]
  (let [bad (first (remove :ok? lines))]
    (cond ok? "the last line is the goal: proved"
          bad (str "line " (:index bad) ": " (:why bad))
          (not concludes?) "every line checks, but the last line is not the goal"
          :else "the proof does not check")))

(defn- closing [ctx report]
  (let [{:keys [margin verdict-y verdict-size]} layout
        n (dec (count (:lines ctx)))
        claim (first (filter #(= [:claim n] (:id %)) (line-items ctx n (shown-slot n n))))
        [[x0 y0] [x1 y1]] (b/extent claim)
        color (if (:ok? report) :green :red)
        verdict {:id :verdict :kind :text :text (verdict-text report) :at [0.0 verdict-y]
                 :size verdict-size :color color}]
    [{:beat :show :items [{:id [:qed :box] :kind :rect :center [(/ (+ x0 x1) 2) (/ (+ y0 y1) 2)]
                           :size [(+ (- x1 x0) 0.25) (+ (- y1 y0) 0.16)] :color color :width 3}
                          {:id [:qed :mark] :kind :text :text (if (:ok? report) "∎" "✗")
                           :at [(+ x1 0.35) (/ (+ y0 y1) 2)] :size 30 :color color}]}
     {:beat :show :items [(assoc verdict :size (b/fit-size verdict (* 2 margin)))]}
     {:beat :hold :seconds 1.5}]))

(defn beats
  "The proof flow of proof as beats, given report (the :ok of
   desargues.logic.proof/check). opts: :library, the ResultLibrary the proof
   was checked against (for :use callouts)."
  [{:keys [goal lines]} report & {:keys [library] :or {library lib/empty-library}}]
  (let [{:keys [goal-y goal-size rule-y margin]} layout
        ctx0 {:lines lines :rows (vec (:lines report)) :depths (depths lines) :library library}
        ctx (assoc ctx0 :reason-size (reduce min (map #(:size (reason-item ctx0 % 0)) (range (count lines)))))
        g {:id :goal :kind :tex :latex (str "\\text{Goal: }" (lt/->TeX goal)) :at [0.0 goal-y]
           :size goal-size :color :white}]
    (vec (concat
          [{:beat :show :items [(assoc g :size (b/fit-size g (* 2 margin)))
                                {:id :rule :kind :line :from [(- margin) rule-y] :to [margin rule-y]
                                 :color :grey :width 2}]}
           {:beat :hold :seconds 0.6}]
          (mapcat (fn [n line] (line-beats ctx n line)) (range) lines)
          (closing ctx report)))))

(defn flow
  "(ok beats) for a well-shaped proof checked against :library, else check's
   error."
  [proof & {:keys [library] :or {library lib/empty-library}}]
  (r/map-ok (p/check proof library) #(beats proof % :library library)))

;; ---------------------------------------------------------------------------
;; Boundary

(defn construct
  "A construct fn playing the proof flow of proof. opts: :library, :pace.
   Throws ex-info carrying the checker's error for a malformed proof."
  [proof & {:as opts}]
  (let [res (flow proof opts)]
    (when-not (r/ok? res)
      (throw (ex-info "The proof is malformed" (:error res))))
    (fn [stage] (b/play! stage (:ok res) opts))))

(defn record
  "Record the proof flow of proof against a fresh RecordingBackend."
  [scene-name proof & {:as opts}]
  (s/with-backend (rec/recording-backend)
    (s/render! scene-name (construct proof opts))))

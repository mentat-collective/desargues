(ns desargues.lesson.schema
  "CORE stratum of lessons: the LESSON BUNDLE, a data-only value.

   A lesson is slides of blocks; every block is plain EDN that one of
   desargues, Emmy or emmy-viewers already understands, so the bundle can be
   written out, read back and rebuilt reproducibly.

     {:lesson/id :area-under-curve
      :lesson/title \"Area under a curve\"
      :lesson/version \"1.0.0\"
      :lesson/slides
      [{:slide/id :intro :slide/title \"The question\"
        :slide/blocks
        [{:block/id :b-md   :block/kind :prose        :md \"How much area?\"}
         {:block/id :b-tex  :block/kind :tex          :expr '(integral (expt x 2) x 0 2)}
         {:block/id :b-fig  :block/kind :figure       :figure <calc figure>}
         {:block/id :b-con  :block/kind :construction :construction <board construction>}
         {:block/id :b-view :block/kind :viewer       :fragment '[mafs.core/Mafs {} ...]}
         {:block/id :b-scn  :block/kind :scene        :scene {:scene \"s\" :steps [...]}}
         {:block/id :b-png  :block/kind :media        :from :b-fig :format :png}]}]}

   Block payloads, by :block/kind:
     :prose         :md            markdown string
     :tex           :expr          a FROZEN Emmy expression (numbers, symbols and
                                   lists headed by a symbol, e.g. '(expt x 2)),
                                   rendered later with emmy ->TeX
     :figure        :figure        a desargues.objects.calc.core figure; it must
                                   pass calc/figure-problems (functions named by
                                   catalog key, never a bare fn)
     :construction  :construction  desargues.board.construction EDN
                                   ({:kind :construction :points [...] ...})
     :viewer        :fragment      a quoted emmy-viewers Reagent form
     :scene         :scene         a desargues.scene.data recording {:scene :steps}
     :media         :from :format  an asset rendered from block :from as
                                   :png, :gif or :mp4

   Beyond shape: block and slide ids are unique, every :media :from names an
   existing non-media block, and the whole bundle is `pure-data?` (no fns,
   atoms, vars or objects that do not round-trip through pr-str/read-string).

   API: valid?, explain (nil when valid), validate (the lesson or ex-info),
   pure-data?, impure-paths. Host-free .cljc, malli-first."
  (:require [malli.core :as m]
            [malli.error :as me]
            [desargues.objects.calc.core :as calc]
            #?(:clj [clojure.edn :as edn] :cljs [cljs.reader :as edn])))

;; ── pure data ───────────────────────────────────────────────────────────────

(defn- leaf-data?
  "Scalars EDN carries without loss (NaN is excluded: it never equals itself)."
  [x]
  (or (nil? x) (boolean? x) (string? x) (keyword? x) (symbol? x) (char? x)
      (and (number? x)
           #?(:clj (not (and (or (instance? Double x) (instance? Float x))
                             (or (Double/isNaN (double x)) (Double/isInfinite (double x)))))
              :cljs (js/isFinite x)))))

(defn impure-paths
  "Paths (vectors of keys / indices) to every value inside `x` that is not
   plain EDN data: fns, atoms, vars, records, objects. Empty when pure."
  ([x] (impure-paths x []))
  ([x path]
   (cond
     (record? x)     [path]
     (map? x)        (into [] (mapcat (fn [[k v]] (concat (impure-paths k (conj path [:key k]))
                                                          (impure-paths v (conj path k)))))
                           x)
     (or (vector? x) (seq? x))
     (into [] (comp (map-indexed (fn [i v] (impure-paths v (conj path i)))) cat) x)
     (set? x)        (into [] (mapcat #(impure-paths % (conj path %))) x)
     (leaf-data? x)  []
     :else           [path])))

(defn- round-trips? [x]
  (try
    (= x (edn/read-string (binding [*print-length* nil *print-level* nil
                                    *print-meta* false *print-namespace-maps* false]
                            (pr-str x))))
    (catch #?(:clj Exception :cljs :default) _ false)))

(defn pure-data?
  "True when `x` is plain EDN data: built only of nil, booleans, finite
   numbers, strings, keywords, symbols, chars, maps, vectors, sets and lists,
   AND it reads back equal from its pr-str (EDN reader). Fns, atoms, vars,
   records, regexes and host objects are rejected."
  [x]
  (and (empty? (impure-paths x)) (round-trips? x)))

;; ── payload schemas ─────────────────────────────────────────────────────────

(def Expr
  "A frozen Emmy expression: numbers, symbols, strings, and lists headed by a
   symbol (an operator application), nested; vectors hold structures/matrices."
  [:schema {:registry {"lesson/expr"
                       [:or
                        number? symbol? string?
                        [:and seq? [:cat symbol? [:* [:schema [:ref "lesson/expr"]]]]]
                        [:vector [:ref "lesson/expr"]]]}}
   "lesson/expr"])

(def FigureShape
  [:map
   [:view [:map [:view [:enum :plane :space]]]]
   [:objects [:vector [:map [:kind :keyword]]]]
   [:params {:optional true} [:map-of :keyword any?]]
   [:title {:optional true} :string]
   [:animate {:optional true} map?]])

(def Figure
  "Figure data desargues.objects.calc.core accepts: its shape, then
   calc/figure-problems must be empty."
  [:and FigureShape
   [:fn {:error/fn (fn [{:keys [value]} _]
                     (str "calc figure problems: "
                          (pr-str (try (calc/figure-problems value)
                                       (catch #?(:clj Exception :cljs :default) e
                                         [{:problem :figure/throws :message (ex-message e)}])))))}
    (fn [fig] (try (calc/valid? fig) (catch #?(:clj Exception :cljs :default) _ false)))]])

(def Construction
  "desargues.board.construction EDN, checked structurally (compiling it needs
   the kernel toolchain): :kind :construction and points with keyword ids."
  [:map
   [:kind [:= :construction]]
   [:id {:optional true} :keyword]
   [:points [:vector {:min 1} [:map [:id :keyword]]]]
   [:params {:optional true} [:vector [:map [:id [:or :symbol :keyword]]]]]
   [:maps {:optional true} [:map-of :keyword any?]]
   [:groups {:optional true} [:map-of :keyword vector?]]
   [:draw {:optional true} [:vector vector?]]
   [:checks {:optional true} [:vector map?]]
   [:view {:optional true} map?]])

(def Fragment
  "A quoted emmy-viewers Reagent form: a vector headed by a component symbol
   or a tag keyword, or a list headed by a symbol (a quoted call)."
  [:or
   [:and vector? [:cat [:or :symbol :keyword] [:* any?]]]
   [:and seq? [:cat :symbol [:* any?]]]])

(def Scene
  "A desargues.scene.data recording."
  [:map
   [:scene [:or :string :keyword :symbol]]
   [:steps [:vector [:map [:step :keyword]]]]
   [:nodes {:optional true} [:map-of any? map?]]
   [:node-count {:optional true} :int]])

(def media-formats #{:png :gif :mp4})

;; ── blocks, slides, lesson ──────────────────────────────────────────────────

(def block-kinds #{:prose :tex :figure :construction :viewer :scene :media})

(defn- block [& entries]
  (into [:map [:block/id :keyword] [:block/kind :keyword]] entries))

(def Block
  [:multi {:dispatch :block/kind
           :error/message (str "unknown :block/kind, expected one of " (pr-str (sort block-kinds)))}
   [:prose        (block [:md :string])]
   [:tex          (block [:expr Expr])]
   [:figure       (block [:figure Figure])]
   [:construction (block [:construction Construction])]
   [:viewer       (block [:fragment Fragment])]
   [:scene        (block [:scene Scene])]
   [:media        (block [:from :keyword] [:format (into [:enum] (sort media-formats))])]])

(def Slide
  [:map
   [:slide/id :keyword]
   [:slide/title :string]
   [:slide/blocks [:vector Block]]])

(def Lesson
  [:map
   [:lesson/id :keyword]
   [:lesson/title :string]
   [:lesson/version :string]
   [:lesson/slides [:vector Slide]]])

(def ^:private shape-valid? (m/validator Lesson))
(def ^:private shape-explainer (m/explainer Lesson))

;; ── cross-checks ────────────────────────────────────────────────────────────

(defn- blocks [lesson] (mapcat :slide/blocks (:lesson/slides lesson)))

(defn- duplicates [xs]
  (into [] (keep (fn [[k n]] (when (> n 1) k))) (frequencies xs)))

(defn reference-problems
  "Cross-block problems of a well-shaped `lesson`: duplicate slide or block
   ids, and :media blocks whose :from is missing, itself, or another :media."
  [lesson]
  (let [bs     (blocks lesson)
        by-id  (into {} (map (juxt :block/id identity)) bs)]
    (into (cond-> []
            (seq (duplicates (map :slide/id (:lesson/slides lesson))))
            (conj {:problem :lesson/duplicate-slide-id
                   :ids (duplicates (map :slide/id (:lesson/slides lesson)))})
            (seq (duplicates (map :block/id bs)))
            (conj {:problem :lesson/duplicate-block-id :ids (duplicates (map :block/id bs))}))
          (for [{:block/keys [id kind] :keys [from]} bs
                :when (= :media kind)
                :let [target (get by-id from)
                      problem (cond (nil? target)            :media/unknown-from
                                    (= from id)              :media/self-reference
                                    (= :media (:block/kind target)) :media/from-media)]
                :when problem]
            {:problem problem :block id :from from}))))

;; ── API ─────────────────────────────────────────────────────────────────────

(defn explain
  "Why `lesson` is not a valid bundle, or nil when it is:
   {:impure [path ...]       values that are not plain EDN
    :schema {...}            humanized malli errors
    :references [{...}]}     id / :media cross-check problems
   Only the keys with problems are present."
  [lesson]
  (let [impure (impure-paths lesson)
        impure (if (and (empty? impure) (not (round-trips? lesson))) [[]] impure)
        shape  (when-not (shape-valid? lesson) (me/humanize (shape-explainer lesson)))
        refs   (when-not shape (not-empty (reference-problems lesson)))]
    (not-empty (cond-> {}
                 (seq impure) (assoc :impure impure)
                 shape        (assoc :schema shape)
                 refs         (assoc :references refs)))))

(defn valid? [lesson] (nil? (explain lesson)))

(defn validate
  "`lesson` when it is a valid bundle; otherwise throws ex-info carrying
   (explain lesson) under :explain."
  [lesson]
  (if-let [why (explain lesson)]
    (throw (ex-info "invalid lesson bundle" {:explain why}))
    lesson))

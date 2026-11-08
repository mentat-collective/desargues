(ns desargues.player.controls
  "PURE stratum of the player: the widget (control) model.

   A control spec is plain data, {:id :kind ...}, and every host renders it
   with one method per kind (OCP). What a user input DOES to the parameters
   is decided here, once, for every host: a host only reports the raw input
   (a number, a boolean, an option value, a typed string, a press) and the
   player turns it into new parameters with `input`.

   Kinds (open: a new kind is one method of each multimethod below plus one
   widget method per host):

     :slider  {:id :kind :slider :label :min :max :value}
              params[:id] = the number, clamped to [min max]. A spec with no
              :kind and no :drag is a slider (the original vocabulary).
     :toggle  {:id :kind :toggle :label :value false}
              params[:id] = the boolean.
     :select  {:id :kind :select :label :options [...] :value}
              :options is a seq of values or of {:value :label}; params[:id]
              = the chosen value. A value not among the options is refused.
     :text    {:id :kind :text :label :value \"...\" :parse f :format g}
              params[:id] = (parse string). parse is pure; it throws or
              returns {:error msg} to refuse the text, which leaves params
              alone and reports the message. :format renders a value back
              into the field (default str). Without :parse the string itself.
     :button  {:id :kind :button :label :on-press f}  or  {... :set {k v}}
              a press is a discrete step of the session: params' =
              (on-press params), or params merged with :set (a preset).
              Holding state between presses is just reading it from params.

   Any widget may also carry :seek, a time in seconds or a pure fn of the
   NEW params giving one: after its input is accepted the player moves its
   clock there and plays on (a lesson's next-step button jumps to where
   that step starts being written). Without :seek the clock keeps its time.

   Drag specs ({:id :drag {...}}, desargues.player.drag) are moved by the
   pointer, not by a widget; their kind is :drag and no host draws them.

   Host-free .cljc.")

;; ── Kind ──────────────────────────────────────────────────────────────

(defn kind
  "The widget kind of `spec`: its :kind, :drag for a drag spec, else :slider."
  [spec]
  (cond (:kind spec)           (:kind spec)
        (contains? spec :drag) :drag
        :else                  :slider))

(defn widget?
  "Does `spec` need a widget in the host (anything but a drag spec)?"
  [spec]
  (not= :drag (kind spec)))

(defn label
  "The text a host shows beside or on the widget."
  [spec]
  (or (:label spec) (some-> (:id spec) name)))

;; ── Per-kind behaviour (OCP: one method per kind) ─────────────────────

(defmulti problems
  "Seq of human-readable problems with `spec` (empty when it is well formed)."
  kind)

(defmulti default-value
  "The parameter value `spec` starts at when params do not name it, or nil
   when the kind holds no value (a button)."
  kind)

(defmulti input
  "The outcome of raw user input `raw` on the widget of `spec`, given the
   current `params`: {:params params'} or {:params params :error msg} when
   the input is refused. Pure."
  (fn [spec _params _raw] (kind spec)))

(defmulti display
  "How the widget of `spec` shows parameter value `v` (what -set-control!
   moves it to): a number, a boolean, an option value, the text."
  (fn [spec _v] (kind spec)))

(defn- need [spec k msg] (when-not (some? (get spec k)) [msg]))

(defmethod problems :default [spec]
  [(str "unknown control kind " (pr-str (kind spec)) " for " (pr-str (:id spec)))])
(defmethod default-value :default [spec] (:value spec))
(defmethod input :default [spec params raw] {:params (assoc params (:id spec) raw)})
(defmethod display :default [_ v] v)

;; drag: moved by the pointer (desargues.player.drag), never by a widget

(defmethod problems :drag [_] [])
(defmethod default-value :drag [spec] (:value spec))

;; slider

(defn- clamp [lo hi x] (-> x (max lo) (min hi)))

(defmethod problems :slider [spec]
  (concat (need spec :min ":slider needs :min")
          (need spec :max ":slider needs :max")
          (when (and (number? (:min spec)) (number? (:max spec))
                     (> (:min spec) (:max spec)))
            [":slider :min is above :max"])))

(defmethod default-value :slider [spec] (or (:value spec) (:min spec)))

(defmethod input :slider [{:keys [id min max]} params raw]
  (if (number? raw)
    {:params (assoc params id (clamp (double min) (double max) (double raw)))}
    {:params params :error (str "not a number: " (pr-str raw))}))

(defmethod display :slider [spec v] (double (or v (default-value spec))))

;; toggle

(defmethod problems :toggle [_] [])
(defmethod default-value :toggle [spec] (boolean (:value spec)))
(defmethod input :toggle [{:keys [id]} params raw] {:params (assoc params id (boolean raw))})
(defmethod display :toggle [_ v] (boolean v))

;; select

(defn options
  "The options of select `spec` as [{:value :label}], in order."
  [spec]
  (mapv (fn [o] (if (and (map? o) (contains? o :value))
                  (update o :label #(or % (str (:value o))))
                  {:value o :label (if (keyword? o) (name o) (str o))}))
        (:options spec)))

(defn option-index
  "Position of value `v` among the options of `spec`, or nil."
  [spec v]
  (first (keep-indexed (fn [i o] (when (= v (:value o)) i)) (options spec))))

(defmethod problems :select [spec]
  (cond (empty? (:options spec)) [":select needs :options"]
        (and (contains? spec :value) (nil? (option-index spec (:value spec))))
        [(str ":select :value " (pr-str (:value spec)) " is not an option")]
        :else []))

(defmethod default-value :select [spec]
  (if (contains? spec :value) (:value spec) (:value (first (options spec)))))

(defmethod input :select [{:keys [id] :as spec} params raw]
  (if (option-index spec raw)
    {:params (assoc params id raw)}
    {:params params :error (str (pr-str raw) " is not an option")}))

(defmethod display :select [spec v] (if (option-index spec v) v (default-value spec)))

;; text

(defn- error-message [e] (or (ex-message e) (str e)))

(defn parse-text
  "Run the :parse of text `spec` on string `s`: {:value v} or {:error msg}.
   parse refuses by throwing or by returning {:error msg}."
  [spec s]
  (let [parse (or (:parse spec) identity)]
    (try
      (let [v (parse s)]
        (if (and (map? v) (contains? v :error) (= 1 (count v)))
          {:error (str (:error v))}
          {:value v}))
      (catch #?(:clj Throwable :cljs :default) e
        {:error (error-message e)}))))

(defmethod problems :text [spec]
  (when (and (contains? spec :parse) (not (ifn? (:parse spec))))
    [":text :parse must be a function"]))

(defmethod default-value :text [spec]
  (let [s (str (or (:value spec) ""))]
    (if (:parse spec)
      (let [{:keys [value error]} (parse-text spec s)] (when-not error value))
      s)))

(defmethod input :text [{:keys [id] :as spec} params raw]
  (let [{:keys [value error]} (parse-text spec (str raw))]
    (if error
      {:params params :error error}
      {:params (assoc params id value)})))

(defmethod display :text [spec v]
  (if (string? v) v ((or (:format spec) str) v)))

;; button

(defmethod problems :button [spec]
  (cond (and (contains? spec :on-press) (not (ifn? (:on-press spec))))
        [":button :on-press must be a function (fn [params] params')"]
        (and (contains? spec :set) (not (map? (:set spec))))
        [":button :set must be a map of params"]
        :else []))

(defmethod default-value :button [_] nil)

(defmethod input :button [{:keys [on-press set]} params _raw]
  (let [ps (cond-> params set (merge set) on-press on-press)]
    (if (map? ps)
      {:params ps}
      {:params params :error ":on-press must return the params map"})))

(defmethod display :button [_ _] nil)

;; ── Over a whole control set ──────────────────────────────────────────

(defn validate
  "`controls` unchanged when every spec is well formed (each needs an :id and
   a known kind, ids are unique); otherwise throws ex-info listing the
   problems by id."
  [controls]
  (let [ids  (map :id controls)
        dups (for [[id n] (frequencies ids) :when (> n 1)] id)
        errs (cond-> (into {}
                           (keep (fn [spec]
                                   (let [ps (if (nil? (:id spec))
                                              ["control needs an :id"]
                                              (seq (problems spec)))]
                                     (when (seq ps) [(:id spec) (vec ps)]))))
                           controls)
               (seq dups) (assoc ::duplicate-ids (vec dups)))]
    (if (seq errs)
      (throw (ex-info (str "bad control specs: " (pr-str errs)) {:problems errs}))
      controls)))

(defn defaults
  "{id value} for every control that holds a value: where params start when
   the caller names none."
  [controls]
  (into {} (keep (fn [spec]
                   (let [v (default-value spec)]
                     (when (some? v) [(:id spec) v]))))
        controls))

(defn widget-spec
  "`spec` as a host should draw it now: its :value is what `params` hold (or
   its default), shown through `display`."
  [spec params]
  (let [v (get params (:id spec) (default-value spec))]
    (cond-> spec
      (some? v) (assoc :value (display spec v))
      (= :select (kind spec)) (assoc :options (options spec)))))

(defn spec-by-id
  "The spec in `controls` with id `id`, or nil."
  [controls id]
  (some #(when (= id (:id %)) %) controls))

(defn changed
  "Ids of the widgets in `controls` whose parameter differs between params
   `before` and `after` (the widgets a host must move after a change)."
  [controls before after]
  (into [] (comp (filter widget?)
                 (remove #(= :button (kind %)))
                 (map :id)
                 (filter #(not= (get before %) (get after %))))
        controls))

(defn seek-time
  "Where the clock goes after `spec` accepted an input that left `params`:
   its :seek (seconds, or a fn of params), or nil to keep the time."
  [spec params]
  (let [sk (:seek spec)]
    (cond (number? sk) (double sk)
          (fn? sk)     (some-> (sk params) double)
          :else        nil)))

(defn step
  "The whole effect of raw input `raw` on widget `id`, as a value both
   players (JVM and browser) apply: {:params params' :error msg-or-nil
   :move [[id shown-value] ...] :seek t-or-nil}. :move lists the OTHER
   widgets whose parameter changed (a preset button moves the sliders it
   set), with what each should now show. :seek is where the clock goes
   (see seek-time); nil, or a refused input, keeps it. An unknown id
   changes nothing."
  [controls params id raw]
  (if-let [spec (spec-by-id controls id)]
    (let [{ps :params err :error} (input spec params raw)]
      {:params ps
       :error  err
       :move   (vec (for [k (changed controls params ps)
                          :when (not= k id)]
                      [k (display (spec-by-id controls k) (get ps k))]))
       :seek   (when-not err (seek-time spec ps))})
    {:params params :error (str "no control " (pr-str id)) :move []}))

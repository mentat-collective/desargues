(ns desargues.wallpaper.trama
  "Field forms as fosfora TRAMA SOURCES: one <id>.wgsl per layer, a JSON
   manifest comment then WGSL, validated by naga inside fosfora. Reached
   through desargues.wallpaper.emit with :target :trama
   (doc/trama-layers.md).

   fosfora's preamble declares `u` (u.resolution, u.time, ..), `param(i)`
   and its own helpers. The emitted file declares none of them: only the
   manifest, a `// desargues: blend=..` hint, desargues_-prefixed helpers,
   desargues_field and fs_main.

   Slots (fosfora packs params by declaration order, and saved bindings
   and presets refer to slots, so the order is fixed and append-only):
     slot 0      loop_rate, the one \"rates\" param (default 1, 0..4); fosfora
                 hands the shader its running integral, so
                 phase = fract(param(0u) / period) loops seamlessly
     slot 1..15  the layer's :params in their declared (vector) order
   At most 16 slots; the manifest carries exactly the keys fosfora's
   deny_unknown_fields accepts.

   Stratified CPPB:
     Collect   `check`: emit/check plus the trama rules (id, slots, ranges).
     Promote   `fld/source :wgsl`: the field as WGSL.
     Pipeline  `manifest`, `source-text`: pure.
     Boundary  emit/emit! writes the file."
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [desargues.wallpaper.emit :as emit]
            [desargues.wallpaper.field :as fld]
            [desargues.wallpaper.wgsl :as wgsl]
            [hive-dsl.result :as r]))

(def max-slots
  "fosfora's scalar param slots per trama node."
  16)

(def rate-param
  "The rates param: always slot 0."
  {"type" "Float" "name" "loop_rate" "default" 1.0 "min" 0.0 "max" 4.0})

(defn trama-id
  "Layer id `id` as a trama id (- becomes _), or nil unless it is [a-z0-9_]+."
  [id]
  (let [s (str/replace (name id) "-" "_")]
    (when (re-matches #"[a-z0-9_]+" s) s)))

(defn- trama-err [k msg data]
  (r/err k (assoc data :message msg :target :trama)))

(defn check
  "Layer spec `layer` checked by emit/check and then by the trama rules,
   as a Result: err :trama/bad-id unless the id (- read as _) is
   [a-z0-9_]+; err :trama/too-many-params beyond 16 slots (loop_rate
   included); err :trama/bad-param for a name that is not snake_case
   [a-z][a-z0-9_]* or is loop_rate, or a param without :min and :max, or a
   :default outside them."
  [layer]
  (r/let-ok [l (emit/check layer)]
    (let [ps (:params l)
          pname #(str/replace (name %) "-" "_")
          bad-name (remove #(and (re-matches #"[a-z][a-z0-9_]*" (pname %)) (not= "loop_rate" (pname %)))
                           (map first ps))
          bad-range (remove (fn [[_ {:keys [default min max]}]] (and min max (<= min default max))) ps)]
      (cond
        (nil? (trama-id (:id l)))
        (trama-err :trama/bad-id "a trama id is the file stem, [a-z0-9_]+ (- is read as _)" {:id (:id l)})
        (> (inc (count ps)) max-slots)
        (trama-err :trama/too-many-params
                   (str "a trama node has " max-slots " slots: loop_rate plus at most " (dec max-slots) " params")
                   {:slots (inc (count ps)) :params (mapv first ps)})
        (seq bad-name)
        (trama-err :trama/bad-param "param names are snake_case [a-z][a-z0-9_]*, not loop_rate" {:bad (vec bad-name)})
        (seq bad-range)
        (trama-err :trama/bad-param "every trama param needs :min <= :default <= :max" {:bad (mapv first bad-range)})
        :else (r/ok (assoc l :trama/id (trama-id (:id l))))))))

(defn slots
  "Param symbol -> its slot index in checked layer `l`: loop_rate is 0, the
   declared params follow in vector order."
  [l]
  (into {} (map-indexed (fn [i [s _]] [s (inc i)])) (:params l)))

(defn manifest
  "The trama manifest of checked layer `l` (as data, keys in fosfora's
   order and ONLY its keys: name id kind inputs params rates integers)."
  [l]
  (array-map "name" (or (:title l) (:trama/id l))
             "id" (:trama/id l)
             "kind" "source"
             "inputs" 0
             "params" (into [rate-param]
                            (for [[s {:keys [default min max]}] (:params l)]
                              (array-map "type" "Float" "name" (str/replace (name s) "-" "_")
                                         "default" (double default) "min" (double min) "max" (double max))))
             "rates" ["loop_rate"]
             "integers" []))

(defn- slot-ref [k] (str "param(" k "u)"))

(defn fs-main
  "fs_main of checked layer `l`: pixel -> scene point as render fits the
   view (whole view visible, centred, y up, the longer axis extended),
   phase = fract(param(0u) / period), the field at the slots, clamped,
   opaque."
  [{:keys [phase period form] [vw vh] :view :as l}]
  (let [slot (slots l)
        args (for [p (:params form)] (if (= p phase) "desargues_phase" (slot-ref (slot p))))]
    (str "@fragment\n"
         "fn fs_main(@builtin(position) frag_coord: vec4f) -> @location(0) vec4f {\n"
         "  let desargues_res: vec2f = u.resolution;\n"
         "  let desargues_s: f32 = min(desargues_res.x / " (wgsl/literal vw) ", desargues_res.y / " (wgsl/literal vh) ");\n"
         "  let desargues_p: vec2f = vec2f(frag_coord.x - 0.5 * desargues_res.x, 0.5 * desargues_res.y - frag_coord.y) / desargues_s;\n"
         "  let desargues_phase: f32 = fract(" (slot-ref 0) " / " (wgsl/literal period) ");\n"
         "  let desargues_c: vec3f = " wgsl/function-name "(desargues_p.x, desargues_p.y"
         (apply str (map #(str ", " %) args)) ");\n"
         "  return vec4f(clamp(desargues_c, vec3f(0.0), vec3f(1.0)), 1.0);\n"
         "}\n")))

(defn- json-text [x]
  (json/write-str x :indent true :escape-slash false :escape-unicode false))

(defn source-text
  "The trama source of checked layer `l` with its compiled WGSL `src`
   ({:helpers :function}): manifest comment, blend hint, helpers, field,
   fs_main."
  [l src]
  (str "/*! trama\n" (json-text (manifest l)) "\n*/\n"
       "// desargues: blend=" (name (:blend l)) "\n"
       "// generated by desargues.wallpaper.trama; phase = fract(loop_rate / "
       (double (:period l)) "), view " (mapv double (:view l)) "\n\n"
       (when (seq (:helpers src)) (str (:helpers src) "\n"))
       (:function src) "\n"
       (fs-main l)))

(defn layer-files
  "{\"<id>.wgsl\" text} of layer spec `layer`, as a Result: the trama
   checks' err, or the :wgsl compiler's err naming what WGSL cannot say."
  [layer]
  (r/let-ok [l (check layer)
             src (fld/source :wgsl (:form l) {})]
    (r/ok (array-map (str (:trama/id l) ".wgsl") (source-text l src)))))

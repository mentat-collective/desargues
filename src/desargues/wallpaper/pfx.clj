(ns desargues.wallpaper.pfx
  "Field forms as fosfora LAYER EFFECTS: <id>.pfx (JSON) plus <id>.wgsl per
   layer, shown in fosfora's effect catalog. Reached through
   desargues.wallpaper.emit with :target :pfx (doc/pfx-effects.md). The
   WGSL body (helpers, desargues_field, fs_main) is the :trama one
   (desargues.wallpaper.trama); only the packaging and the rate slot differ.

   Slots (declaration order, append-only, as for trama):
     slot 0        loop_rate, its RAW value (a .pfx keeps the rate's own value)
     slot 1..N-1   the layer's :params in their declared (vector) order
     slot N        EXTRA: loop_rate's running integral, so
                   phase = fract(param(Nu) / period)
   N + 1 <= 16 slots, so at most 14 params.

   The display name (.pfx \"name\") is the catalog title and preset key; it
   must not equal one of fosfora's built-in effect names (`builtin-names`).

   Stratified CPPB:
     Collect   `check`: trama/check, the slot cap, the display name.
     Promote   `fld/source :wgsl`.
     Pipeline  `pfx-data`, `shader-text`: pure.
     Boundary  emit/emit! writes the two files."
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [desargues.wallpaper.field :as fld]
            [desargues.wallpaper.trama :as trama]
            [hive-dsl.result :as r]))

(def max-slots
  "fosfora's scalar param slots per layer effect, rate slots included."
  16)

(def builtin-names
  "fosfora's built-in effect names (fosfora session, 2026-11-08). A tessera
   effect may not take one of them: presets save effects BY NAME. \"Raster\"
   and \"Stress\" are two separate names (confirmed by the fosfora session)."
  ["Accretion" "Array" "Ascend" "Astrolabe" "Aurora" "Beam" "Bezel" "Cascade"
   "Chaos" "Chromatica" "Cleave" "Cymatics" "Drift" "Etch" "Fenestra" "Fluvid"
   "Flux" "Fosfora" "Frost" "Genesis" "Helix" "Intarsia" "Iris" "Lattice 445"
   "Lattice Brain" "Lattice Builder" "Lattice Chunky" "Lattice Clouds"
   "Lattice Pulse" "Lattice Pyroclastic" "Lattice Shells" "Limn" "Lumen"
   "Morph" "Murmur" "Mycelium" "Panorama" "Pegboard" "Plateau" "Polycephalum"
   "Prism" "Protea" "Pulse" "Raster" "Stress" "Reliquary" "Reticle" "Shards"
   "Splat" "Storm" "Strand" "Strata" "Sumi" "Symbiosis" "Tesla" "Tessera"
   "Tide" "Tunnel" "Turing" "Vessel"])

(defn- name-key [s] (str/lower-case (str/join " " (str/split (str/trim s) #"\s+"))))

(def ^:private builtin-keys (into #{} (map name-key) builtin-names))

(defn builtin-name?
  "True when display name `s` equals a fosfora built-in effect name,
   ignoring case and runs of whitespace."
  [s]
  (contains? builtin-keys (name-key s)))

(defn- pfx-err [k msg data]
  (r/err k (assoc data :message msg :target :pfx)))

(defn effect-meta
  "The .pfx metadata of layer `l`: {:name :author :description :category},
   from its :pfx map, falling back to :title (else the id), :description
   (else the title), author \"tessera\", category \"pattern\"."
  [{:keys [pfx title description category] :as l}]
  (let [nm (or (:name pfx) title (str/replace (name (:id l)) "-" "_"))]
    {:name nm
     :author (or (:author pfx) "tessera")
     :description (or (:description pfx) description title nm)
     :category (or (:category pfx) category "pattern")}))

(defn rate-slot
  "The extra slot holding loop_rate's running integral in checked layer
   `l`: after loop_rate and every packed param (each Float is 1 slot)."
  [l]
  (inc (count (:params l))))

(defn check
  "Layer spec `layer` checked by trama/check and then by the .pfx rules,
   as a Result: err :pfx/too-many-slots when loop_rate + the params + the
   extra rate slot exceed 16; err :pfx/bad-name for a blank display name;
   err :pfx/name-collision for a name equal to a fosfora built-in."
  [layer]
  (r/let-ok [l (trama/check layer)]
    (let [{nm :name :as em} (effect-meta l)
          used (inc (rate-slot l))]
      (cond
        (> used max-slots)
        (pfx-err :pfx/too-many-slots
                 (str "a layer effect has " max-slots " slots: loop_rate, the params and one extra rate slot,"
                      " so at most " (- max-slots 2) " params")
                 {:slots used :params (mapv first (:params l))})
        (not (and (string? nm) (seq (str/trim nm))))
        (pfx-err :pfx/bad-name "the display name must be a non-blank string" {:name nm})
        (builtin-name? nm)
        (pfx-err :pfx/name-collision "the display name is a fosfora built-in effect name" {:name nm})
        :else (r/ok (assoc l :pfx/meta em))))))

(defn pfx-data
  "The .pfx JSON of checked layer `l` (as data, exactly the keys name
   author description shader category inputs rates, in that order)."
  [l]
  (let [{:keys [name author description category]} (:pfx/meta l)]
    (array-map "name" name
               "author" author
               "description" description
               "shader" (str (:trama/id l) ".wgsl")
               "category" category
               "inputs" (get (trama/manifest l) "params")
               "rates" ["loop_rate"])))

(defn shader-text
  "The layer .wgsl of checked layer `l` with its compiled WGSL `src`: a
   plain comment, helpers, field, fs_main reading the phase from the
   extra rate slot. Shared trama/fs-main rounds :integer? controls;
   PFX has no integers UI metadata key. Float slots and JSON stay unchanged.
   No manifest comment, no blend hint."
  [l src]
  (str "// generated by desargues.wallpaper.pfx; phase = fract(param(" (rate-slot l) "u) / "
       (double (:period l)) "), the integral of loop_rate; view " (mapv double (:view l)) "\n\n"
       (when (seq (:helpers src)) (str (:helpers src) "\n"))
       (:function src) "\n"
       (trama/fs-main l (rate-slot l))))

(defn- json-text [x]
  (json/write-str x :indent true :escape-slash false :escape-unicode false))

(defn layer-files
  "{\"<id>.pfx\" json, \"<id>.wgsl\" text} of layer spec `layer`, as a
   Result: the checks' err, or the :wgsl compiler's err."
  [layer]
  (r/let-ok [l (check layer)
             src (fld/source :wgsl (:form l) {})]
    (let [id (:trama/id l)]
      (r/ok (array-map (str id ".pfx") (str (json-text (pfx-data l)) "\n")
                       (str id ".wgsl") (shader-text l src))))))

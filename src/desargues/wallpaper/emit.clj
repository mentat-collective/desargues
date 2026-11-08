(ns desargues.wallpaper.emit
  "Field forms as self-contained shader LAYERS for VJ hosts and compositors.

   A layer spec is data:

     {:id     :quasicrystal                 ; file stem
      :title  \"7-fold quasicrystal\"
      :form   field-form                    ; desargues.wallpaper.field
      :phase  u                             ; the param that is the loop phase (default u)
      :period 12                            ; seconds per loop when the phase is unbound
      :view   [14.22 8.0]                   ; scene window fitted to the output, centred
      :blend  :screen                       ; hint for the host's layer stack
      :params {scale {:default 3.0 :min 1.0 :max 8.0 :label \"Wave number\"}}
      :description \"..\" :categories [\"Generator\"]}

   Every non-phase param of the form needs a :default; :min :max :label
   are optional. The phase u is an input too: the shader runs at
     phase = fract(u + rate * TIME / period)
   so with the inputs at their defaults (u 0, rate 1) the layer loops
   seamlessly on its own clock; rate 0 hands the phase to u, bound by the
   host (a beat, an LFO). The view is fitted the way render fits it (the
   whole window visible, the longer axis extended), so a wider output
   (2560x1080) shows more of the plane, never a stretched picture.

   Stratified CPPB:
     Collect   `check`: the spec against LayerSpec (Malli), its params
               against the form's, as a Result.
     Promote   `inputs`: the bindable inputs; `glsl/source`: the field body.
     Pipeline  `isf`, `frag`, `manifest`: text and data, pure.
     Boundary  `emit!`: writes <id>.fs (ISF v2), <id>.frag + <id>.json.
   Each pure stage returns a hive-dsl Result; a field construct GLSL cannot
   express is an err naming it (desargues.wallpaper.glsl), never dropped."
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [desargues.wallpaper.field :as fld]
            [desargues.wallpaper.glsl :as glsl]
            [hive-dsl.result :as r]
            [malli.core :as m]
            [malli.error :as me]))

(def default-view
  "Manim's frame, 128/9 x 8 scene units: desargues.wallpaper.render's default."
  [(/ 128.0 9.0) 8.0])

(def blend-modes
  "Blend-mode hints a layer may carry (the names VJ hosts share)."
  #{:normal :add :screen :multiply :overlay :lighten :darken :difference})

(def ParamSpec
  [:map {:closed true}
   [:default number?]
   [:min {:optional true} number?]
   [:max {:optional true} number?]
   [:label {:optional true} string?]])

(def LayerSpec
  [:map
   [:id [:or keyword? string?]]
   [:form [:map [:params [:sequential symbol?]] [:body any?]]]
   [:title {:optional true} string?]
   [:description {:optional true} string?]
   [:categories {:optional true} [:sequential string?]]
   [:phase {:optional true} symbol?]
   [:period {:optional true} [:and number? pos?]]
   [:view {:optional true} [:tuple [:and number? pos?] [:and number? pos?]]]
   [:blend {:optional true} (into [:enum] blend-modes)]
   [:params {:optional true} [:map-of symbol? ParamSpec]]])

(def ^:private reserved
  "Names an input may not take: the ones the emitted main declares itself,
   ISF's built-ins, GLSL's keywords this code could meet."
  #{"rate" "phase" "TIME" "TIMEDELTA" "RENDERSIZE" "PASSINDEX" "DATE" "FRAMEINDEX"
    "fx" "fy" "px" "p" "s" "c" "main" "fragColor" "isf_FragNormCoord"
    "in" "out" "inout" "uniform" "float" "int" "bool" "vec2" "vec3" "vec4" "return"
    "if" "else" "for" "while" "do" "discard" "const" "struct" "true" "false" "highp" "precision"
    "sin" "cos" "tan" "exp" "log" "sqrt" "abs" "floor" "atan" "pow" "min" "max" "mod"
    "fract" "mix" "clamp" "smoothstep" "d_pow"})

(defn glsl-name
  "Param symbol `s` as a GLSL identifier (- becomes _), or nil when it is not one."
  [s]
  (let [n (str/replace (name s) "-" "_")]
    (when (and (re-matches #"[A-Za-z][A-Za-z0-9_]*" n)
               (not (str/starts-with? n "gl_")) (not (str/includes? n "__"))
               (not (reserved n)) (not (str/starts-with? n "desargues_")) (not (re-matches #"[vp][0-9]+" n)))
      n)))

(defn- layer-err [msg data]
  (r/err :layer/invalid (assoc data :message msg)))

(defn check
  "Layer spec `layer` with its defaults filled ({:phase u :period 10
   :view default-view :blend :normal}), as a Result: err :layer/invalid when
   it fails LayerSpec, names a phase the form lacks, leaves a non-phase
   param without a :default, binds a param the form lacks, or has a param
   whose name is no usable GLSL identifier."
  [layer]
  (if-let [ex (m/explain LayerSpec layer)]
    (layer-err "the layer spec does not match LayerSpec" {:explain (me/humanize ex)})
    (let [l (merge {:phase 'u :period 10 :view default-view :blend :normal :params {}} layer)
          ps (vec (get-in l [:form :params]))
          free (remove #{(:phase l)} ps)]
      (cond
        (not (some #{(:phase l)} ps))
        (layer-err (str "the form has no phase param " (:phase l)) {:params ps})
        (seq (remove (set free) (keys (:params l))))
        (layer-err "the spec binds params the form lacks" {:unknown (vec (remove (set free) (keys (:params l))))})
        (seq (remove (:params l) free))
        (layer-err "every non-phase param needs a :default" {:missing (vec (remove (:params l) free))})
        (some (complement glsl-name) free)
        (layer-err "param names must be GLSL identifiers, not reserved" {:bad (vec (remove glsl-name free))})
        (not= (count free) (count (distinct (map glsl-name free))))
        (layer-err "two params map to one GLSL name" {:params (vec free)})
        :else (r/ok l)))))

;; ── Promote: the bindable inputs ───────────────────────────────────────────

(defn inputs
  "The checked layer's inputs, in order: the phase, the loop rate, then
   the form's other params. Each {:name :role :default :min? :max? :label}."
  [{:keys [phase params form]}]
  (into [{:name (glsl-name phase) :role :phase :default 0.0 :min 0.0 :max 1.0
          :label "Phase (added to the clock)"}
         {:name "rate" :role :rate :default 1.0 :min 0.0 :max 4.0
          :label "Loops per period (0: phase only)"}]
        (for [p (:params form) :when (not= p phase)
              :let [{:keys [default min max label]} (params p)]]
          (cond-> {:name (glsl-name p) :role :param :default (double default)
                   :label (or label (name p))}
            min (assoc :min (double min))
            max (assoc :max (double max))))))

(defn- main-text
  "The main() shared by every variant: pixel -> scene point (render's
   fit), phase from the clock, the field, clamped. `io` names the
   variant's pixel coordinate, resolution, clock and output."
  [{:keys [phase period form] [vw vh] :view :as layer} {:keys [coord res time out]}]
  (let [args (for [p (:params form)] (if (= p phase) "desargues_phase" (glsl-name p)))]
    (str "void main() {\n"
         "  vec2 desargues_res = " res ";\n"
         "  float desargues_s = min(desargues_res.x / " (glsl/literal vw) ", desargues_res.y / " (glsl/literal vh) ");\n"
         "  vec2 desargues_p = (" coord " - 0.5 * desargues_res) / desargues_s;\n"
         "  float desargues_phase = fract(" (glsl-name phase) " + rate * " time " / " (glsl/literal period) ");\n"
         "  vec3 desargues_c = " glsl/function-name "(desargues_p.x, desargues_p.y"
         (apply str (map #(str ", " %) args)) ");\n"
         "  " out " = vec4(clamp(desargues_c, 0.0, 1.0), 1.0);\n"
         "}\n")))

(defn compiled
  "The checked layer spec `layer` with its GLSL: (ok layer + {:glsl
   {:helpers :function} :inputs [..]}), or the compiler's err."
  [layer]
  (r/let-ok [l (check layer)
             src (fld/source :glsl (:form l) {})]
    (r/ok (assoc l :glsl (select-keys src [:helpers :function]) :inputs (inputs l)))))

;; ── Pipeline: ISF v2 ───────────────────────────────────────────────────────

(defn- description [{:keys [title description blend period]}]
  (str (or description title "A desargues field layer")
       " [blend: " (name blend) "; loops every " period " s at rate 1]"))

(defn- meta-block
  "The desargues block shared by the ISF header and the manifest."
  [{:keys [id phase period view blend]}]
  (array-map "id" (name id) "blend" (name blend) "phase" (name phase)
             "period" (double period) "view" (mapv double view)
             "generator" "desargues.wallpaper.emit"))

(defn- isf-input [{:keys [name default min max label]}]
  (cond-> (array-map "NAME" name "TYPE" "float" "DEFAULT" default)
    min (assoc "MIN" min)
    max (assoc "MAX" max)
    true (assoc "LABEL" label)))

(defn isf-header
  "The ISF v2 JSON header (as data, keys in ISF order) of compiled layer `c`."
  [c]
  (array-map "ISFVSN" "2"
             "DESCRIPTION" (description c)
             "CREDIT" "desargues (github.com/mentat-collective/desargues)"
             "CATEGORIES" (vec (or (:categories c) ["Generator"]))
             "INPUTS" (mapv isf-input (:inputs c))
             "DESARGUES" (meta-block c)))

(defn- json-text [x]
  (json/write-str x :indent true :escape-slash false :escape-unicode false))

(defn isf
  "ISF v2 source of compiled layer `c` (see `compiled`): the JSON header
   comment, the helpers, the field function and a main reading
   isf_FragNormCoord, RENDERSIZE and TIME, writing gl_FragColor. The
   inputs are declared by the host, not here."
  [c]
  (str "/*" (json-text (isf-header c)) "*/\n\n"
       (when (seq (get-in c [:glsl :helpers])) (str (get-in c [:glsl :helpers]) "\n"))
       (get-in c [:glsl :function]) "\n"
       (main-text c {:coord "isf_FragNormCoord * RENDERSIZE" :res "RENDERSIZE"
                     :time "TIME" :out "gl_FragColor"})))

;; ── Pipeline: plain fragment shader + manifest ─────────────────────────────

(def versions
  "Plain-shader dialects: GLSL ES 3.00 (WebGL2, GLES) and desktop GLSL 330."
  {:es300 "#version 300 es\nprecision highp float;\n"
   :330   "#version 330 core\n"})

(defn frag
  "Plain fragment shader of compiled layer `c` in dialect `version` (a key
   of `versions`, default :es300): every input a float uniform, plus
   uniform vec2 RENDERSIZE (pixels) and uniform float TIME (seconds),
   gl_FragCoord as the pixel, out vec4 fragColor."
  ([c] (frag c :es300))
  ([c version]
   (str (versions version)
        "// " (name (:id c)) ": generated by desargues.wallpaper.emit; see " (name (:id c)) ".json\n"
        "uniform vec2 RENDERSIZE;\nuniform float TIME;\n"
        (apply str (for [{:keys [name]} (:inputs c)] (str "uniform float " name ";\n")))
        "out vec4 fragColor;\n\n"
        (when (seq (get-in c [:glsl :helpers])) (str (get-in c [:glsl :helpers]) "\n"))
        (get-in c [:glsl :function]) "\n"
        (main-text c {:coord "gl_FragCoord.xy" :res "RENDERSIZE" :time "TIME" :out "fragColor"}))))

(defn manifest
  "The JSON manifest (as data) beside a plain .frag of compiled layer `c`:
   its uniforms with roles, defaults and ranges, the clock and resolution
   uniforms, and the blend hint."
  [c]
  (array-map "title" (or (:title c) (name (:id c)))
             "description" (description c)
             "frag" (str (name (:id c)) ".frag")
             "dialects" ["300 es" "330 core"]
             "resolution" "RENDERSIZE"
             "time" "TIME"
             "uniforms" (mapv (fn [{:keys [role] :as in}]
                                (-> (isf-input in) (assoc "ROLE" (name role))
                                    (update-keys str/lower-case)))
                              (:inputs c))
             "desargues" (meta-block c)))

(defn layer-files
  "File name -> text of layer spec `layer`, as a Result:
     <id>.fs    ISF v2
     <id>.frag  GLSL ES 3.00 (WebGL2 / GLES)
     <id>.330.frag  desktop GLSL 330
     <id>.json  the manifest of the plain shaders."
  [layer]
  (r/let-ok [c (compiled layer)]
    (let [id (name (:id c))]
      (r/ok (array-map (str id ".fs") (isf c)
                       (str id ".frag") (frag c :es300)
                       (str id ".330.frag") (frag c :330)
                       (str id ".json") (str (json-text (manifest c)) "\n"))))))

;; ── Boundary ───────────────────────────────────────────────────────────────

(defn emit!
  "Write layer spec `layer` into directory `dir` (see `layer-files`).
   Returns (ok [path ..]) or the err that stopped it; nothing is written
   when the layer does not compile."
  [layer dir]
  (r/let-ok [files (layer-files layer)]
    (.mkdirs (io/file dir))
    (r/ok (vec (for [[n text] files :let [f (io/file dir n)]]
                 (do (spit f text) (str f)))))))

(defn emit-all!
  "Emit every layer spec of `layers` into `dir`: {id Result}."
  [layers dir]
  (into (array-map) (for [l layers] [(:id l) (emit! l dir)])))

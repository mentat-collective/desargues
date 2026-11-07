(ns desargues.wallpaper.luxcore
  "The :luxcore scene engine of desargues.wallpaper.render: a 3D scene map
   rendered by LuxCoreRender's luxcoreconsole, one process per frame.

   A scene is plain data:

     {:camera    {:orig [x y z] :target [x y z] :up [0 0 1] :fov 40}
      :materials {:gold {:type \"metal2\" :preset \"gold\" :roughness 0.08}}
      :objects   {:bar {:material :gold :vertices [[x y z] ..] :faces [[i j k] ..]}}
      :lights    {:sky {:type \"sky2\" :gain [1 1 1]}}      ; optional
      :spp 32 :seed 1 :engine \"PATHCPU\"}                  ; optional

   Material and light entries are LuxCore properties, written verbatim
   (vectors space-separated). Collect: `scene-props`, `config-props`.
   Promote: `props->text`, and the mesh builders `prism` and `ribbons`.
   Boundary: the `render/scene-render` :luxcore method, which runs
   `luxcoreconsole` (or $DESARGUES_LUXCORE) on files in a temp directory."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [desargues.wallpaper.render :as render])
  (:import (java.awt.image BufferedImage)
           (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)
           (javax.imageio ImageIO)))

(set! *warn-on-reflection* true)

;; ── Promote: properties ───────────────────────────────────────────────────

(defn- prop-value [v]
  (cond
    (keyword? v) (name v)
    (sequential? v) (str/join " " (map prop-value (flatten v)))
    (float? v) (format "%.6f" (double v))
    :else (str v)))

(defn props->text
  "LuxCore property text of [[key value] ..] pairs, one `key = value` per line."
  [pairs]
  (str/join "\n" (for [[k v] pairs] (str k " = " (prop-value v)))))

(defn- entries [prefix m]
  (for [[id props] (sort-by (comp str key) m)
        [k v] (sort-by (comp str key) props)]
    [(str prefix "." (name id) "." (name k)) v]))

(defn scene-props
  "The [key value] property pairs of scene map `s`."
  [{:keys [camera materials objects lights]}]
  (let [{:keys [orig target up fov] :or {up [0 0 1] fov 40}} camera]
    (concat
     [["scene.camera.lookat.orig" orig]
      ["scene.camera.lookat.target" target]
      ["scene.camera.up" up]
      ["scene.camera.fieldofview" fov]]
     (entries "scene.materials" materials)
     (entries "scene.lights" lights)
     (for [[id {:keys [material vertices faces]}] (sort-by (comp str key) objects)
           pair [[(str "scene.objects." (name id) ".material") material]
                 [(str "scene.objects." (name id) ".vertices") vertices]
                 [(str "scene.objects." (name id) ".faces") faces]]]
       pair))))

(defn config-props
  "The render.cfg property pairs for a `w` x `h` film written to `png`."
  [{:keys [spp seed engine] :or {spp 32 seed 1 engine "PATHCPU"}} w h png]
  [["scene.file" "scene.scn"]
   ["renderengine.type" engine]
   ["renderengine.seed" seed]
   ["sampler.type" "SOBOL"]
   ["film.width" w]
   ["film.height" h]
   ["film.outputs.1.type" "RGB_IMAGEPIPELINE"]
   ["film.outputs.1.filename" png]
   ["batch.haltspp" spp]])

;; ── Promote: meshes ───────────────────────────────────────────────────────

(defn prism
  "A closed convex-or-star polygon `pts` ([[x y] ..], counter-clockwise)
   extruded from z0 to z1, fanned from its centroid: {:vertices :faces}."
  [pts z0 z1]
  (let [n (count pts)
        [cx cy] (let [[sx sy] (reduce (fn [[a b] [x y]] [(+ a x) (+ b y)]) [0.0 0.0] pts)]
                  [(/ sx n) (/ sy n)])
        bottom (mapv (fn [[x y]] [x y z0]) pts)
        top    (mapv (fn [[x y]] [x y z1]) pts)
        vs (-> [] (into bottom) (into top) (conj [cx cy z1]))
        c (* 2 n)
        faces (concat
               (for [i (range n)] [c (+ n i) (+ n (mod (inc i) n))])
               (for [i (range n)
                     :let [j (mod (inc i) n)]
                     f [[i j (+ n j)] [i (+ n j) (+ n i)]]]
                 f))]
    {:vertices vs :faces (vec faces)}))

(defn- segment-box
  "Box of half-width `half` along segment a-b, lengthened by `half` at both
   ends so consecutive boxes overlap at their joint."
  [[x0 y0] [x1 y1] half z0 z1]
  (let [dx (- x1 x0) dy (- y1 y0) l (Math/hypot dx dy)
        ux (/ dx l) uy (/ dy l)
        x0 (- x0 (* half ux)) y0 (- y0 (* half uy))
        x1 (+ x1 (* half ux)) y1 (+ y1 (* half uy))
        nx (* half (- uy)) ny (* half ux)]
    (prism [[(- x0 nx) (- y0 ny)] [(- x1 nx) (- y1 ny)] [(+ x1 nx) (+ y1 ny)] [(+ x0 nx) (+ y0 ny)]]
           z0 z1)))

(defn merge-meshes
  "One {:vertices :faces} of several, face indices offset."
  [meshes]
  (reduce (fn [{:keys [vertices faces]} m]
            (let [o (count vertices)]
              {:vertices (into vertices (:vertices m))
               :faces (into faces (map (fn [f] (mapv #(+ o %) f)) (:faces m)))}))
          {:vertices [] :faces []}
          meshes))

(defn ribbons
  "Every segment of every polyline in `lines` as a box of width `w` from z0
   to z1, merged into one mesh."
  [lines w z0 z1]
  (merge-meshes (for [pts lines
                      [a b] (partition 2 1 pts)
                      :when (pos? (Math/hypot (- (first b) (first a)) (- (second b) (second a))))]
                  (segment-box a b (/ w 2.0) z0 z1))))

;; ── Boundary ──────────────────────────────────────────────────────────────

(defn console
  "The luxcoreconsole executable: $DESARGUES_LUXCORE or luxcoreconsole on PATH."
  []
  (or (System/getenv "DESARGUES_LUXCORE") "luxcoreconsole"))

(defn render-scene!
  "Render scene map `s` at `w` x `h` through luxcoreconsole; the image."
  ^BufferedImage [s w h]
  (let [dir (.toFile (Files/createTempDirectory "desargues-lux" (make-array FileAttribute 0)))
        png (io/file dir "frame.png")]
    (try
      (spit (io/file dir "scene.scn") (props->text (scene-props s)))
      (spit (io/file dir "render.cfg") (props->text (config-props s w h (.getPath png))))
      (let [proc (.start (doto (ProcessBuilder. ^java.util.List [(console) "render.cfg"])
                           (.directory dir)
                           (.redirectErrorStream true)))
            log (slurp (.getInputStream proc))
            code (.waitFor proc)]
        (when-not (and (zero? code) (.exists png))
          (throw (ex-info (str "luxcoreconsole failed (" code ")")
                          {:exit code :dir (.getPath dir) :log (subs log (max 0 (- (count log) 2000)))})))
        (ImageIO/read png))
      (finally
        (doseq [^java.io.File f (reverse (file-seq dir))] (.delete f))))))

(defmethod render/scene-render :luxcore [_ ^BufferedImage img s _opts]
  (let [frame (render-scene! s (.getWidth img) (.getHeight img))
        g (.createGraphics img)]
    (.drawImage g frame 0 0 nil)
    (.dispose g)
    img))

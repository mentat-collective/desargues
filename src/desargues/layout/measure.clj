(ns desargues.layout.measure
  "Python-free extent estimator: a `measure-fn [node path] -> [w h]` for
   `desargues.layout.core/resolve-layout`, counterpart to the exact
   `desargues.layout.realize/make-measure`. Units are manim scene units."
  (:require [desargues.layout.core :as core]))

(def default-metrics
  "Manim Text/MathTex metrics. :unit-per-pt = line-box height per font-size
   point; :advance-ratio = mean glyph advance / em; :math-widen = MathTex width
   factor; :line-height = inter-line multiple."
  {:unit-per-pt   0.0146
   :advance-ratio 0.52
   :math-widen    1.30
   :line-height   1.15})

(def ^:private default-font-size
  "Manim DEFAULT_FONT_SIZE, used when a leaf and all its ancestors set none."
  48.0)

(defn- text-extent
  "[w h] of one line of `content` at `font-size`, in scene units."
  [content font-size metrics]
  (let [n   (max 1 (count (str content)))
        upt (:unit-per-pt metrics)]
    [(* n font-size (:advance-ratio metrics) upt)
     (* font-size (:line-height metrics) upt)]))

(defn estimate
  "Estimate [w h] for leaf `node` under effective `style`. Meaningful for
   :text/:math/:image; else [0.0 0.0]. :image with no asset honors a declared
   :intrinsic-width/:intrinsic-height, else a unit square."
  ([node style] (estimate node style default-metrics))
  ([node style metrics]
   (let [fs (double (or (:font-size style) default-font-size))]
     (case (:type node)
       :text  (text-extent (:content node) fs metrics)
       :math  (let [[w h] (text-extent (:content node) fs metrics)]
                [(* w (:math-widen metrics)) h])
       :image (let [a (:attrs node)]
                [(double (or (:intrinsic-width a) 1.0))
                 (double (or (:intrinsic-height a) 1.0))])
       [0.0 0.0]))))

(defn make-measure
  "Build a measure-fn `[node path] -> [w h]` over the effective STYLES map
   (path -> style, from `core/collect-styles`)."
  ([styles] (make-measure styles default-metrics))
  ([styles metrics]
   (fn [node path]
     (estimate node (get styles path {}) metrics))))

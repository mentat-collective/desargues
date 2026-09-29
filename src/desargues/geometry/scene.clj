(ns desargues.geometry.scene
  "Solved configurations -> scene mobjects and play steps, through the
   backend-neutral desargues.scene facade. Thin by design: it emits exactly
   the EDN the RecordingBackend already records (:dot/:line/:text nodes,
   :play/:hold steps) — no new mobject kinds.

   Deck-runtime safe: depends only on the scene facade and
   desargues.geometry.projective. Solving (core.logic, Nelder-Mead) happens
   upstream, at dev/build time, in desargues.geometry.disposition / .fit;
   scenes here consume solved parameters as literals."
  (:require [desargues.geometry.projective :as g]
            [desargues.scene :as s]))

;; =============================================================================
;; Segments that cover what they draw
;; =============================================================================

(defn extend-to
  "Endpoints of the shortest segment on line PQ that covers all of pts,
   padded by pad on both ends."
  [P Q pts pad]
  (let [d (let [v (mapv - Q P) n (Math/hypot (v 0) (v 1))] (mapv #(/ % n) v))
        t (fn [X] (reduce + (map * (mapv - X P) d)))
        ts' (map t pts)
        lo (- (apply min ts') pad) hi (+ (apply max ts') pad)]
    [(mapv + P (mapv #(* lo %) d)) (mapv + P (mapv #(* hi %) d))]))

(defn config->segments
  "Named line segments over a point map. spec: {name {:base [k1 k2] :covers
  [ks...] :pad p}} — with :covers, the segment is the shortest one on the base
  line covering those points, padded; without, the plain base segment."
  [points spec]
  (into {}
        (map (fn [[name {:keys [base covers pad]}]]
               (let [a (points (base 0)) b (points (base 1))]
                 [name (if covers
                         (extend-to a b (map points covers) (or pad 0.3))
                         [a b])]))
        spec)))

;; =============================================================================
;; Points + segments -> mobjects (facade calls; EDN under RecordingBackend)
;; =============================================================================

(defn config->nodes
  "Mobjects for a solved configuration: one :dot per point, one :line per
   segment, optional :text labels placed next to their point's dot.

   dot-style  {k {:color c :radius r}}   (default {:color :white :radius 0.09})
   line-style {name {:color c :width w}} (default {:color :grey :width 2})
   labels     {k {:text s :direction dir :font-size n :color c}}

   Returns {:dots {k obj} :lines {name obj} :labels {k obj}}."
  [points segments & {:keys [dot-style line-style labels]
                      :or {dot-style {} line-style {} labels {}}}]
  (let [dots (into {}
                   (map (fn [[k p]]
                          (let [{:keys [color radius] :or {color :white radius 0.09}}
                                (dot-style k)]
                            [k (s/move-to (s/dot :color color :radius radius) p)])))
                   points)
        lines (into {}
                    (map (fn [[k [a b]]]
                           (let [{:keys [color width] :or {color :grey width 2}}
                                 (line-style k)]
                             [k (s/line a b :color color :width width)])))
                    segments)
        labels (into {}
                     (map (fn [[k {:keys [text direction font-size color]
                                   :or {direction :up font-size 24 color :white}}]]
                            [k (s/next-to (s/text text :font-size font-size :color color)
                                          (dots k)
                                          direction)]))
                     labels)]
    {:dots dots :lines lines :labels labels}))

;; =============================================================================
;; Gliding through solved keyframes
;; =============================================================================

(defn config->play-steps
  "Drive nodes through a sequence of solved keyframes: lerp the PARAMETERS
   (never the points), re-derive the configuration at every sample, glide
   every dot, reconnect every line. `derive`: params -> point map over the
   dot keys. Holds `hold` seconds after each leg."
  [stage nodes derive segment-spec frames
   & {:keys [samples run-time hold]
      :or {samples 14 run-time 0.1 hold 0.3}}]
  (doseq [[f0 f1] (partition 2 1 frames)]
    (doseq [i (range 1 (inc samples))]
      (let [pts (derive (g/lerp-params f0 f1 (/ (double i) samples)))
            segs (config->segments pts segment-spec)]
        (s/play! stage
                 (s/together
                  (concat
                   (for [[k d] (:dots nodes)] (s/glide d (pts k) :run-time run-time))
                   (for [[k l] (:lines nodes)]
                     (s/connect l (nth (segs k) 0) (nth (segs k) 1) :run-time run-time)))))))
    (s/hold! stage hold)))

(ns desargues.wallpaper-demo.hopf
  "Demo of desargues.wallpaper.space: the Hopf fibration as a seamless loop.

   S3 = {(z1, z2) : |z1|^2 + |z2|^2 = 1} fibres over S2 by (z1, z2) -> z1/z2;
   the fibre through a base point is the circle (e^{it} z1, e^{it} z2).
   Stereographic projection to R3 turns every fibre into a circle, and the
   fibres over one latitude of S2 into Villarceau circles on a torus
   |z1| = cos(eta): nested tori, each fibre linked once with every other.

   Motion, all periodic in u: beads flow along the fibres (t = 2 pi (u + m/M),
   an exact lap per loop); the 4D pose breathes by a rotation of S3 in the
   (x1, y2) plane through 0.35 sin(2 pi u) rad; the 3D picture turns once about
   z. The camera is fixed. Fibres are depth-cued strokes, beads are additive
   splats, bloom and vignette finish.

   Render the preview:  (render-preview!)  ->  /tmp/desargues-space/hopf.mp4"
  (:require [desargues.wallpaper.loop :as lp]
            [desargues.wallpaper.render :as r]
            [desargues.wallpaper.space :as sp]))

(def tau lp/tau)

(def etas
  "Latitudes of S2 (|z1| = cos eta), one nested torus each."
  [0.42 0.72 0.98])

(def per-ring
  "Fibres on each latitude circle."
  [10 14 18])

(def ring-rgb
  "Colour of each torus, inner to outer."
  [[1.0 0.55 0.25] [0.95 0.85 0.45] [0.4 0.75 1.0]])

(def cam
  (sp/camera {:eye [7.5 -7.5 4.2] :target [0.0 0.0 0.0] :up [0.0 0.0 1.0] :fov 29.0}))

(def cue
  {:range [7.0 15.0] :alpha [1.0 0.25] :size [1.0 0.55] :fog [0.0 0.6]})

(defn fibre-point
  "Point at fibre angle `t` of the fibre over (eta, phi) as [x1 y1 x2 y2] in S3."
  [eta phi t]
  (let [c (Math/cos eta) s (Math/sin eta)]
    [(* c (Math/cos t)) (* c (Math/sin t))
     (* s (Math/cos (+ t phi))) (* s (Math/sin (+ t phi)))]))

(defn pose
  "Rotate S3 point `q` in the (x1, y2) plane by `a`."
  [[x1 y1 x2 y2] a]
  (let [c (Math/cos a) s (Math/sin a)]
    [(- (* c x1) (* s y2)) y1 x2 (+ (* s x1) (* c y2))]))

(defn stereo
  "Stereographic projection from (0 0 0 1) to R3."
  [[x1 y1 x2 y2]]
  (let [k (/ 1.0 (max 0.12 (- 1.0 y2)))]
    [(* k x1) (* k y1) (* k x2)]))

(defn turn
  "R3 point `p` turned about z by 2 pi u."
  [p u]
  (sp/rotate p [0.0 0.0 1.0] (* tau u)))

(defn world
  "World position of fibre (eta, phi) at angle t, loop phase u."
  [eta phi t u]
  (-> (fibre-point eta phi t)
      (pose (* 0.35 (Math/sin (* tau u))))
      stereo
      ((fn [p] (sp/v* 0.9 p)))
      (turn u)))

(defn fibres
  "[{:ring i :eta e :phi p}] over every latitude circle."
  []
  (vec (for [[i eta] (map-indexed vector etas)
             :let [n (nth per-ring i)]
             j (range n)]
         {:ring i :eta eta :phi (+ (* tau (/ (double j) n)) (* 0.3 i))})))

(def all-fibres (fibres))

(defn fibre-polyline
  [{:keys [eta phi]} u]
  (mapv #(world eta phi (* tau (/ (double %) 160.0)) u) (range 161)))

(defn frame-fn
  "The fibres as depth-cued strokes, farthest first."
  [u]
  (sp/frame
   (into []
         (mapcat (fn [{:keys [ring] :as f}]
                   (sp/path-shapes cam (fibre-polyline f u)
                                   {:color (let [[cr cg cb] (nth ring-rgb ring)]
                                             (str "rgb(" (long (* 255 cr)) "," (long (* 255 cg)) "," (long (* 255 cb)) ")"))
                                    :width 2.2 :cue cue :opacity 0.55})))
         all-fibres)))

(def beads-per-fibre 5)

(defn bead-positions
  "[[point ring]] of every bead at phase u: one lap per loop."
  [u]
  (for [{:keys [ring eta phi]} all-fibres
        m (range beads-per-fibre)]
    [(world eta phi (* tau (+ (double u) (/ (double m) beads-per-fibre))) u) ring]))

(defn splat-fn
  "Glowing beads, depth-cued, additive."
  [u]
  (let [bs (bead-positions u)]
    (sp/splat-rows cam (mapv first bs)
                   {:radius 0.045 :intensity 1.6 :cue cue
                    :color (fn [i _] (nth ring-rgb (second (nth bs i))))
                    :fog-color [0.05 0.08 0.2]})))

(def spec
  {:id :hopf :title "Hopf fibration" :seconds 12 :fps 30 :size [1920 1080]
   :view r/default-view
   :frame frame-fn
   :splat splat-fn
   :exposure 0.9
   :blur {:samples 3 :shutter 0.6}
   :post (fn [_] [{:type :bloom :radius 14 :strength 0.9}
                  {:type :vignette :strength 0.55}])})

(defn render-preview!
  "A 640x360, 6 s preview at 20 fps."
  []
  (r/encode! :mp4 spec "/tmp/desargues-space/hopf.mp4"
             {:size [640 360] :fps 20 :seconds 6 :crf 22}))

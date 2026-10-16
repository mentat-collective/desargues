(ns desargues.objects.plot
  "PURE stratum: function graphs as parts over a CoordSystem.

   plot        y = f(x) over [a b]
   parametric  t -> [x y] over [t0 t1]
   polar       theta -> r over [th0 th1]

   Each samples adaptively (desargues.objects.sample) into one :path per
   continuous run, so poles split the curve instead of drawing a jump."
  (:require [desargues.objects.coords :as c]
            [desargues.objects.parts :as parts]
            [desargues.objects.sample :as sample]))

(def curve-style {:color :blue :width 4})

(defn plot
  "Graph of y = (f x) over `domain` [a b] (default: the x-range).
   opts: :color :width plus sampler opts :seed :tol :max-depth :max-jump."
  ([cs f] (plot cs f {}))
  ([cs f opts]
   (merge {:object :plot :coords cs :f f :domain (vec (take 2 (:x-range cs)))
           :max-jump (* 0.5 (:y-length cs))}
          curve-style opts)))

(defn parametric
  "Curve t -> [x y] (math coords) over `domain` [t0 t1]."
  ([cs f domain] (parametric cs f domain {}))
  ([cs f domain opts]
   (merge {:object :parametric :coords cs :f f :domain domain} curve-style opts)))

(defn polar
  "Polar curve theta -> r over `domain` (default [0 2pi])."
  ([cs r] (polar cs r {}))
  ([cs r opts]
   (merge {:object :polar :coords cs :f r :domain [0.0 (* 2 Math/PI)]} curve-style opts)))

(defn- curve-fn [{:keys [object coords f]}]
  (let [to-scene #(c/->scene coords %)]
    (case object
      :plot       (sample/graph-fn to-scene f)
      :parametric (sample/parametric-fn to-scene f)
      :polar      (sample/polar-fn to-scene f))))

(defn runs
  "The scene-point runs of a curve spec (plot, parametric or polar)."
  [{:keys [domain] :as spec}]
  (let [[a b] domain]
    (sample/sample-curve (curve-fn spec) a b
                         (select-keys spec [:seed :tol :max-depth :max-jump]))))

(defn- curve-parts [{:keys [color width] :as spec}]
  (vec (map-indexed (fn [i run] (parts/path [:run i] run {:color color :width width}))
                    (runs spec))))

(defmethod parts/parts :plot [spec] (curve-parts spec))
(defmethod parts/parts :parametric [spec] (curve-parts spec))
(defmethod parts/parts :polar [spec] (curve-parts spec))

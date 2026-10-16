(ns desargues.objects.calc.figure
  "COMPOSITION stratum: a calculus figure (desargues.objects.calc.core/figure)
   as a player ISource, animated by widget-driven params and the clock.

   frame-at = view furniture (axes) + every object's ->shapes, layered in
   order, + a title and a readout overlay, at params merged from the figure,
   the widgets and the clock (:animate {:param k :from a :to b :duration s}
   drives one param over time while the :play toggle is on). The readout
   shows each object's value through the numeric port (:port param, :jvm by
   default, :raster when available) and, when :exact is on and an exact
   method is registered (desargues.objects.calc.readout on the JVM), Emmy's
   closed form beside it.

   `source-fn` is what the stage opens: (fn [params] ISource). Host-free .cljc."
  (:require [clojure.string :as str]
            [desargues.objects.calc.core :as calc]
            [desargues.objects.calc.numeric :as num]
            [desargues.objects.calc.shapes :as sh]
            [desargues.objects.calc.shapes2d]
            [desargues.objects.calc.shapes3d]
            [desargues.player.frame :as frame]
            [desargues.player.protocols :as p]))

(defn fmt
  "`x` with 4 significant decimals, the same text on every host."
  [x]
  (if (number? x)
    (let [x (double x) c (Math/round (* 10000.0 (Math/abs x)))
          r (rem c 10000) frac (str r)]
      (str (when (and (neg? x) (pos? c)) "-") (quot c 10000) "."
           (subs "0000" (count frac)) frac))
    (str x)))

(defmulti exact
  "Emmy's exact text for resolved object `o` (a closed form or an exact
   rational), or nil. Methods are registered on the JVM by
   desargues.objects.calc.readout; ClojureScript has none (numeric only)."
  :kind)

(defmethod exact :default [_] nil)

(def value-names
  {:area-under "area" :area-between "area between" :riemann "sum" :tangent-line "slope f'(x0)"
   :secant "secant slope" :secant->tangent "secant slope" :accumulation "A(x)"
   :revolution "volume" :double-integral "integral" :line-integral "work"})

(defn readout-lines
  "The readout strings of figure `fig` at resolved `params`."
  [fig params]
  (let [port (num/resolve-port (:port params :jvm))]
    (into [(str "numeric port: " (name (num/-port-name port)))]
          (for [o (calc/resolved fig params)
                :let [v (calc/value o port)]
                :when v]
            (let [x (when (:exact params) (exact o))]
              (str (get value-names (:kind o) (name (:kind o)))
                   (when-let [extra (case (:kind o)
                                      :riemann (str " (" (name (:rule o)) ", n=" (long (:n o)) ")")
                                      :double-integral (str "  columns " (fmt (calc/column-sum o)))
                                      :revolution (str " (" (name (:method o)) ")")
                                      nil)]
                     extra)
                   " = " (fmt v) (when x (str "    exact: " x))))))))

(defn- overlay [fig params]
  (let [lines (readout-lines fig params)]
    (into [(sh/text "title" (:title fig) [0.0 3.6] {:size 34 :color :white})]
          (map-indexed (fn [i s] (sh/text (str "readout/" i) s [-6.9 (- 3.05 (* 0.42 i))]
                                          {:size 22 :color (if (zero? i) :grey :gold)
                                           :anchor "start"}))
                       lines))))

(defn params-at
  "The params of `fig` at time `t`: its own, the widgets', and the animated
   param when :play is on."
  [fig params t]
  (let [ps (merge (:params fig) params)
        {:keys [param from to duration]} (:animate fig)]
    (if (and param (:play ps true))
      (let [u (max 0.0 (min 1.0 (/ (double t) (double (or duration 1.0)))))
            v (+ (double from) (* u (- (double to) (double from))))]
        (assoc ps param v))
      ps)))

(defn frame-at
  "The frame of figure `fig` at `params` and time `t` (pure)."
  [fig params t]
  (let [ps   (params-at fig params t)
        view (:view fig)
        ctx  (sh/ctx view (:port ps :jvm))
        objs (calc/resolved fig ps)
        body (mapcat #(sh/->shapes % ctx) objs)]
    (cond-> (frame/frame t (concat (if (= :space (:view view)) [(sh/space-axes view)] (sh/plane-axes ctx))
                                   body
                                   (overlay fig ps)))
      (= :space (:view view)) (assoc :camera (:camera view)))))

(defrecord FigureSource [fig params]
  p/ISource
  (-duration [_] (double (get-in fig [:animate :duration] 0.0)))
  (-frame-at [_ t] (frame-at fig params t)))

(defn source-fn
  "(fn [params] ISource) of figure `fig` for desargues.player/open-source!."
  [fig]
  (fn [params] (->FigureSource fig params)))

(defn readout-text
  "The readout of `fig` at `params` and `t` as one string (what a check reads)."
  [fig params t]
  (str/join "\n" (readout-lines fig (params-at fig params t))))

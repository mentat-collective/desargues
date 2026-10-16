(ns desargues.tracks.core
  "CORE stratum of the tracks context: time tracks as exporter-neutral values.

   A Keyframe is {:t seconds} plus any of
     :transform     {:translate [x y z] :rotate [qw qx qy qz] :scale [sx sy sz]}
                    (every component optional; rotation as a unit quaternion)
     :shape-params  {param-key number-or-numeric-vector}
   A Track is the keyframes of ONE scene-graph node, ascending in :t, plus
   optional :meta (where it came from, badges such as :energy-drift).
   Tracks is {node-id Track}: the value a scene graph carries under :tracks,
   next to :nodes and :steps.

   Nothing here knows an exporter: MathBox playback, glTF animation channels
   and Blender keyframes are all projections of this one value (translate,
   rotate as a quaternion, scale and named scalar/vector params are exactly
   what each of them keys). Host-free .cljc, malli-first."
  (:require [malli.core :as m]
            [malli.error :as me]))

;; ── schemas ─────────────────────────────────────────────────────────────────

(def Num [:and number? [:fn {:error/message "must be finite"}
                        #(not (or (#?(:clj Double/isNaN :cljs js/isNaN) (double %))
                                  (#?(:clj Double/isInfinite :cljs (fn [x] (not (js/isFinite x)))) (double %))))]])

(def Vec3 [:tuple Num Num Num])

(def Quat [:tuple Num Num Num Num])

(def Transform
  [:map {:closed true}
   [:translate {:optional true} Vec3]
   [:rotate {:optional true} Quat]
   [:scale {:optional true} Vec3]])

(def ParamValue [:or Num [:vector Num]])

(def ShapeParams [:map-of :keyword ParamValue])

(def Keyframe
  [:and
   [:map
    [:t Num]
    [:transform {:optional true} Transform]
    [:shape-params {:optional true} ShapeParams]]
   [:fn {:error/message "a keyframe carries :transform or :shape-params"}
    #(or (contains? % :transform) (contains? % :shape-params))]])

(defn- ascending? [kfs]
  (every? (fn [[a b]] (< (:t a) (:t b))) (partition 2 1 kfs)))

(def Track
  [:and
   [:map
    [:keyframes [:vector {:min 1} Keyframe]]
    [:meta {:optional true} [:map-of :keyword any?]]]
   [:fn {:error/message "keyframe times must be strictly ascending"}
    #(ascending? (:keyframes %))]])

(def NodeId [:or :int :keyword :string])

(def Tracks [:map-of NodeId Track])

(def ^:private validators
  {::track (m/validator Track) ::tracks (m/validator Tracks) ::keyframe (m/validator Keyframe)})

(defn valid-track? [x] ((::track validators) x))
(defn valid-tracks? [x] ((::tracks validators) x))
(defn valid-keyframe? [x] ((::keyframe validators) x))

(defn explain-track
  "Humanized malli explanation of why `x` is not a Track, or nil."
  [x]
  (some-> (m/explain Track x) me/humanize))

;; ── constructors ────────────────────────────────────────────────────────────

(defn track
  "A Track from `keyframes` (sorted by :t here) and optional `meta`. Throws
   ex-info with the malli explanation when the result is not a Track. A Track
   is a plain map, not a record, so it round-trips as EDN inside a scene graph."
  ([keyframes] (track keyframes nil))
  ([keyframes meta]
   (let [t (cond-> {:keyframes (vec (sort-by :t keyframes))}
             (seq meta) (assoc :meta meta))]
     (when-not (valid-track? t)
       (throw (ex-info "not a Track" {:explain (explain-track t)})))
     t)))

(defn keyframes [trk] (:keyframes trk))

(defn start-time [trk] (:t (first (:keyframes trk))))

(defn end-time [trk] (:t (peek (:keyframes trk))))

(defn with-meta-entry
  "`trk` with meta key `k` set to `v` (a badge is a value on the track)."
  [trk k v]
  (assoc-in trk [:meta k] v))

(defn badge
  "The value of badge `k` on `trk`, or nil."
  [trk k]
  (get-in trk [:meta k]))

(ns desargues.bench.field
  "DESARGUES-WALLPAPER-GPU-FIELD bench: ms per 1920x1080 frame of the 7-wave
   quasicrystal (desargues.wallpaper.field/quasicrystal) on every
   field-raster backend, warm, interleaved (desargues.bench.ab):

     jvm-closure  the shader hand-written as a primitive fn, :jvm
     jvm-form     the field form evaluated to a primitive fn, :jvm
     raster       the form as a raster deftm over bands of rows, :raster
     gpu          the form as an OpenCL kernel, :gpu

   Each iteration renders one frame at a fresh phase (u advances), so no
   backend can reuse a frame. Needs -A:dynamics; :gpu needs an OpenCL
   device and is left out without one. Run: (run! {:ss 1})"
  (:require [desargues.backend :as backend]
            [desargues.bench.ab :as ab]
            [desargues.wallpaper.field :as fld]
            [desargues.wallpaper.render :as r]))

(def tau (* 2.0 Math/PI))

(defn closure
  "The quasicrystal at phase `u`, scale `k`, hand-written."
  [^double u ^double k]
  (let [dirs (double-array (mapcat (fn [i] (let [a (/ (* i Math/PI) 7.0)] [(Math/cos a) (Math/sin a)])) (range 7)))
        ph (* tau u)
        ch (fn ^double [^double t ^double off] (+ 0.5 (* 0.5 (Math/cos (* tau (+ t off))))))]
    (fn ^long [^double x ^double y]
      (let [s (loop [i 0 acc 0.0]
                (if (< i 7)
                  (recur (inc i) (+ acc (Math/cos (+ (* k (+ (* (aget dirs (* 2 i)) x) (* (aget dirs (inc (* 2 i))) y))) ph))))
                  acc))
            v (+ (* 0.5 (/ s 7.0)) 0.5)
            t (- v (Math/floor v))]
        (fld/pack (ch t 0.0) (ch t 0.33) (ch t 0.67))))))

(defn arm
  "The interleaved arm over `backends` (subset of the four variant names)."
  [{:keys [ss rounds warmup size variants]
    :or {ss 1 rounds 9 warmup 2 size [1920 1080]}}]
  (let [[w h] size
        i (atom 0)
        u (fn [] (/ (double (mod (swap! i inc) 360)) 360.0))
        img (r/image w h)
        opts {:ss ss}
        v (fn [nm f] {:name nm :run (fn [_] (f (u)))})
        all [(v "jvm-closure" #(r/field-raster :jvm img (closure % 3.0) opts))
             (v "jvm-form" #(r/field-raster :jvm img (fld/bind fld/quasicrystal % 3.0) opts))
             (v "raster" #(r/field-raster :raster img (fld/bind fld/quasicrystal % 3.0) opts))
             (v "gpu" #(r/field-raster :gpu img (fld/bind fld/quasicrystal % 3.0) opts))]]
    {:id (str "field " w "x" h " ss" ss)
     :rounds rounds :warmup warmup :alternate? true
     :variants (filterv (comp (set variants) :name) all)}))

(defn run!
  "Run the arm; returns ab/run-arm's report. opts: :ss :rounds :warmup :size."
  ([] (run! {}))
  ([opts]
   (let [gpu? (and (backend/available? :gpu)
                   ((requiring-resolve 'desargues.infrastructure.gpu-solver/device-available?)))
         names (cond-> ["jvm-closure" "jvm-form" "raster"] gpu? (conj "gpu"))]
     (try
       (ab/run-arm (arm (assoc opts :variants names)))
       (finally
         (when gpu? ((requiring-resolve 'desargues.infrastructure.raster-field/close-gpu!))))))))

(ns desargues.showcase
  "The README gallery, regenerated: every picture under doc/showcase/ comes
   from one entry of `entries`, rendered through the pure player and the
   headless PNG host (desargues.player.headless). No JavaFX, no Manim, no
   browser.

     clojure -M:showcase                      ; everything
     clojure -M:showcase --only plot,pendulum ; some entries
     clojure -M:showcase --list               ; ids, durations, namespaces

   An entry is plain data:

     {:id       :plot                    ; file name: doc/showcase/plot.png
      :title    \"plot\"                   ; caption title
      :ns       \"desargues.studio.objects-demos\"  ; where the construct lives
      :source   (fn [params] ISource)    ; e.g. (construct-source od/plot-demo)
      :params   {...}                    ; handed to :source
      :t        2.5 | :end               ; the still: the instant to paint
      :camera   {:yaw :pitch :distance}  ; 3D sources: merged over their own
      :frame-xf (fn [frame] frame)       ; optional touch-up of each frame
      :size     [w h]                    ; still size, default 960 x 540
      :gif      {:fps 12 :from 0.4 :to 6.0 :size [w h]          ; or
                 :fps 12 :seconds 4 :at (fn [u] {:t :params :camera})}}

   A still is written unless the entry has :gif and no :t. A GIF samples
   (:at u) for u from 0 to 1, or t from :from to :to; :at may vary params
   (each distinct params value is recorded once) or the camera. Adding a demo
   is adding one entry."
  (:require [clojure.java.io :as io]
            [clojure.java.shell :as sh]
            [clojure.string :as str]
            [desargues.logic.proof.library :as lib]
            [desargues.logic.scene.hasse :as hasse]
            [desargues.logic.scene.proof :as proof]
            [desargues.logic.scene.refute :as refute]
            [desargues.logic.scene.table :as table]
            [desargues.logic.scene.venn :as venn]
            [desargues.player :as player]
            [desargues.player.graph-source :as gs]
            [desargues.player.headless :as hl]
            [desargues.player.protocols :as p]
            [desargues.player.space-demos :as space-demos]
            [desargues.studio.demos :as demos]
            [desargues.studio.objects-demos :as od]
            [desargues.studio.widget-demos :as wd]
            [desargues.tex.label :as tl]))

(def out-dir "doc/showcase")

(def default-size [960 540])

;; ── Sources ──────────────────────────────────────────────────────────────────

(defn construct-source
  "(fn [params] ISource) for a parametric construct (fn [params] (fn [stage]
   ...)): recorded through the RecordingBackend, played as a graph source."
  [construct]
  (fn [params]
    (gs/graph-source (player/record "showcase" (construct params)))))

(def tex-scale
  "Logic flows size labels in px at 60 px per world unit; the player's :tex
   and :text sizes are Manim font sizes (about 120 px per unit). JLaTeXMath
   sets TeX a little wider than the latex+dvisvgm extents the flows lay out
   against, so TeX is scaled a little less than text."
  {:tex 1.75 :text 2.0})

(defn- tex-label->tex
  "A logic flow's TeX label (a :text node \"tex:...\", typeset by desargues.tex
   in the site build) as a :tex node the headless host typesets through
   JLaTeXMath; plain text is rescaled too."
  [nd]
  (cond (tl/tex-node? nd)
        (-> nd
            (assoc :node :tex :tex (tl/node-latex nd))
            (dissoc :text)
            (update-in [:opts :font-size] #(* (:tex tex-scale) (or % 24))))
        (= :text (:node nd)) (update-in nd [:opts :font-size] #(* (:text tex-scale) (or % 24)))
        :else nd))

(defn logic-source
  "(fn [_] ISource) for a logic flow: `record-fn` returns its recorded scene
   graph (desargues.logic.scene.*/record), whose TeX labels are typeset here."
  [record-fn]
  (fn [_]
    (gs/graph-source (update (record-fn) :nodes update-vals tex-label->tex))))

(defn- with-camera
  "ISource `src` seen through `cam` (merged over each frame's own camera)."
  [src cam]
  (if-not cam
    src
    (reify p/ISource
      (-duration [_] (p/-duration src))
      (-frame-at [_ t] (update (p/-frame-at src t) :camera merge cam)))))

;; ── The gallery ──────────────────────────────────────────────────────────────

(defn- objects-entry [k t caption & {:as more}]
  (merge {:id k :title (get-in od/demos [k :title]) :ns "desargues.studio.objects-demos"
          :source (construct-source (get-in od/demos [k :construct]))
          :params (od/defaults k) :t t :caption caption}
         more))

(defn- no-orbit-hint
  "The space demo's title tells a live viewer to drag; a still cannot."
  [f]
  (update f :shapes (partial mapv #(if (= "title" (:id %))
                                     (assoc % :text "a saddle and a curve on it")
                                     %))))

(def transitivity-proof
  "Pinter 1.11 v by element chasing (as in desargues.logic.proof-test)."
  '{:goal (implies (and (subset A B) (subset B C)) (subset A C))
    :lines [{:claim (subset A B) :by :hyp}
            {:claim (subset B C) :by :hyp}
            {:claim (forall [x] (implies (in x A) (in x B))) :by :def :from 0}
            {:claim (forall [y] (implies (in y B) (in y C))) :by :def :from 1}
            {:claim (implies (in x A) (in x B)) :by :inst :from 2 :with {x x}}
            {:claim (implies (in x B) (in x C)) :by :inst :from 3 :with {y x}}
            {:claim (implies (in x A) (in x C)) :by :use :result "1.1.5a"
             :with {P (in x A) Q (in x B) R (in x C)} :from [4 5]}
            {:claim (forall [x] (implies (in x A) (in x C))) :by :gen :from 6 :vars [x]}
            {:claim (subset A C) :by :def :from 7}
            {:claim (implies (and (subset A B) (subset B C)) (subset A C)) :by :taut :from [8]}]})

(def proof-library
  (lib/library '{"1.1.5a" (implies (and (implies P Q) (implies Q R)) (implies P R))}))

(def divisors-of-12
  (let [ds [1 2 3 4 6 12]]
    {:carrier (set ds) :order (set (for [x ds y ds :when (zero? (mod y x))] [x y]))}))

(def entries
  [;; physics
   {:id :pendulum :title "damped pendulum" :ns "desargues.studio.demos"
    :caption "RK4-integrated damped pendulum with a fading trail, recorded as a scene graph"
    :source (construct-source demos/pendulum)
    :params (into {} (map (juxt :id :value)) demos/pendulum-controls)
    :t 3.0
    :gif {:fps 12 :from 0.4 :to 6.4 :size [560 315]}}
   ;; 3D
   {:id :space :title "surface and space curve (3D)" :ns "desargues.player.space-demos"
    :caption "a saddle z = h(x^2 - y^2) with a spiral drawn on it, projected by the 2D host"
    :source space-demos/source-fn :params space-demos/defaults :t :end
    :frame-xf no-orbit-hint}
   {:id :space-top :title "the same surface from above" :ns "desargues.player.space-demos"
    :caption "same source, camera pitched to look down on the spiral"
    :source space-demos/source-fn :params space-demos/defaults :t :end
    :camera {:yaw -0.3 :pitch 1.2} :frame-xf no-orbit-hint}
   {:id :space-orbit :title "orbiting the saddle" :ns "desargues.player.space-demos"
    :caption "camera yaw swept once around the finished surface"
    :source space-demos/source-fn :params space-demos/defaults :frame-xf no-orbit-hint
    :gif {:fps 15 :seconds 4 :size [560 315]
          :at (fn [u] {:t :end :camera {:yaw (+ -0.9 (* 2 Math/PI u))}})}}
   ;; math objects (desargues.objects through the scene facade)
   (objects-entry :axes :end "number plane with a labelled point and its drop lines")
   (objects-entry :plot :end "y = A sin(kx + phi) next to tan x, adaptively sampled")
   (objects-entry :parametric :end "Lissajous curve (sin(at + delta), sin(bt))"
                  :gif {:fps 12 :from 1.2 :to 5.0 :size [560 315]})
   (objects-entry :polar :end "rose r = R cos(k theta) on a number plane"
                  :params {:petals 5.0 :radius 2.6})
   (objects-entry :field :end "vector field: rotation plus a source")
   (objects-entry :tangent :end "f(x) = x^3/3 - x with its tangent swept along and a secant")
   (objects-entry :riemann :end "Riemann rectangles of 1.2 + sin 2x refined to 4n, then the exact area")
   (objects-entry :tex :end "TeX labels typeset by JLaTeXMath in the headless host")
   ;; widget demos (buttons, select, text field), at interesting states
   {:id :turing :title "Turing machine" :ns "desargues.studio.widget-demos"
    :caption "3-state busy beaver after 9 of its 14 steps (Step / Run / Reset buttons)"
    :source (construct-source wd/turing-demo) :params {:steps 9} :t :end}
   {:id :kepler :title "Kepler's solids" :ns "desargues.studio.widget-demos"
    :caption "dodecahedron with its inscribed and circumscribed spheres (select, slider, toggle)"
    :source (construct-source wd/kepler-demo)
    :params {:solid :dodecahedron :turn 0.8 :spheres true} :t :end
    :gif {:fps 12 :seconds 4 :size [560 315]
          :at (fn [u] {:t :end :params {:solid :icosahedron :spheres true
                                        :turn (* 2 Math/PI u)}})}}
   {:id :goedel :title "Goedel numbering" :ns "desargues.studio.widget-demos"
    :caption "the formula Ex(x=s0) symbol by symbol and its Goedel number (text field)"
    :source (construct-source wd/goedel-demo) :params {:formula "Ex(x=s0)"} :t :end}
   ;; logic flows (desargues.logic.scene.*)
   {:id :venn :title "Venn flow" :ns "desargues.logic.scene.venn"
    :caption "A - B = B - A refuted: the regions that differ turn red"
    :source (logic-source #(venn/record "venn" '(= (diff A B) (diff B A)))) :t :end}
   {:id :truth-table :title "truth table" :ns "desargues.logic.scene.table"
    :caption "De Morgan's law filled column by column, a tautology"
    :source (logic-source #(table/record "table" '(iff (not (or P Q)) (and (not P) (not Q)))))
    :t :end}
   {:id :proof :title "proof flow" :ns "desargues.logic.scene.proof"
    :caption "transitivity of inclusion by element chasing, checked line by line"
    :source (logic-source #(proof/record "proof" transitivity-proof :library proof-library))
    :t :end}
   {:id :refute :title "refutation flow" :ns "desargues.logic.scene.refute"
    :caption "contraposition assumed false: every branch of the backwards truth table clashes"
    :source (logic-source #(refute/record "refute" '(iff (implies P Q) (implies (not Q) (not P)))))
    :t :end}
   {:id :hasse :title "Hasse diagram" :ns "desargues.logic.scene.hasse"
    :caption "divisors of 12 under divisibility: the covers, then the join and meet tables of a lattice"
    :source (logic-source #(hasse/record "hasse" divisors-of-12 {:subset #{4 6}})) :t :end}])

;; ── Rendering ────────────────────────────────────────────────────────────────

(def ^:private source-of
  "ISource of entry `e` at `params`, each distinct pair built once."
  (memoize (fn [e params] ((:source e) params))))

(defn- frame-of
  "The frame of entry `e` at {:t :params :camera} (t :end is its last instant)."
  [e {:keys [t params camera]}]
  (let [src (with-camera (source-of e (or params (:params e) {})) (or camera (:camera e)))
        dur (double (p/-duration src))
        t   (if (= :end t) (- dur 1e-3) (min (double t) dur))]
    ((or (:frame-xf e) identity) (p/-frame-at src (max 0.0 t)))))

(defn- write-frame! [e at [w h] path]
  (hl/write-png! (hl/frame->image (frame-of e at) w h) path))

(defn still!
  "Write entry `e`'s still to doc/showcase/<id>.png; returns the path."
  [e]
  (write-frame! e {:t (:t e)} (:size e default-size) (str out-dir "/" (name (:id e)) ".png")))

(defn- gif-samples [{:keys [fps from to seconds at] :or {fps 12}}]
  (let [secs (double (or seconds (- to from)))
        n    (max 2 (inc (long (Math/round (* secs fps)))))]
    (for [i (range n) :let [u (/ (double i) (dec n))]]
      (if at (at u) {:t (+ from (* u (- to from)))}))))

(defn- sh! [& args]
  (let [{:keys [exit err]} (apply sh/sh args)]
    (when-not (zero? exit) (throw (ex-info (str "failed: " (str/join " " args)) {:err err})))))

(defn gif!
  "Render entry `e`'s :gif to doc/showcase/<id>.gif through ffmpeg's two-pass
   palette (palettegen, then paletteuse); returns the path."
  [e]
  (let [{:keys [fps size] :or {fps 12 size [560 315]}} (:gif e)
        tmp (.toFile (java.nio.file.Files/createTempDirectory
                      "showcase" (make-array java.nio.file.attribute.FileAttribute 0)))
        out (str out-dir "/" (name (:id e)) ".gif")]
    (try
      (doseq [[i at] (map-indexed vector (gif-samples (:gif e)))]
        (write-frame! e at size (io/file tmp (format "f-%05d.png" i))))
      (sh! "ffmpeg" "-y" "-loglevel" "error" "-framerate" (str fps)
           "-i" (str tmp "/f-%05d.png")
           "-vf" (str "split[a][b];[a]palettegen=max_colors=96:stats_mode=diff[p];"
                      "[b][p]paletteuse=dither=bayer:bayer_scale=4:diff_mode=rectangle")
           "-loop" "0" out)
      out
      (finally
        (doseq [f (reverse (file-seq tmp))] (io/delete-file f true))))))

(defn render!
  "Write every output of entry `e`; returns the paths."
  [e]
  (cond-> []
    (or (:t e) (not (:gif e))) (conj (still! e))
    (:gif e)                   (conj (gif! e))))

(defn- kb [path] (format "%7.1f KB" (/ (.length (io/file path)) 1024.0)))

(defn -main [& args]
  (let [[flag v] args
        only (when (= "--only" flag) (set (map keyword (str/split v #","))))
        es   (filter #(or (nil? only) (only (:id %))) entries)]
    (if (= "--list" flag)
      (doseq [e es]
        (println (format "%-12s %6.2fs  %s" (name (:id e))
                         (double (p/-duration (source-of e (:params e {})))) (:ns e))))
      (doseq [e es path (render! e)]
        (println (kb path) path)))
    (shutdown-agents)))

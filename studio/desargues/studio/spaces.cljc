(ns desargues.studio.spaces
  "The 3D sources the stage opens by key: ISources (desargues.player.
   protocols), not constructs, each with its params and controls.

     :space     a saddle and a curve on it (desargues.player.space-demos)
     :calculus  partial derivatives of z = f(x, y): tangent plane, slices,
                gradient, level curves, animated (desargues.player.
                calculus-demos)

   The JavaFX stage (studio/desargues/studio/repl.clj, `open-space`) and the
   browser page (browser/desargues/browser/main.cljs, ?demo=<key>) open the
   same entries. Host-free .cljc."
  (:require [desargues.player.calculus-demos :as calculus]
            [desargues.player.space-demos :as space-demos]
            [desargues.objects.calc.demos :as calc-demos]))

(def demos
  "Key -> {:title :source-fn :params :controls}. The calculus figures
   (desargues.objects.calc.demos: :calc-area :calc-riemann :calc-between
   :calc-tangent :calc-ftc :calc-work :calc-solid :calc-columns) are sources
   too, 2D or 3D by their view."
  (merge
   {:space    {:title "saddle and curve (3D)" :source-fn space-demos/source-fn
               :params space-demos/defaults :controls space-demos/controls}
    :calculus {:title "partial derivatives (3D)" :source-fn calculus/source-fn
               :params calculus/defaults :controls calculus/controls}}
   calc-demos/entries))

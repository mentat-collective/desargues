(ns desargues.steps.viewers
  "BOUNDARY: Clerk viewers for worked solutions.

   The JVM side is the middleware: each viewer's :transform-fn runs the
   present pipeline (desargues.steps.present: Collect -> Promote ->
   Pipeline) and pushes finished markdown/TeX. The browser side only lays it
   out. `stepper` is the blackboard in a notebook: steps are revealed one at
   a time with previous / next / show all, and the boxed answer appears with
   the last step."
  (:require [desargues.steps.model :as model]
            [desargues.steps.present :as present]
            [nextjournal.clerk :as clerk]))

(defn derivation? [x] (instance? desargues.steps.model.Derivation x))

(defn- step-md [s] (clerk/md (present/->md (present/present s))))

(defn- answer-tex [d] (clerk/tex (str "\\boxed{" (present/present-form (:answer d)) "}")))

(def stepper
  "A Derivation, revealed step by step."
  {:name `stepper
   :pred derivation?
   :transform-fn (clerk/update-val (fn [d] (into [(answer-tex d)] (map step-md (:steps d)))))
   :render-fn
   '(fn [[answer & steps] opts]
      (reagent.core/with-let [n (reagent.core/atom 1)]
        (let [total (count steps)
              btn   (fn [label f]
                      [:button {:on-click f
                                :class "px-3 py-1 rounded border border-slate-500 text-sm hover:bg-slate-700"}
                       label])]
          [:div {:class "my-2"}
           (into [:div]
                 (map-indexed (fn [i s]
                                [:div {:class "flex gap-3 items-baseline my-1"}
                                 [:span {:class "font-mono text-slate-400 w-6"} (str (inc i) ".")]
                                 [:div {:class "flex-1 min-w-0"} [nextjournal.clerk.render/inspect-presented s]]])
                              (take @n steps)))
           (when (= @n total) [:div {:class "my-2"} [nextjournal.clerk.render/inspect-presented answer]])
           [:div {:class "flex gap-2 items-center mt-2"}
            (btn "◀ previous" #(swap! n (fn [k] (max 1 (dec k)))))
            (btn "next ▶" #(swap! n (fn [k] (min total (inc k)))))
            (btn "show all" #(reset! n total))
            [:span {:class "text-slate-400 text-sm"} (str @n " / " total)]]])))})

(defn show-steps
  "A Derivation through the stepper viewer."
  [d]
  (clerk/with-viewer stepper d))

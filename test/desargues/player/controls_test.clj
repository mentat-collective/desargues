(ns desargues.player.controls-test
  "Widget kinds (:slider :toggle :select :text :button): the pure control
   model, then each kind driven end to end through the player and a
   recording host with no window."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [desargues.player :as player]
            [desargues.player.controls :as c]
            [desargues.player.protocols :as p]
            [desargues.player.recording-host :as rh]))

(defn- int-or-error [s]
  (if-let [n (parse-long (str s))] n {:error "not an integer"}))

(def ^:private ctrls
  [{:id :a :label "a" :min 0.0 :max 1.0 :value 0.5}
   {:id :on :kind :toggle :label "on"}
   {:id :solid :kind :select :options [:cube :tetra {:value :octa :label "Octahedron"}]}
   {:id :n :kind :text :value "3" :parse int-or-error}
   {:id :step :kind :button :on-press #(update % :count (fnil inc 0))}
   {:id :preset :kind :button :set {:a 0.9 :on true}}
   {:id :pt :drag {:node "A"}}])

;; ── Pure model ────────────────────────────────────────────────────────

(deftest kinds
  (is (= [:slider :toggle :select :text :button :button :drag] (map c/kind ctrls)))
  (is (= [:a :on :solid :n :step :preset] (map :id (filter c/widget? ctrls)))))

(deftest validate-accepts-good-and-refuses-bad-specs
  (is (= ctrls (c/validate ctrls)))
  (testing "each kind reports its own problems"
    (is (seq (c/problems {:id :s :kind :slider :max 1})))
    (is (seq (c/problems {:id :s :kind :select :options []})))
    (is (seq (c/problems {:id :s :kind :select :options [:a] :value :b})))
    (is (seq (c/problems {:id :s :kind :button :on-press 3})))
    (is (seq (c/problems {:id :s :kind :button :set [1]})))
    (is (seq (c/problems {:id :s :kind :wheel}))))
  (testing "duplicate and missing ids"
    (is (thrown? clojure.lang.ExceptionInfo (c/validate [{:id :a :min 0 :max 1} {:id :a :kind :toggle}])))
    (is (thrown? clojure.lang.ExceptionInfo (c/validate [{:kind :toggle}])))))

(deftest defaults-per-kind
  (is (= {:a 0.5 :on false :solid :cube :n 3} (c/defaults ctrls))))

(deftest slider-input-clamps
  (let [s (first ctrls)]
    (is (= {:a 1.0} (:params (c/input s {} 7))))
    (is (= {:a 0.0} (:params (c/input s {} -2))))
    (is (:error (c/input s {} "x")))))

(defspec slider-always-lands-in-range 200
  (prop/for-all [x (gen/double* {:NaN? false :infinite? false})]
    (let [v (:a (:params (c/input (first ctrls) {} x)))]
      (<= 0.0 v 1.0))))

(deftest toggle-input
  (is (= {:on true} (:params (c/input (second ctrls) {} true))))
  (is (= {:on false} (:params (c/input (second ctrls) {:on true} nil)))))

(deftest select-input-and-options
  (let [s (nth ctrls 2)]
    (is (= [{:value :cube :label "cube"} {:value :tetra :label "tetra"}
            {:value :octa :label "Octahedron"}]
           (c/options s)))
    (is (= {:solid :octa} (:params (c/input s {} :octa))))
    (is (= {:params {:solid :cube} :error ":bogus is not an option"}
           (c/input s {:solid :cube} :bogus)))))

(deftest text-input-parses-or-refuses
  (let [s (nth ctrls 3)]
    (is (= {:n 42} (:params (c/input s {} "42"))))
    (is (= {:params {:n 3} :error "not an integer"} (c/input s {:n 3} "x")))
    (testing "a throwing parser refuses too"
      (is (:error (c/input (assoc s :parse #(Long/parseLong %)) {} "x"))))
    (testing "no parser: the string itself"
      (is (= {:t "hi"} (:params (c/input {:id :t :kind :text} {} "hi")))))
    (is (= "42" (c/display s 42)))))

(deftest button-steps-the-params
  (let [[step preset] (drop 4 ctrls)]
    (is (= {:count 1} (:params (c/input step {} nil))))
    (is (= {:count 3} (:params (c/input step {:count 2} nil))))
    (is (= {:a 0.9 :on true :x 1} (:params (c/input preset {:a 0.1 :x 1} nil))))
    (is (:error (c/input {:id :b :kind :button :on-press (constantly 3)} {} nil))
        "an on-press that returns no map is refused")))

(deftest step-reports-the-widgets-to-move
  (let [out (c/step ctrls {:a 0.1 :on false} :preset nil)]
    (is (= {:a 0.9 :on true} (:params out)))
    (is (= #{[:a 0.9] [:on true]} (set (:move out)))))
  (is (= [] (:move (c/step ctrls {:a 0.1} :a 0.3))) "the widget acted on is not moved")
  (is (:error (c/step ctrls {} :nope 1))))

(deftest widget-spec-shows-the-param
  (is (= 0.2 (:value (c/widget-spec (first ctrls) {:a 0.2}))))
  (is (= "7" (:value (c/widget-spec (nth ctrls 3) {:n 7}))))
  (is (every? map? (:options (c/widget-spec (nth ctrls 2) {})))))

(defmethod c/problems ::stepper [_] [])
(defmethod c/input ::stepper [{:keys [id]} params raw]
  {:params (update params id (fnil + 0) raw)})

(deftest a-new-kind-is-one-method-per-multimethod
  (is (= {:k 5} (:params (c/step [{:id :k :kind ::stepper}] {:k 2} :k 3))))
  (is (= [{:id :k :kind ::stepper}] (c/validate [{:id :k :kind ::stepper}]))))

;; ── End to end through a recording host ───────────────────────────────

(defn- source [ps]
  (reify p/ISource
    (-duration [_] 2.0)
    (-frame-at [_ t] {:t t :shapes [] :params ps})))

(defn- open []
  (let [h (rh/host)]
    [h (player/open-source! h source {:controls ctrls :autoplay? false})]))

(deftest the-host-draws-one-widget-per-kind
  (let [[h pl] (open)]
    (is (= [:a :on :solid :n :step :preset] (map :id (rh/widgets h))))
    (is (= {:a 0.5 :on false :solid :cube :n 3} (player/params pl)))
    (is (= "3" (rh/shown h :n)) "the text field shows the formatted value")))

(deftest each-kind-changes-the-picture
  (let [[h pl] (open)]
    (player/seek! pl 1.0)
    (testing "slider"
      (rh/user! h :a 0.25)
      (is (= 0.25 (get-in (player/current-frame pl) [:params :a])))
      (is (= 1.0 (:t (player/current-frame pl))) "time is kept"))
    (testing "toggle"
      (rh/user! h :on true)
      (is (true? (get-in (player/current-frame pl) [:params :on]))))
    (testing "select"
      (rh/user! h :solid :tetra)
      (is (= :tetra (:solid (player/params pl))))
      (rh/user! h :solid :bogus)
      (is (= :tetra (:solid (player/params pl))))
      (is (rh/error h :solid)))
    (testing "text: a bad formula is refused and reported, a good one clears it"
      (rh/user! h :n "seven")
      (is (= 3 (:n (player/params pl))))
      (is (= "not an integer" (rh/error h :n)))
      (rh/user! h :n "7")
      (is (= 7 (get-in (player/current-frame pl) [:params :n])))
      (is (nil? (rh/error h :n))))
    (testing "button: a discrete step holding state between presses"
      (rh/user! h :step nil)
      (player/press! pl :step)
      (is (= 2 (:count (player/params pl)))))
    (testing "preset button moves the other widgets"
      (rh/user! h :preset nil)
      (is (= [0.9 true] [(:a (player/params pl)) (:on (player/params pl))]))
      (is (= [0.9 true] [(rh/shown h :a) (rh/shown h :on)])))))

(deftest set-param-from-the-repl-moves-each-kind
  (let [[h pl] (open)]
    (player/set-param! pl :n 11)
    (player/set-param! pl :on true)
    (player/set-param! pl :solid :octa)
    (is (= ["11" true :octa] [(rh/shown h :n) (rh/shown h :on) (rh/shown h :solid)]))))

(deftest the-recording-host-ticks-by-hand
  (let [h (rh/host)
        pl (player/open-source! h source {:controls ctrls})]
    (rh/tick! h 0.5)
    (is (= 0.5 (:t (player/current-frame pl))))))

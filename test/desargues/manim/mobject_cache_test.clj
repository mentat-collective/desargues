(ns desargues.manim.mobject-cache-test
  "IMobjectCache: the pure LRU contract over a JVM value store (always runs),
   the pure layout keys (always runs), and the Python store plus realize with
   a cache against Manim (skips without a Manim env)."
  (:require [clojure.test :refer [deftest is testing]]
            [desargues.config :as config]
            [desargues.layout.core :as l]
            [desargues.layout.mobject-key :as mk]
            [desargues.manim.mobject-cache :as mc]
            [malli.core :as m]))

(def manim-env?
  (delay (try (config/manim-ready?) (catch Throwable _ false))))

;; A mutable JVM object stands in for a mobject: copying must give an
;; independent one.
(defn- box-copy [a] (atom @a))

(defn- value-cache [n] (mc/lru-cache {:max-entries n :store (mc/value-store box-copy)}))

(deftest records-construct-without-manim
  (is (some? (mc/null-cache)))
  (is (some? (mc/lru-cache)) "lru-cache with the Python store builds no Python")
  (is (some? (mc/python-store))))

(deftest null-cache-always-builds
  (let [c (mc/null-cache) n (atom 0)
        build #(do (swap! n inc) {:mobject (atom :x) :meta 1})]
    (is (false? (mc/active? c)))
    (is (false? (:hit? (mc/fetch! c :k build))))
    (is (false? (:hit? (mc/fetch! c :k build))))
    (is (= 2 @n))
    (is (nil? (mc/kept-meta c :k)))
    (is (m/validate mc/CacheStatsSchema (mc/stats c)))))

(deftest lru-hit-builds-once-and-keeps-meta
  (let [c (value-cache 4) n (atom 0)
        build #(do (swap! n inc) {:mobject (atom {:v 1}) :meta {:w 2.0}})
        a (mc/fetch! c [:k 1] build)
        b (mc/fetch! c [:k 1] build)]
    (is (= 1 @n))
    (is (false? (:hit? a)))
    (is (true? (:hit? b)))
    (is (= {:w 2.0} (:meta b) (mc/kept-meta c [:k 1])))
    (is (= {:v 1} @(:mobject b)))
    (is (= {:hits 1 :misses 1 :size 1 :max 4} (mc/stats c)))
    (is (m/validate mc/CacheStatsSchema (mc/stats c)))))

(deftest copies-are-independent
  (let [c (value-cache 4)
        built (atom {:color :white})
        first-out (:mobject (mc/fetch! c :k (fn [] {:mobject built})))
        second-out (:mobject (mc/fetch! c :k (fn [] (throw (ex-info "rebuilt" {})))))
        third-out (:mobject (mc/fetch! c :k (fn [] (throw (ex-info "rebuilt" {})))))]
    (testing "the build handed out on a miss is not the kept one"
      (swap! first-out assoc :color :red)
      (is (= {:color :white} @second-out)))
    (testing "mutating one hit copy affects neither the kept build nor another copy"
      (swap! second-out assoc :color :gold)
      (is (= {:color :white} @third-out))
      (is (= {:color :white} @(:mobject (mc/fetch! c :k (fn [] (throw (ex-info "rebuilt" {}))))))))
    (is (not (identical? second-out third-out)))))

(deftest lru-evicts-least-recently-used
  (let [c (value-cache 2)
        put (fn [k] (mc/fetch! c k (fn [] {:mobject (atom k) :meta k})))]
    (put :a) (put :b)
    (put :a)                            ; :a most recent
    (put :c)                            ; evicts :b
    (is (= :a (mc/kept-meta c :a)))
    (is (nil? (mc/kept-meta c :b)))
    (is (= 2 (:size (mc/stats c))))
    (mc/clear! c)
    (is (= {:hits 0 :misses 0 :size 0 :max 2} (mc/stats c)))))

(deftest bad-build-is-refused
  (is (thrown? clojure.lang.ExceptionInfo
               (mc/fetch! (value-cache 2) :k (fn [] {:not-a-mobject 1})))))

;; ── keys ──────────────────────────────────────────────────────────────────

(deftest leaf-keys-are-descriptions
  (let [t (l/text "cell" {:font-size 30})]
    (is (= (mk/leaf-key t {:font-size 30} [:s]) (mk/leaf-key (l/text "cell" {:font-size 30}) {:font-size 30} [:s])))
    (is (not= (mk/leaf-key t {:font-size 30} [:s]) (mk/leaf-key (l/text "cell!" {:font-size 30}) {:font-size 30} [:s])))
    (is (not= (mk/leaf-key t {:font-size 30} [:s]) (mk/leaf-key t {:font-size 31} [:s])) "style is in the key")
    (is (not= (mk/leaf-key t {} [:a]) (mk/leaf-key t {} [:b])) "the backend stamp is in the key")))

(deftest nodes-outside-the-description-are-not-keyed
  (is (nil? (mk/leaf-key {:type :image :content "a.png"} {} nil)))
  (is (nil? (mk/subtree-key (l/box {:id :hero} (l/text "x")) {} [1 1 0] nil)))
  (is (nil? (mk/subtree-key (l/row {} (l/text "a") (l/box {:id :k} (l/text "b"))) {} [1 1 0] nil)))
  (is (some? (mk/subtree-key (l/row {} (l/text "a")) {} [1 1 0] nil))))

;; ── Manim ─────────────────────────────────────────────────────────────────

(defn- py [sym] (requiring-resolve sym))

(deftest python-store-copies-are-independent
  (if-not @manim-env?
    (is true "skipped: no Manim env")
    (do
      ((py 'desargues.manim.core/init!))
      (let [c      (mc/lru-cache {:max-entries 4})
            call   (py 'libpython-clj2.python/call-attr)
            ->jvm  (py 'libpython-clj2.python/->jvm)
            getx   (fn [mob] (first (->jvm (call mob "get_center"))))
            build  (fn [] {:mobject ((py 'desargues.manim.mobjects/rectangle) :width 1.0 :height 1.0)})
            a      (:mobject (mc/fetch! c :sq build))
            b      (:mobject (mc/fetch! c :sq build))
            d      (:mobject (mc/fetch! c :sq build))]
        (call a "shift" ((py 'desargues.manim.core/->py-list) [3.0 0.0 0.0]))
        (call b "shift" ((py 'desargues.manim.core/->py-list) [5.0 0.0 0.0]))
        (is (< (Math/abs (- 3.0 (getx a))) 1e-9))
        (is (< (Math/abs (- 5.0 (getx b))) 1e-9))
        (is (< (Math/abs (getx d)) 1e-9) "a later copy is the kept original, unmoved")
        (is (= 2 (:hits (mc/stats c))))))))

(def ^:private tree
  (l/column {:width :fill :height :fill :padding 0.5 :spacing 0.2}
    (l/text "Title" {:font-size 36})
    (l/row {:width :fill :spacing 0.2}
      (l/box {:width [:portion 1] :border :white} (l/text "a"))
      (l/box {:width [:portion 1] :border :white} (l/math "x^2")))))

(deftest realize-with-cache-matches-cold-points
  (if-not @manim-env?
    (is true "skipped: no Manim env")
    (do
      ((py 'desargues.manim.core/init!))
      (let [realize (py 'desargues.layout.realize/realize)
            call    (py 'libpython-clj2.python/call-attr)
            ->jvm   (py 'libpython-clj2.python/->jvm)
            pts     (fn [L] (->jvm (call (call (:vgroup L) "get_all_points") "tolist")))
            c       (mc/lru-cache)
            cold    (pts (realize tree))
            first*  (pts (realize tree {:mobject-cache c}))
            warm    (pts (realize tree {:mobject-cache c}))]
        (is (= cold first* warm))
        (is (pos? (:hits (mc/stats c))))))))

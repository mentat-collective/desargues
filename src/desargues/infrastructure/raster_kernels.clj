(ns desargues.infrastructure.raster-kernels
  "Whole integrations as raster deftm kernels: the state derivative AND the
   step loop run inside raster, with no Clojure call per step.

   Two sources of derivatives:
   - an Emmy :primitive kernel source (what desargues.videos.lagrangian
     compiles, kept on the system as :kernel-source), lowered to raster's
     vocabulary the way the board kernels lower Emmy expressions: Emmy does
     the algebra, raster runs every number;
   - the hand-written damped pendulum (desargues.videos.physics/Pendulum).

   Every JVM kernel performs the same IEEE operations in the same order as
   the pure array path (physics/rk4-trajectory-arrays and rk4-step-doubles),
   so a kernel trajectory equals the array trajectory bit for bit; the tests
   prove it. The Emmy kernels also compile to WebAssembly (wasm-kernel), with
   sin/cos from desargues.kernel.libm. Only the adapters
   desargues.infrastructure.raster-adapter and .wasm-runtime require this
   namespace, and raster is on the classpath only under the :dynamics alias.

   Kernel ABI (fixed-step RK4, batched):
     (rk4 u0s nb out steps dt) -> nb
   u0s holds nb initial states of n doubles back to back; out receives, per
   trajectory b, steps+1 rows of n doubles starting at b*(steps+1)*n (row 0
   is the initial state). The pendulum kernel takes two more arrays, gl and
   mu, the per-trajectory g/L and damping, so one call sweeps parameters. The
   wasm kernel takes three work arrays more (see rk4-wasm-form)."
  (:require [clojure.walk :as walk]
            [desargues.kernel :as kernel]
            [desargues.kernel.libm :as libm]
            [raster.arrays]
            [raster.compiler.pipeline :as pl]
            [raster.core :refer [deftm]]
            [raster.par]))

;; ── Emmy :primitive source -> raster vocabulary ─────────────────────────────

(def ^:private op-map
  '{clojure.core/+ + clojure.core/* * clojure.core/- - clojure.core// /})

(defn lower-emmy-kernel
  "Emmy's :primitive state-derivative source
     (fn [in out ps] (let [bindings] (doto out (aset k expr) ..)))
   over the local tuple in = [t q.. qdot..] -> {:bindings [[sym expr] ..]
   :stores [[j expr] ..]} over a state array named `s` (in[k] reads s[k-1],
   in[0] the time reads 0.0: the systems are autonomous), out[k] becomes
   store j = k-1 and out[0] (dt/dt) is dropped. clojure.core arithmetic
   becomes raster's typed +,*,-,/; integer literals become doubles, the
   value Clojure's mixed arithmetic would use anyway. Reads the source
   through desargues.kernel/source->ir, like the JVM backend.

   Bottom-up (postwalk), so the integer index of an emitted (aget s j) is
   never revisited: the JVM coerces a double index silently, wasm rejects it."
  [src]
  (let [{:keys [in bindings] :as ir} (kernel/source->ir src)
        fix (fn [form]
              (walk/postwalk
               (fn [x]
                 (cond
                   (and (seq? x) (= 'clojure.core/aget (first x)) (= in (second x)))
                   (let [k (long (nth x 2))] (if (zero? k) 0.0 (list 'aget 's (dec k))))
                   (and (seq? x) (op-map (first x))) (cons (op-map (first x)) (rest x))
                   (and (number? x) (not (double? x))) (double x)
                   :else x))
               form))]
    {:bindings (mapv (fn [[sym e]] [sym (fix e)]) bindings)
     :stores (vec (keep (fn [[k e]] (when (pos? (long k)) [(dec (long k)) (fix e)])) (kernel/stores ir)))}))

(def chunk-size
  "Let bindings per generated deftm: the spherical pendulum's 1588 bindings in
   one method exceed HotSpot's huge-method limit and run interpreted (4x
   slower than the array path); 60-binding chunks JIT."
  60)

(defn deriv-forms
  "deftm forms defining `nm` [s ds scratch] -> Long, which fills ds with the
   derivative at s. Beyond chunk-size bindings the let is split
   (desargues.kernel/chunk) into chunk deftms that hand their locals over
   through scratch (one slot per binding): same expressions on the same
   values, so the same results."
  [nm {:keys [bindings stores]}]
  (let [sig '[s :- (Array double) ds :- (Array double) scratch :- (Array double)]
        load (fn [slots syms] (mapcat (fn [sym] [sym (list 'aget 'scratch (slots sym))]) syms))
        write-ds (fn [] (map (fn [[j e]] (list 'aset 'ds j e)) stores))]
    (if (<= (count bindings) chunk-size)
      [(list 'raster.core/deftm nm sig :- 'Long
             (list 'let (vec (mapcat identity bindings)) (cons 'do (concat (write-ds) [0]))))]
      (let [{:keys [slots chunks tail-needs]} (kernel/chunk bindings (map second stores) chunk-size)
            names (mapv #(symbol (str nm "-chunk" %)) (range (count chunks)))]
        (concat
         (map (fn [nmi {:keys [needs bindings]}]
                (list 'raster.core/deftm nmi sig :- 'Long
                      (list 'let (vec (concat (load slots needs) (mapcat identity bindings)))
                            (cons 'do (concat (map (fn [[sym _]] (list 'aset 'scratch (slots sym) sym)) bindings)
                                              [0])))))
              names chunks)
         [(list 'raster.core/deftm nm sig :- 'Long
                (cons 'do (concat (map (fn [c] (list c 's 'ds 'scratch)) names)
                                  [(list 'let (vec (load slots tail-needs))
                                         (cons 'do (concat (write-ds) [0])))])))])))))

(defn rk4-form
  "deftm `nm` (u0s nb out steps dt) -> nb: batched fixed-step RK4 over n-dim
   states with derivative deftm `d` [s ds scratch]. The operation order is
   physics/rk4-trajectory-arrays': tmp = s + k*h, c = ((k1 + 2k2) + 2k3) + k4,
   s = s + c*(dt/6)."
  [nm d n nscratch]
  `(raster.core/deftm ~nm [~'u0s :- (~'Array ~'double) ~'nb :- ~'Long ~'out :- (~'Array ~'double)
                           ~'steps :- ~'Long ~'dt :- ~'Double] :- ~'Long
     (let [~'s (double-array ~n)
           ~'k1 (double-array ~n) ~'k2 (double-array ~n) ~'k3 (double-array ~n) ~'k4 (double-array ~n)
           ~'tmp (double-array ~n) ~'scratch (double-array ~(max 1 nscratch))
           ~'h2 (/ ~'dt 2.0) ~'h6 (/ ~'dt 6.0)
           ~'rows (+ ~'steps 1)]
       (loop [~'b 0]
         (if (< ~'b ~'nb)
           (let [~'ob (* (* ~'b ~'rows) ~n)]
             (dotimes [~'j ~n]
               (let [~'v (aget ~'u0s (+ (* ~'b ~n) ~'j))]
                 (aset ~'s ~'j ~'v)
                 (aset ~'out (+ ~'ob ~'j) ~'v)))
             (loop [~'i 0]
               (when (< ~'i ~'steps)
                 (~d ~'s ~'k1 ~'scratch)
                 (dotimes [~'j ~n] (aset ~'tmp ~'j (+ (aget ~'s ~'j) (* (aget ~'k1 ~'j) ~'h2))))
                 (~d ~'tmp ~'k2 ~'scratch)
                 (dotimes [~'j ~n] (aset ~'tmp ~'j (+ (aget ~'s ~'j) (* (aget ~'k2 ~'j) ~'h2))))
                 (~d ~'tmp ~'k3 ~'scratch)
                 (dotimes [~'j ~n] (aset ~'tmp ~'j (+ (aget ~'s ~'j) (* (aget ~'k3 ~'j) ~'dt))))
                 (~d ~'tmp ~'k4 ~'scratch)
                 (let [~'base (+ ~'ob (* (+ ~'i 1) ~n))]
                   (dotimes [~'j ~n]
                     (let [~'c (+ (+ (+ (aget ~'k1 ~'j) (* 2.0 (aget ~'k2 ~'j))) (* 2.0 (aget ~'k3 ~'j))) (aget ~'k4 ~'j))
                           ~'v (+ (aget ~'s ~'j) (* ~'c ~'h6))]
                       (aset ~'s ~'j ~'v)
                       (aset ~'out (+ ~'base ~'j) ~'v))))
                 (recur (+ ~'i 1))))
             (recur (+ ~'b 1)))
           ~'nb)))))

(defn rk4-wasm-form
  "deftm `nm` (u0s nb out steps dt st s ks) -> nb: the batched RK4 of
   rk4-form in the shape raster's wasm backend compiles. Its v1 emitter takes
   param-only kernels, so the work buffers are parameters (st, s: n doubles;
   ks: 4n), and the derivative (`lowered`, from lower-emmy-kernel) is inlined
   ONCE, inside a four-stage loop: stage i evaluates it at s and stores k_i at
   ks[i*n]. s is the state for stage 0 and s + k_{i-1}*c after, c = dt/2, dt/2,
   dt, so the operations and their order are rk4-form's. Every (Math/sin x),
   (Math/cos x) is rewritten by desargues.kernel.libm, because raster lowers
   its own to a minimax good only to 5e-6; those are within 2^-53 of Math, so
   this kernel tracks rk4-form to rounding rather than bit for bit."
  [nm {:keys [bindings stores]} n]
  (let [deriv `(let ~(vec (mapcat (fn [[sym e]] [sym (libm/with-precise-trig e)]) bindings))
                 ~@(map (fn [[j e]] (list 'aset 'ks (list '+ 'ko j) (libm/with-precise-trig e))) stores))]
    `(raster.core/deftm ~nm [~'u0s :- (~'Array ~'double) ~'nb :- ~'Long ~'out :- (~'Array ~'double)
                             ~'steps :- ~'Long ~'dt :- ~'Double
                             ~'st :- (~'Array ~'double) ~'s :- (~'Array ~'double) ~'ks :- (~'Array ~'double)] :- ~'Long
       (let [~'h2 (/ ~'dt 2.0) ~'h6 (/ ~'dt 6.0) ~'rows (+ ~'steps 1)]
         (loop [~'b 0]
           (if (< ~'b ~'nb)
             (let [~'ob (* (* ~'b ~'rows) ~n)]
               (dotimes [~'j ~n]
                 (let [~'v (aget ~'u0s (+ (* ~'b ~n) ~'j))]
                   (aset ~'st ~'j ~'v)
                   (aset ~'out (+ ~'ob ~'j) ~'v)))
               (loop [~'i 0]
                 (when (< ~'i ~'steps)
                   (dotimes [~'stage 4]
                     (if (< ~'stage 1)
                       (dotimes [~'j ~n] (aset ~'s ~'j (aget ~'st ~'j)))
                       (let [~'c (if (> ~'stage 2) ~'dt ~'h2) ~'off (* (- ~'stage 1) ~n)]
                         (dotimes [~'j ~n] (aset ~'s ~'j (+ (aget ~'st ~'j) (* (aget ~'ks (+ ~'off ~'j)) ~'c))))))
                     (let [~'ko (* ~'stage ~n)] ~deriv))
                   (let [~'base (+ ~'ob (* (+ ~'i 1) ~n))]
                     (dotimes [~'j ~n]
                       (let [~'c (+ (+ (+ (aget ~'ks ~'j) (* 2.0 (aget ~'ks (+ ~n ~'j))))
                                       (* 2.0 (aget ~'ks (+ ~(* 2 n) ~'j))))
                                    (aget ~'ks (+ ~(* 3 n) ~'j)))
                             ~'v (+ (aget ~'st ~'j) (* ~'c ~'h6))]
                         (aset ~'st ~'j ~'v)
                         (aset ~'out (+ ~'base ~'j) ~'v))))
                   (recur (+ ~'i 1))))
               (recur (+ ~'b 1)))
             ~'nb))))))

(defn- upto
  "(loop [i 0] (when (< i n) body.. (recur (+ i 1)))): dotimes spelled out.
   raster's OpenCL emitter turns dotimes' (long n) into a call to an
   undefined long_, so GPU kernels loop explicitly."
  [i n & body]
  (list 'loop [i 0] (list 'when (list '< i n) (cons 'do (concat body [(list 'recur (list '+ i 1))])))))

(defn rk4-par-form
  "deftm `nm` (u0s out st s ks steps dt nb) -> Void: the batched RK4 of
   rk4-form as ONE raster.par/map-void! over trajectories, the shape raster
   compiles to a GPU kernel (one work item per initial condition). Work item
   b keeps its state, stage input and stages in slices of the work buffers
   (st, s: nb*n; ks: nb*4n), and evaluates the derivative (`lowered`, from
   lower-emmy-kernel, its (aget s k) shifted to b's slice) inlined once in a
   four-stage loop, in rk4-form's operation order. On the JVM map-void! is a
   sequential loop and the result is rk4-form's bit for bit; on a GPU the
   device's sin/cos and its fused multiply-adds round differently."
  [nm {:keys [bindings stores]} n]
  (let [shift (fn [e] (walk/postwalk
                       (fn [x] (if (and (seq? x) (= 'aget (first x)) (= 's (second x)))
                                 (list 'aget 's (list '+ 'so (nth x 2)))
                                 x))
                       e))
        deriv `(let ~(vec (mapcat (fn [[sym e]] [sym (shift e)]) bindings))
                 ~@(map (fn [[j e]] (list 'aset 'ks (list '+ 'ko j) (shift e))) stores))
        at (fn [base j] (list '+ base j))]
    `(raster.core/deftm ~nm [~'u0s :- (~'Array ~'double) ~'out :- (~'Array ~'double)
                             ~'st :- (~'Array ~'double) ~'s :- (~'Array ~'double) ~'ks :- (~'Array ~'double)
                             ~'steps :- ~'Long ~'dt :- ~'Double ~'nb :- ~'Long] :- ~'Void
       (raster.par/map-void! ~'b ~'nb
         (let [~'h2 (/ ~'dt 2.0) ~'h6 (/ ~'dt 6.0) ~'rows (+ ~'steps 1)
               ~'ob (* (* ~'b ~'rows) ~n) ~'so (* ~'b ~n) ~'kb (* ~'b ~(* 4 n))]
           ~(upto 'j n
                  `(let [~'v (aget ~'u0s ~(at 'so 'j))]
                     (aset ~'st ~(at 'so 'j) ~'v)
                     (aset ~'out ~(at 'ob 'j) ~'v)))
           (loop [~'i 0]
             (when (< ~'i ~'steps)
               ~(upto 'stage 4
                      `(if (< ~'stage 1)
                         ~(upto 'j n `(aset ~'s ~(at 'so 'j) (aget ~'st ~(at 'so 'j))))
                         (let [~'c (if (> ~'stage 2) ~'dt ~'h2) ~'off (+ ~'kb (* (- ~'stage 1) ~n))]
                           ~(upto 'j n `(aset ~'s ~(at 'so 'j)
                                              (+ (aget ~'st ~(at 'so 'j)) (* (aget ~'ks ~(at 'off 'j)) ~'c))))))
                      `(let [~'ko (+ ~'kb (* ~'stage ~n))] ~deriv))
               (let [~'base (+ ~'ob (* (+ ~'i 1) ~n))]
                 ~(upto 'j n
                        `(let [~'c (+ (+ (+ (aget ~'ks (+ ~'kb ~'j)) (* 2.0 (aget ~'ks (+ ~'kb (+ ~n ~'j)))))
                                         (* 2.0 (aget ~'ks (+ ~'kb (+ ~(* 2 n) ~'j)))))
                                      (aget ~'ks (+ ~'kb (+ ~(* 3 n) ~'j))))
                               ~'v (+ (aget ~'st ~(at 'so 'j)) (* ~'c ~'h6))]
                           (aset ~'st ~(at 'so 'j) ~'v)
                           (aset ~'out ~(at 'base 'j) ~'v))))
               (recur (+ ~'i 1)))))))))

(defn rhs-form
  "An ftm right-hand side (du u t) for raster.ode's solvers, calling deriv
   deftm `d` directly: no Clojure fn and no state map per evaluation."
  [d nscratch]
  `(let [~'scratch (double-array ~(max 1 nscratch))]
     (raster.core/ftm [~'du :- (~'Array ~'double), ~'u :- (~'Array ~'double), ~'t :- ~'Double]
       (~d ~'u ~'du ~'scratch))))

;; ── compiling a system's kernels (once per kernel source) ───────────────────

(def ^:private compiled
  "Emmy kernel source -> {:n :rk4 :rhs-fn}. The source is the value
   lagrangian-system already caches under its :cache-key, so the same system
   reaches the same entry."
  (atom {}))

(defn- build [src]
  (let [lowered (lower-emmy-kernel src)
        n (count (:stores lowered))
        nscratch (count (:bindings lowered))
        id (str (gensym "k"))
        d (symbol (str "deriv-" id))
        rk (symbol (str "rk4-" id))]
    (binding [*ns* (the-ns 'desargues.infrastructure.raster-kernels)]
      (doseq [f (deriv-forms d lowered)] (eval f))
      (eval (rk4-form rk d n nscratch))
      {:n n
       :rk4 @(ns-resolve *ns* rk)
       :deriv @(ns-resolve *ns* d)
       :nscratch nscratch
       :rhs-fn (eval `(fn [] ~(rhs-form d nscratch)))})))

(defn kernels
  "The compiled raster kernels for Emmy kernel source `src` (see ns doc)."
  [src]
  (or (get @compiled src)
      (let [k (build src)]
        (get (swap! compiled (fn [c] (if (contains? c src) c (assoc c src k)))) src))))

(def ^:private compiled-wasm
  "Emmy kernel source -> its wasm module (see wasm-kernel)."
  (atom {}))

(defn wasm-kernel
  "The batched RK4 of Emmy kernel source `src` as a WebAssembly module (f64,
   no imports), compiled once per source:
     {:bytes byte[] :export \"rk4\" :n n :work [n n (* 4 n)]}
   The export is (rk4 u0s nb out steps dt st s ks) -> nb, every array a byte
   offset into the module's exported `memory` and nb, steps i32: the ABI of
   rk4-form plus the work buffers st, s, ks whose sizes :work gives (see
   rk4-wasm-form). The host lays out the arrays, grows memory to fit, calls
   the export and reads out back; any wasm engine runs it (V8 in a browser,
   Chicory on the JVM via desargues.infrastructure.wasm-runtime)."
  [src]
  (or (get @compiled-wasm src)
      (let [lowered (lower-emmy-kernel src)
            n (count (:stores lowered))
            nm (symbol (str "rk4-wasm-" (gensym "k")))
            v (binding [*ns* (the-ns 'desargues.infrastructure.raster-kernels)]
                (eval (rk4-wasm-form nm lowered n)))
            module (pl/compile-wasm v :name "rk4" :dtype :double)
            k {:bytes (byte-array (:bytes module)) :export "rk4" :n n :work [n n (* 4 n)]}]
        (get (swap! compiled-wasm (fn [c] (if (contains? c src) c (assoc c src k)))) src))))

(def ^:private compiled-par
  "Emmy kernel source -> {:var :n}, its rk4-par-form deftm."
  (atom {}))

(defn gpu-kernel
  "The rk4-par-form deftm of Emmy kernel source `src`, defined once per
   source: {:var :n}. raster.gpu/compile! turns :var into a device kernel;
   on the JVM it is an ordinary (sequential) deftm."
  [src]
  (or (get @compiled-par src)
      (let [lowered (lower-emmy-kernel src)
            n (count (:stores lowered))
            nm (symbol (str "rk4-par-" (gensym "k")))
            v (binding [*ns* (the-ns 'desargues.infrastructure.raster-kernels)]
                (eval (rk4-par-form nm lowered n)))]
        (get (swap! compiled-par (fn [c] (if (contains? c src) c (assoc c src {:var v :n n})))) src))))

(deftm pendulum-par-rk4
  "pendulum-rk4 as one raster.par/map-void! over trajectories (a GPU kernel):
   work item b integrates theta' = omega, omega' = -(g/L) sin theta - mu omega
   from u0s[2b..2b+1] with its own gl[b] and mu[b], in pendulum-rk4's
   operation order."
  [u0s :- (Array double) gl :- (Array double) mu :- (Array double)
   out :- (Array double) steps :- Long dt :- Double nb :- Long] :- Void
  (raster.par/map-void! b nb
    (let [h2 (/ dt 2.0) h6 (/ dt 6.0) rows (+ steps 1)
          g (aget gl b) m (aget mu b) ob (* (* b rows) 2)
          th0 (aget u0s (* b 2)) om0 (aget u0s (+ (* b 2) 1))]
      (aset out ob th0)
      (aset out (+ ob 1) om0)
      (loop [i 0 th th0 om om0]
        (when (< i steps)
          (let [k1a om k1b (- (- (* g (Math/sin th))) (* m om))
                t2 (+ th (* k1a h2)) o2 (+ om (* k1b h2))
                k2a o2 k2b (- (- (* g (Math/sin t2))) (* m o2))
                t3 (+ th (* k2a h2)) o3 (+ om (* k2b h2))
                k3a o3 k3b (- (- (* g (Math/sin t3))) (* m o3))
                t4 (+ th (* k3a dt)) o4 (+ om (* k3b dt))
                k4a o4 k4b (- (- (* g (Math/sin t4))) (* m o4))
                nth (+ th (* (+ (+ (+ k1a (* 2.0 k2a)) (* 2.0 k3a)) k4a) h6))
                nom (+ om (* (+ (+ (+ k1b (* 2.0 k2b)) (* 2.0 k3b)) k4b) h6))
                base (+ ob (* (+ i 1) 2))]
            (aset out base nth)
            (aset out (+ base 1) nom)
            (recur (+ i 1) nth nom)))))))

;; ── the damped pendulum ─────────────────────────────────────────────────────

(deftm pendulum-rk4
  "Batched RK4 for theta' = omega, omega' = -(g/L) sin theta - mu omega, the
   operations of physics/Pendulum's derivatives in rk4-step-doubles' order.
   gl and mu hold one g/L and one damping per trajectory."
  [u0s :- (Array double) nb :- Long gl :- (Array double) mu :- (Array double)
   out :- (Array double) steps :- Long dt :- Double] :- Long
  (let [h2 (/ dt 2.0) h6 (/ dt 6.0) rows (+ steps 1)]
    (loop [b 0]
      (if (< b nb)
        (let [g (aget gl b) m (aget mu b) ob (* (* b rows) 2)
              th0 (aget u0s (* b 2)) om0 (aget u0s (+ (* b 2) 1))]
          (aset out ob th0)
          (aset out (+ ob 1) om0)
          (loop [i 0 th th0 om om0]
            (when (< i steps)
              (let [k1a om k1b (- (- (* g (Math/sin th))) (* m om))
                    t2 (+ th (* k1a h2)) o2 (+ om (* k1b h2))
                    k2a o2 k2b (- (- (* g (Math/sin t2))) (* m o2))
                    t3 (+ th (* k2a h2)) o3 (+ om (* k2b h2))
                    k3a o3 k3b (- (- (* g (Math/sin t3))) (* m o3))
                    t4 (+ th (* k3a dt)) o4 (+ om (* k3b dt))
                    k4a o4 k4b (- (- (* g (Math/sin t4))) (* m o4))
                    nth (+ th (* (+ (+ (+ k1a (* 2.0 k2a)) (* 2.0 k3a)) k4a) h6))
                    nom (+ om (* (+ (+ (+ k1b (* 2.0 k2b)) (* 2.0 k3b)) k4b) h6))
                    base (+ ob (* (+ i 1) 2))]
                (aset out base nth)
                (aset out (+ base 1) nom)
                (recur (+ i 1) nth nom))))
          (recur (+ b 1)))
        nb))))

(deftm pendulum-deriv
  "The pendulum derivative as a raster RHS body: du = f(u), params gl, mu."
  [du :- (Array double) u :- (Array double) gl :- Double mu :- Double] :- Long
  (let [th (aget u 0) om (aget u 1)]
    (aset du 0 om)
    (aset du 1 (- (- (* gl (Math/sin th))) (* mu om)))
    0))

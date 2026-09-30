(ns desargues.board
  "Boards: interactive calculus frames, compiled by raster, rendered by plato.

   A board spec names a function of one variable over a few slider
   parameters, as an S-expression:

     {:id     :wave
      :f      '(+ (* a (sin (* b x))) (* c (* x x)))
      :var    'x
      :params [{:id 'a :min -2 :max 2 :step 0.01 :init 1} ...]
      :window {:x [-4 4] :y [-3 3] :n 401}}

   `compile-board!` does the calculus once, at build time, on the JVM:
   raster.sym.diff differentiates f, and f, f' and the running integral of f
   (trapezoid) are spliced into ONE `deftm` array kernel. raster compiles the
   kernel to a WebAssembly module with no imports, written to disk; the SAME
   kernel runs on the JVM to sample the initial frame. The result is plain
   data, the board value, which plato renders: a static plot from the frame
   for a page without JavaScript, and a live board (plato's board island)
   that calls the wasm export on every drag.

   Kernel ABI, shared with plato.board-island:
     (export xs ys dys iys n x0 h p1 p2 ...) -> n
   xs..iys are byte offsets of four f64 arrays of length n in the module's
   exported `memory`; params come in the order of :params.

   Requires raster on the classpath (the :dynamics alias), like
   desargues.infrastructure.raster-adapter; nothing else in desargues loads
   this namespace."
  (:require [clojure.java.io :as io]
            [clojure.walk :as walk]
            [raster.compiler.pipeline :as pl]
            [raster.core :refer [deftm]]
            [raster.math]
            [raster.sym.diff :as diff]))

;; ---------------------------------------------------------------------------
;; The kernel

(def ^:private math-ops
  "Bare transcendentals, qualified so deftm dispatches them through
   raster.math, which the wasm backend lowers to inline polynomials."
  {'sin 'raster.math/sin 'cos 'raster.math/cos 'tan 'raster.math/tan
   'exp 'raster.math/exp 'log 'raster.math/log 'pow 'raster.math/pow
   'sqrt 'raster.math/sqrt 'sinh 'raster.math/sinh 'cosh 'raster.math/cosh
   'tanh 'raster.math/tanh})

(defn- lower [expr] (walk/postwalk #(get math-ops % %) expr))

(defn derivative
  "f' as raster's simplified S-expression."
  [{:keys [f var] :or {var 'x}}]
  (diff/differentiate f var))

(defn kernel-form
  "The deftm source of a board's kernel, named `kname`.

   Two constraints of raster's wasm backend (v0.2.258) shape the body:
   - no Long*Double arithmetic: `(* i h)` emits f64.convert_i32_s on an
     operand already f64, an invalid module. So the variable is carried
     through the loop as a Double and the step h is a parameter; the Long
     index is only compared and incremented.
   - no type for a bare nil, so the kernel returns the sample count rather
     than Void."
  [kname {:keys [f var params] :or {var 'x} :as spec}]
  (let [ps (mapv (comp symbol name :id) params)]
    `(deftm ~kname
       (~'All [~'T]
        [~'xs :- (~'Array ~'T) ~'ys :- (~'Array ~'T)
         ~'dys :- (~'Array ~'T) ~'iys :- (~'Array ~'T)
         ~'n :- ~'Long ~'x0 :- ~'Double ~'h :- ~'Double
         ~@(mapcat (fn [p] [p :- 'Double]) ps)] :- ~'Long
        (loop [~'i 0 ~var ~'x0 ~'acc 0.0 ~'prev 0.0]
          (if (< ~'i ~'n)
            (let [~'y ~(lower f)
                  ~'acc (if (> ~'i 0) (+ ~'acc (* 0.5 ~'h (+ ~'prev ~'y))) 0.0)]
              (aset ~'xs ~'i ~var)
              (aset ~'ys ~'i ~'y)
              (aset ~'dys ~'i ~(lower (derivative spec)))
              (aset ~'iys ~'i ~'acc)
              (recur (+ ~'i 1) (+ ~var ~'h) ~'acc ~'y))
            ~'i))))))

(defn- define-kernel!
  "Evaluate the kernel into this namespace; returns its var."
  [{:keys [id] :as spec}]
  (let [kname (symbol (str (name id) "-kernel!"))]
    (binding [*ns* (the-ns 'desargues.board)]
      (eval (kernel-form kname spec)))
    (ns-resolve 'desargues.board kname)))

;; ---------------------------------------------------------------------------
;; The board value

(defn- sample-frame
  "Run the kernel on the JVM at the params' :init values: the frame a page
   without JavaScript shows, and the one the live board starts from."
  [kvar {:keys [params] {[x0 x1] :x n :n} :window}]
  (let [xs (double-array n) ys (double-array n) dys (double-array n) iys (double-array n)
        h (/ (- (double x1) (double x0)) (dec n))]
    (apply @kvar xs ys dys iys (long n) (double x0) h (map (comp double :init) params))
    {:xs (vec xs) :ys (vec ys) :dys (vec dys) :iys (vec iys)}))

(def default-window {:x [-4 4] :y [-3 3] :n 401})

(defn compile-board!
  "Compile `spec` (see ns doc) and write <out-dir>/<id>.wasm. Returns the
   board value plato renders; its :board/kernel :wasm is `url-base` + file
   name, the URL the page fetches the module from.

   opts: :out-dir (default \"vendor/boards\"), :url-base (default
   \"./vendor/boards/\")."
  ([spec] (compile-board! spec {}))
  ([{:keys [id label params probes] :as spec}
    {:keys [out-dir url-base] :or {out-dir "vendor/boards" url-base "./vendor/boards/"}}]
   (let [spec (update spec :window #(merge default-window %))
         kvar (define-kernel! spec)
         export (str (name id))
         module (pl/compile-wasm kvar :name export :dtype :double)
         file (io/file out-dir (str export ".wasm"))]
     (io/make-parents file)
     (with-open [o (io/output-stream file)] (.write o ^bytes (byte-array (:bytes module))))
     {:board/id id
      :board/kernel {:wasm (str url-base export ".wasm") :export export}
      :board/label (or label (pr-str (:f spec)))
      :board/f (pr-str (:f spec))
      :board/df (pr-str (derivative spec))
      :board/window (:window spec)
      :board/params (mapv (fn [p] (update p :id keyword)) params)
      :board/probes (merge {:tangent 0.8 :area [-1.5 1.5]} probes)
      :board/frame (sample-frame kvar spec)})))

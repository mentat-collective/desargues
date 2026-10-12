(ns desargues.board
  "Boards: interactive calculus frames, compiled by raster, rendered by plato.

   The Boundary of a stratified bounded context:

     desargues.specs.board     Core      BoardSpec, Param, Window, Layer, Board
     desargues.board.kernel    Pure      BoardSpec -> kernel plan (open, by :kind)
     desargues.board.compiler  Protocol  KernelCompiler: plan -> callable, wasm
     desargues.board           Boundary  validate, compile, sample, write

   An author writes a BoardSpec:

     {:id :wave :kind :calculus
      :f '(+ (* a (sin (* b x))) (* c (* x x))) :var 'x
      :params [{:id 'a :min -2 :max 2 :step 0.01 :init 1} ...]
      :window {:x [-4 4] :y [-3 3] :n 401}}

   and `compile-board!` returns a Board: plain data naming the wasm kernel,
   its outputs, the layers plato draws and the frame the kernel sampled on
   this host at the params' :init values. plato.board renders it: a static
   plot from the frame, hydrated into a live board that calls the kernel on
   every drag. A new kind of board is a `defmethod` of
   desargues.board.kernel/plan; nothing here changes.

   Requires raster on the classpath (the :dynamics alias), like
   desargues.infrastructure.raster-adapter."
  (:require [clojure.java.io :as io]
            [clojure.spec.alpha :as s]
            [desargues.board.compiler :as compiler]
            [desargues.board.kernel :as kernel]
            [desargues.specs.board :as spec]
            [desargues.board.construction]
            [desargues.board.euclid]
            [desargues.board.projective]
            [desargues.board.symmetry]))

(defn- conform!
  "The spec with its kind's defaults (kernel/defaults), or an ex-info naming
   what is wrong."
  [board-spec]
  (let [spec* (merge {:kind :calculus} board-spec)
        dflt (kernel/defaults spec*)
        spec* (-> (merge (dissoc dflt :window) spec*)
                  (update :window #(merge (:window dflt) %)))]
    (when-not (s/valid? ::spec/board-spec spec*)
      (throw (ex-info "Invalid board spec" {:explain (s/explain-data ::spec/board-spec spec*)})))
    spec*))

;; ---------------------------------------------------------------------------
;; Pure: the frame and the Board value

(defn sample-frame
  "Run `kernel` on this host at the params' :init values: {output [numbers]}
   for every output of the plan. The kernel sweeps its variable over the
   window's :sweep domain (default its :x range) in n samples; n = 1 is one
   call at the domain's start."
  [kernel {:keys [params] {:keys [x n sweep]} :window} outputs]
  (let [[s0 s1] (or sweep x)
        arrays (mapv (fn [_] (double-array n)) outputs)
        h (if (> n 1) (/ (- (double s1) (double s0)) (dec n)) 0.0)]
    (apply kernel (concat arrays [(long n) (double s0) h] (map (comp double :init) params)))
    (zipmap outputs (map vec arrays))))

(defn board-value
  "The Board plato renders, from the conformed spec, its plan, where the
   page will find the kernel, and the sampled frame. Pure."
  [{:keys [id kind label window params]} plan kernel-ref frame]
  (cond-> {:board/id id
           :board/kind kind
           :board/kernel kernel-ref
           :board/label (or label (get-in plan [:labels :f]) (name id))
           :board/labels (:labels plan)
           :board/window window
           :board/params (mapv #(update % :id keyword) params)
           :board/outputs (:outputs plan)
           :board/layers (:layers plan)
           :board/probes (:probes plan)
           :board/frame frame}
    (seq (:math plan)) (assoc :board/math (vec (:math plan)))
    (:view plan) (assoc :board/view (:view plan))))

;; ---------------------------------------------------------------------------
;; Boundary

(defn write-wasm!
  "The module on disk at <out-dir>/<export>.wasm; returns the file."
  [out-dir export ^bytes module]
  (let [file (io/file out-dir (str export ".wasm"))]
    (io/make-parents file)
    (with-open [o (io/output-stream file)] (.write o module))
    file))

(defn compile-board!
  "Compile `board-spec` and write its kernel; return the Board.

   opts:
     :out-dir   where the .wasm is written      (default \"vendor/boards\")
     :url-base  where the page fetches it from  (default \"./vendor/boards/\")
     :compiler  a desargues.board.compiler/KernelCompiler
                                                (default: raster)"
  ([board-spec] (compile-board! board-spec {}))
  ([board-spec {:keys [out-dir url-base] :or {out-dir "vendor/boards" url-base "./vendor/boards/"}
                :as opts}]
   (let [spec* (conform! board-spec)
         plan (kernel/plan spec*)
         ;; A kind may own params beyond the author's (a construction's free
         ;; points own their coordinates); the plan's list is then the ABI's.
         spec* (assoc spec* :params (vec (or (:params plan) (:params spec*))))
         export (name (:id spec*))
         kc (or (:compiler opts) (compiler/raster-compiler))
         kvar (compiler/define-kernel kc (symbol (str export "-kernel!")) ((:form plan) (symbol (str export "-kernel!"))))
         board (board-value spec* plan
                            {:wasm (str url-base export ".wasm") :export export}
                            (sample-frame @kvar spec* (:outputs plan)))]
     (write-wasm! out-dir export (compiler/wasm-bytes kc kvar export))
     (when-not (s/valid? ::spec/board board)
       (throw (ex-info "Compiled board violates the Board contract"
                       {:explain (s/explain-data ::spec/board (dissoc board :board/frame))})))
     board)))

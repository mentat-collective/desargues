(ns desargues.geometry.simplex
  "Nelder-Mead over double arrays: a step-for-step port of
   emmy.numerical.multimin.nelder-mead (adaptive parameters, same initial
   simplex, same TimSort ordering of vertices, same convergence and stop
   tests, same function-call accounting). Given the same objective it visits
   the same points in the same order and returns bit-identical results; the
   difference is that vertices are double[] rows mutated in place instead of
   persistent vectors rebuilt per arithmetic op.

   The objective receives a double[] it must not retain or mutate.")

(set! *warn-on-reflection* true)
(set! *unchecked-math* false)

(defn- sort-simplex!
  "Reorder rows/fs ascending by f, with the permutation clojure.core/sort-by
   would produce (stable TimSort over the indices, comparator =
   clojure.core/compare on Doubles, i.e. Double/compare)."
  [^objects rows ^doubles fs]
  (let [m (alength fs)
        idx (object-array (range m))
        cmp (reify java.util.Comparator
              (compare [_ a b]
                (Double/compare (aget fs (int (long a))) (aget fs (int (long b))))))]
    (java.util.Arrays/sort idx cmp)
    (let [rows' (aclone rows) fs' (aclone fs)]
      (dotimes [i m]
        (let [j (int (long (aget idx i)))]
          (aset rows i (aget rows' j))
          (aset fs i (aget fs' j)))))))

(defn- sorted-check!
  "emmy's step :pre [(apply <= f-simplex)]."
  [^doubles fs]
  (when *assert*
    (dotimes [i (dec (alength fs))]
      (when-not (<= (aget fs i) (aget fs (inc i)))
        (throw (AssertionError. "Assert failed: (apply <= f-simplex)"))))))

(defn- within?
  "emmy's convergence test: sup-norm of the simplex about its best vertex
   <= stol, and of the f values about the best value <= ftol."
  [^objects rows ^doubles fs n stol ftol]
  (let [n (long n) stol (double stol) ftol (double ftol)
        ^doubles x0 (aget rows 0)
        f0 (aget fs 0)]
    (and (loop [i 0]
           (if (> i n)
             true
             (let [^doubles r (aget rows i)]
               (if (loop [j 0]
                     (if (= j n) true
                         (if (<= (Math/abs (- (aget r j) (aget x0 j))) stol)
                           (recur (inc j))
                           false)))
                 (recur (inc i))
                 false))))
         (loop [i 0]
           (if (> i n)
             true
             (if (<= (Math/abs (- (aget fs i) f0)) ftol)
               (recur (inc i))
               false))))))

(defn nelder-mead
  "Minimize f (double[] -> double) from x0 (a seq of doubles). Same options
   and defaults as emmy.numerical.multimin.nelder-mead/nelder-mead except
   :callback. Returns {:result (vector) :value :converged? :iterations
   :fncalls}."
  [f x0 {:keys [adaptive? nonzero-delta zero-delta maxiter maxfun
                simplex-tolerance fn-tolerance alpha beta gamma sigma]
         :or {adaptive? true nonzero-delta 0.05 zero-delta 0.00025
              simplex-tolerance 1e-4 fn-tolerance 1e-4}}]
  (let [x0 (vec x0)
        n (count x0)
        base (if adaptive?
               {:alpha 1.0
                :beta (+ 1.0 (/ 2.0 n))
                :gamma (- 0.75 (/ (* 2.0 n)))
                :sigma (- 1.0 (/ n))}
               {:alpha 1.0 :beta 2.0 :gamma 0.5 :sigma 0.5})
        alpha (or alpha (:alpha base)) beta (or beta (:beta base))
        gamma (or gamma (:gamma base)) sigma (or sigma (:sigma base))
        ;; the scalars emmy's v* multiplies by, computed as emmy computes them
        inv-n (double (/ n))
        k-refl (double (inc alpha)) a (double alpha)
        k-exp (double (inc (* alpha beta))) ab (double (* alpha beta))
        k-rc (double (inc (* gamma alpha))) ga (double (* gamma alpha))
        k-c (double (- 1 gamma)) g (double gamma)
        sg (double sigma)
        maxiter (long (or maxiter (* n 200)))
        maxfun (long (or maxfun (* n 200)))
        stol (double simplex-tolerance) ftol (double fn-tolerance)
        calls (long-array 1)
        f* (fn ^double [^doubles x]
             (aset calls 0 (inc (aget calls 0)))
             (double (f x)))
        m (inc n)
        rows (object-array m)
        fs (double-array m)
        scale (inc (double nonzero-delta))
        c (double-array n)
        ;; trial points, recycled: a rejected trial array is reused, an
        ;; accepted one is swapped with the evicted worst row.
        spare (object-array [(double-array n) (double-array n)])
        take! (fn ^doubles [^long k] (aget spare k))
        accept! (fn [^long k ^doubles x ^double fx]
                  (aset spare k (aget rows n))
                  (aset rows n x)
                  (aset fs n fx)
                  (sort-simplex! rows fs))]
    (aset rows 0 (double-array x0))
    (dotimes [i n]
      (let [xi (double (nth x0 i))
            r (double-array x0)]
        (aset r i (if (zero? xi) (double zero-delta) (* scale xi)))
        (aset rows (inc i) r)))
    (dotimes [i m] (aset fs i (double (f* (aget rows i)))))
    (sort-simplex! rows fs)
    (loop [iteration 0]
      (let [converged? (within? rows fs n stol ftol)]
        (if (or converged?
                (> iteration maxiter)
                (> (aget calls 0) maxfun))
          {:result (vec ^doubles (aget rows 0))
           :value (aget fs 0)
           :converged? converged?
           :iterations iteration
           :fncalls (aget calls 0)}
          (let [_ (sorted-check! fs)
                ^doubles w (aget rows n)
                f-best (aget fs 0)
                f-worst (aget fs n)
                f-butworst (aget fs (dec n))
                lin! (fn ^doubles [^doubles out ^double kc ^double kw sub?]
                       (dotimes [j n]
                         (aset out j (if sub?
                                       (- (* kc (aget c j)) (* kw (aget w j)))
                                       (+ (* kc (aget c j)) (* kw (aget w j))))))
                       out)
                shrink! (fn []
                          (let [^doubles s0 (aget rows 0)]
                            (loop [i 1]
                              (when (<= i n)
                                (let [^doubles r (aget rows i)]
                                  (dotimes [j n]
                                    (aset r j (+ (aget s0 j) (* sg (- (aget r j) (aget s0 j))))))
                                  (recur (inc i)))))
                            (dotimes [i m] (aset fs i (double (f* (aget rows i)))))
                            (sort-simplex! rows fs)))]
            ;; centroid of all but the worst: ((s0 + s1) + ...) * (1/n)
            (let [^doubles s0 (aget rows 0)]
              (dotimes [j n] (aset c j (aget s0 j))))
            (loop [i 1]
              (when (< i n)
                (let [^doubles r (aget rows i)]
                  (dotimes [j n] (aset c j (+ (aget c j) (aget r j))))
                  (recur (inc i)))))
            (dotimes [j n] (aset c j (* inv-n (aget c j))))
            (let [refl (lin! (take! 0) k-refl a true)
                  fr (f* refl)]
              (cond
                (< fr f-best)
                (let [expd (lin! (take! 1) k-exp ab true)
                      fe (f* expd)]
                  (if (< fe fr)
                    (accept! 1 expd fe)
                    (accept! 0 refl fr)))

                (< fr f-butworst)
                (accept! 0 refl fr)

                (< fr f-worst)
                (let [rc (lin! (take! 1) k-rc ga true)
                      frc (f* rc)]
                  (if (<= frc fr)
                    (accept! 1 rc frc)
                    (shrink!)))

                :else
                (let [ct (lin! (take! 1) k-c g false)
                      fc (f* ct)]
                  (if (< fc f-worst)
                    (accept! 1 ct fc)
                    (shrink!)))))
            (recur (inc iteration))))))))

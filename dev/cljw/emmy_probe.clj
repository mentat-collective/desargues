;; Walk Emmy bottom-up under cljw and stop at the first namespace that fails,
;; printing the decisive error. Run via dev/cljw/run.sh emmy.
(def chain
  '[emmy.util emmy.util.def emmy.value emmy.ratio emmy.generic emmy.complex
    emmy.numbers emmy.expression emmy.numsymb emmy.abstract.number
    emmy.structure emmy.operator emmy.calculus.derivative emmy.simplify
    emmy.tex emmy.env])

(defn first-line [s]
  (let [s (str s) i (.indexOf s "\n")] (if (neg? i) s (subs s 0 i))))

(loop [[n & more] chain]
  (when n
    (let [r (try (require n) nil
                 (catch Throwable e (first-line (or (ex-message e) (str e)))))]
      (if r
        (println (format "%-28s FAIL %s" (str n) r))
        (do (println (format "%-28s ok" (str n))) (recur more))))))

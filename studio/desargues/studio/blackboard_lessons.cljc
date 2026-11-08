(ns desargues.studio.blackboard-lessons
  "Worked examples for the blackboard (desargues.studio.blackboard), and the
   adapter from the step solver's display shape.

   A LESSON is plain data:

     {:id :by-parts :title \"integration by parts\"
      :problem \"\\\\int x e^x\\\\,dx\"          ; TeX, written at the top
      :answer  \"(x-1)e^x + C\"                  ; TeX
      :check   \"\\\\frac{d}{dx}(x-1)e^x = x e^x\" ; TeX, the verification line
      :figure  {:f fn :F fn :x-range [a b] :y-range [c d]}   ; optional
               ;; or {:f fn :area [a b] ...} for a definite integral
      :pathways [{:label \"by parts, u = x\" :steps [step ...]} ...]}

   A STEP is the step solver's display entry, {:tex :explain}, plus optional
   board extras: :side (TeX for the margin, e.g. u = sin x => du = cos x dx),
   :answer? (box it in coloured chalk), :shade? (shade the figure's area now).

   TeX may carry chalk colours as \\yel{..} (u, du), \\blu{..} (dv, v),
   \\pnk{..} and \\grn{..}; the board expands them to \\textcolor. An
   :explain is plain English with inline math between $...$.

   The step solver (desargues.steps, branch step-solver) is not merged: its
   (solve p) -> {:answer :derivation :alternatives :verified?} and
   (steps->display d) -> [{:tex :explain}] meet the board through
   `from-display` and `from-solution`, so a solved problem becomes a lesson
   without touching the board. Every hand-written answer below is checked by
   Emmy in desargues.studio.blackboard-lessons-test (differentiate back).
   Host-free .cljc.")

;; ── adapter: step-solver display -> lesson ────────────────────────────────────

(defn from-display
  "A pathway from a step-solver display `steps` ([{:tex :explain} ...]): the
   last step is the boxed answer unless one is already marked."
  ([steps] (from-display steps "method"))
  ([steps label]
   (let [steps (vec steps)
         marked? (some :answer? steps)]
     {:label label
      :steps (if (or marked? (empty? steps))
               steps
               (update steps (dec (count steps)) assoc :answer? true))})))

(defn from-solution
  "A lesson from a solved problem {:answer :derivation :alternatives
   :verified?}: `display` turns one derivation into [{:tex :explain} ...];
   the derivation and each alternative become pathways. `meta` gives the
   lesson its :id :title :problem (and optional :figure)."
  [display {:keys [answer derivation alternatives verified?]} meta]
  (let [ds (cons derivation alternatives)]
    (merge {:answer (str answer)
            :check  (str "\\text{" (if verified? "verified" "not verified") " by Emmy}")
            :pathways (vec (map-indexed (fn [i d] (from-display (display d) (str "method " (inc i))))
                                        ds))}
           meta)))

(defn parts->explain
  "Phrase parts [[:text s] [:math e] ...] (desargues.steps.phrase/parts) as a
   board explanation: text as is, math between $...$ through `tex`."
  [tex parts]
  (apply str (for [[k v] parts] (if (= :math k) (str "$" (tex v) "$") (str v)))))

(defn from-derivation
  "A pathway from a step-model Derivation (desargues.steps.model, branch
   solution-notebooks): each Step writes its :to (or, for a move that only
   introduces bindings, the bindings) as the equation line, its words from
   `parts` (desargues.steps.phrase/parts) under it, and bindings named u/du
   or dv/v in the margin. `tex` renders a math form (desargues.steps.render/tex).
   Records are maps, so plain maps of the same shape work too."
  ([d parts tex] (from-derivation d parts tex "method"))
  ([{:keys [steps answer]} parts tex label]
   (let [bind-tex (fn [b] (->> (dissoc b :reason :formula :identity :unknown)
                               (map (fn [[k v]] (str (tex k) " = " (tex v))))
                               (interpose ",\\ ")
                               (apply str)))
         line (fn [{:keys [to bindings]}]
                (cond to                           (tex to)
                      (seq (bind-tex bindings))    (bind-tex bindings)
                      :else                        "\\ldots"))
         sts  (vec (for [s steps
                         :let [side (when (and (:to s) (seq (bind-tex (:bindings s))))
                                      (bind-tex (:bindings s)))]]
                     (cond-> {:tex (line s) :explain (parts->explain tex (parts s))}
                       side (assoc :side side))))
         last-tex (some-> answer tex)]
     (from-display (cond-> sts
                     (and last-tex (not= last-tex (:tex (peek sts))))
                     (conj {:tex (str "= " last-tex) :explain "The answer."}))
                   label))))

;; ── the worked examples ───────────────────────────────────────────────────────

(def u-substitution
  {:id :u-sub :title "u-substitution"
   :problem "\\int \\sin^2 x\\,\\cos x\\,dx"
   :answer "\\frac{\\sin^3 x}{3} + C"
   :check "\\frac{d}{dx}\\,\\frac{\\sin^3 x}{3} = \\sin^2 x\\,\\cos x"
   :figure {:f #(* (Math/sin %) (Math/sin %) (Math/cos %))
            :F #(/ (Math/pow (Math/sin %) 3) 3.0)
            :x-range [-3.2 3.2] :y-range [-0.6 0.6]}
   :pathways
   [{:label "substitution u = sin x"
     :steps [{:tex "\\int \\yel{\\sin^2 x}\\,\\blu{\\cos x\\,dx}"
              :explain "Spot the pair: $\\sin x$ and its derivative $\\cos x\\,dx$."
              :side "\\yel{u = \\sin x} \\Rightarrow \\blu{du = \\cos x\\,dx}"}
             {:tex "= \\int \\yel{u}^2\\,\\blu{du}"
              :explain "Substitute: the integrand becomes a power of $u$."}
             {:tex "= \\frac{u^3}{3} + C"
              :explain "Power rule: $\\int u^n\\,du = \\frac{u^{n+1}}{n+1} + C$."}
             {:tex "= \\frac{\\sin^3 x}{3} + C" :answer? true
              :explain "Put $u = \\sin x$ back."}]}
    {:label "reverse chain rule"
     :steps [{:tex "\\frac{d}{dx}\\,\\sin^3 x = 3\\,\\sin^2 x\\,\\cos x"
              :explain "Guess $\\sin^3 x$: its derivative is our integrand, times 3."
              :side "(f^3)' = 3f^2\\,f'"}
             {:tex "\\int \\sin^2 x\\,\\cos x\\,dx = \\frac13 \\int 3\\,\\sin^2 x\\,\\cos x\\,dx"
              :explain "Fix the constant: multiply and divide by 3."}
             {:tex "= \\frac{\\sin^3 x}{3} + C" :answer? true
              :explain "The integral of a derivative is the function back."}]}]})

(def by-parts
  {:id :by-parts :title "integration by parts"
   :problem "\\int x\\,e^x\\,dx"
   :answer "(x-1)\\,e^x + C"
   :check "\\frac{d}{dx}\\,(x-1)\\,e^x = x\\,e^x"
   :figure {:f #(* % (Math/exp %)) :F #(* (- % 1.0) (Math/exp %))
            :x-range [-3.0 1.5] :y-range [-1.5 4.0]}
   :pathways
   [{:label "by parts, u = x"
     :steps [{:tex "\\int \\yel{x}\\,\\blu{e^x\\,dx}"
              :explain "A product: take $u = x$, it gets simpler when differentiated."
              :side "\\yel{u = x} \\Rightarrow \\yel{du = dx}"}
             {:tex "\\int u\\,dv = uv - \\int v\\,du"
              :explain "Integration by parts, the product rule read backwards."
              :side "\\blu{dv = e^x\\,dx} \\Rightarrow \\blu{v = e^x}"}
             {:tex "= \\yel{x}\\,\\blu{e^x} - \\int \\blu{e^x}\\,\\yel{dx}"
              :explain "Plug in $u$, $v$ and $du$."}
             {:tex "= x\\,e^x - e^x + C"
              :explain "The integral left over is just $e^x$."}
             {:tex "= (x-1)\\,e^x + C" :answer? true
              :explain "Factor out $e^x$."}]}
    {:label "tabular method"
     :steps [{:tex "\\yel{x} \\to \\yel{1} \\to \\yel{0}, \\qquad \\blu{e^x} \\to \\blu{e^x} \\to \\blu{e^x}"
              :explain "Differentiate $x$ down to 0; integrate $e^x$ alongside."
              :side "\\text{signs: } +,\\ -,\\ +"}
             {:tex "= +\\,\\yel{x}\\,\\blu{e^x} - \\yel{1}\\cdot\\blu{e^x}"
              :explain "Multiply along the diagonals with alternating signs."}
             {:tex "= (x-1)\\,e^x + C" :answer? true
              :explain "Collect the terms."}]}]})

(def cyclic-parts
  {:id :cyclic :title "by parts, twice (cyclic)"
   :problem "\\int e^x \\sin x\\,dx"
   :answer "\\frac{e^x(\\sin x - \\cos x)}{2} + C"
   :check "\\frac{d}{dx}\\,\\frac{e^x(\\sin x - \\cos x)}{2} = e^x \\sin x"
   :figure {:f #(* (Math/exp %) (Math/sin %))
            :F #(/ (* (Math/exp %) (- (Math/sin %) (Math/cos %))) 2.0)
            :x-range [-3.0 2.2] :y-range [-1.5 7.0]}
   :pathways
   [{:label "parts twice, solve for I"
     :steps [{:tex "I = \\int \\blu{e^x}\\,\\yel{\\sin x}\\,dx"
              :explain "Name the integral $I$: we will meet it again."
              :side "\\yel{u = \\sin x},\\ \\blu{dv = e^x\\,dx}"}
             {:tex "I = e^x \\sin x - \\int e^x \\cos x\\,dx"
              :explain "Parts once: $du = \\cos x\\,dx$, $v = e^x$."}
             {:tex "I = e^x \\sin x - \\left(e^x \\cos x + \\int e^x \\sin x\\,dx\\right)"
              :explain "Parts again with $u = \\cos x$, so $du = -\\sin x\\,dx$."
              :side "\\yel{u = \\cos x},\\ \\blu{v = e^x}"}
             {:tex "I = e^x(\\sin x - \\cos x) - \\yel{I}"
              :explain "The original integral came back!"}
             {:tex "2I = e^x(\\sin x - \\cos x)"
              :explain "Move $I$ to the left-hand side."}
             {:tex "I = \\frac{e^x(\\sin x - \\cos x)}{2} + C" :answer? true
              :explain "Divide by 2 and add the constant."}]}
    {:label "complex exponential"
     :steps [{:tex "e^x \\sin x = \\mathrm{Im}\\; e^{(1+i)x}"
              :explain "Euler's formula turns the product into one exponential."
              :side "e^{ix} = \\cos x + i \\sin x"}
             {:tex "\\int e^{(1+i)x}\\,dx = \\frac{e^{(1+i)x}}{1+i}"
              :explain "Integrate the exponential directly."}
             {:tex "= \\frac{1-i}{2}\\,e^x(\\cos x + i \\sin x)"
              :explain "Rationalise: $\\frac{1}{1+i} = \\frac{1-i}{2}$."
              :side "(1-i)(\\cos x + i\\sin x)"}
             {:tex "\\mathrm{Im}: \\ \\frac{e^x}{2}(\\sin x - \\cos x)"
              :explain "Keep the imaginary part."}
             {:tex "I = \\frac{e^x(\\sin x - \\cos x)}{2} + C" :answer? true
              :explain "That is the integral."}]}]})

(def partial-fractions
  {:id :partial :title "partial fractions"
   :problem "\\int \\frac{dx}{x^2 - 1}"
   :answer "\\frac12 \\ln\\left|\\frac{x-1}{x+1}\\right| + C"
   :check "\\frac{d}{dx}\\,\\frac12 \\ln\\left|\\frac{x-1}{x+1}\\right| = \\frac{1}{x^2-1}"
   :figure {:f #(/ 1.0 (- (* % %) 1.0))
            :F #(* 0.5 (Math/log (Math/abs (/ (- % 1.0) (+ % 1.0)))))
            :x-range [1.2 4.0] :y-range [-1.5 2.5]}
   :pathways
   [{:label "equate coefficients"
     :steps [{:tex "\\frac{1}{x^2-1} = \\frac{1}{(x-1)(x+1)}"
              :explain "Factor the denominator: a difference of squares."}
             {:tex "= \\frac{\\yel{A}}{x-1} + \\frac{\\blu{B}}{x+1}"
              :explain "Split it into partial fractions."}
             {:tex "1 = \\yel{A}\\,(x+1) + \\blu{B}\\,(x-1)"
              :explain "Clear the denominators: this holds for every $x$."
              :side "A + B = 0,\\quad A - B = 1"}
             {:tex "\\yel{A = \\tfrac12}, \\qquad \\blu{B = -\\tfrac12}"
              :explain "Solve the two equations."}
             {:tex "= \\frac12 \\ln|x-1| - \\frac12 \\ln|x+1| + C"
              :explain "Each piece integrates to a logarithm."}
             {:tex "= \\frac12 \\ln\\left|\\frac{x-1}{x+1}\\right| + C" :answer? true
              :explain "Combine the logarithms."}]}
    {:label "cover-up rule"
     :steps [{:tex "\\frac{1}{(x-1)(x+1)} = \\frac{\\yel{A}}{x-1} + \\frac{\\blu{B}}{x+1}"
              :explain "Factor, then split into partial fractions."}
             {:tex "\\yel{A} = \\frac{1}{x+1}\\Big|_{x=1} = \\frac12"
              :explain "Cover up $x - 1$ and set $x = 1$."
              :side "\\text{cover } (x-1),\\ x = 1"}
             {:tex "\\blu{B} = \\frac{1}{x-1}\\Big|_{x=-1} = -\\frac12"
              :explain "Cover up $x + 1$ and set $x = -1$."
              :side "\\text{cover } (x+1),\\ x = -1"}
             {:tex "= \\frac12 \\ln\\left|\\frac{x-1}{x+1}\\right| + C" :answer? true
              :explain "Integrate both logarithms and combine them."}]}]})

(def definite-area
  {:id :area :title "a definite integral (area)"
   :problem "\\int_0^{\\pi} \\sin x\\,dx"
   :answer "2"
   :check "\\left[-\\cos x\\right]_0^{\\pi} = 2"
   :figure {:f #(Math/sin %) :area [0.0 Math/PI]
            :x-range [-0.3 3.6] :y-range [-0.3 1.3]}
   :pathways
   [{:label "fundamental theorem"
     :steps [{:tex "\\int_0^{\\pi} \\yel{\\sin x}\\,dx" :shade? true
              :explain "The area under one arch of $\\sin x$."
              :side "\\yel{F' = \\sin x} \\Rightarrow \\blu{F = -\\cos x}"}
             {:tex "= \\Big[\\blu{-\\cos x}\\Big]_0^{\\pi}"
              :explain "An antiderivative: $(-\\cos x)' = \\sin x$."}
             {:tex "= -\\cos \\pi - (-\\cos 0)"
              :explain "Fundamental theorem: $F(\\pi) - F(0)$."}
             {:tex "= -(-1) + 1"
              :explain "$\\cos \\pi = -1$ and $\\cos 0 = 1$."}
             {:tex "= 2" :answer? true
              :explain "The shaded arch has area exactly 2."}]}
    {:label "symmetry of the arch"
     :steps [{:tex "\\int_0^{\\pi} \\sin x\\,dx = 2 \\int_0^{\\pi/2} \\sin x\\,dx" :shade? true
              :explain "The arch is symmetric about $x = \\pi/2$."
              :side "\\sin(\\pi - x) = \\sin x"}
             {:tex "= 2\\,\\Big[-\\cos x\\Big]_0^{\\pi/2}"
              :explain "Antiderivative of $\\sin x$ is $-\\cos x$."}
             {:tex "= 2\\,(0 + 1)"
              :explain "$\\cos \\frac{\\pi}{2} = 0$ and $\\cos 0 = 1$."}
             {:tex "= 2" :answer? true
              :explain "Twice the half arch."}]}]})

(def lessons
  "Every worked example, in menu order."
  [u-substitution by-parts cyclic-parts partial-fractions definite-area])

(def by-id (into {} (map (juxt :id identity)) lessons))

(defn lesson
  "The lesson with id `k` (the first one when `k` is unknown)."
  [k]
  (get by-id k (first lessons)))

(defn pathway
  "Pathway `i` of `lesson`, clamped to the pathways it has."
  [lesson i]
  (let [ps (:pathways lesson)]
    (nth ps (-> (long (or i 0)) (max 0) (min (dec (count ps)))))))

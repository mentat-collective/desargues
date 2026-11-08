(ns desargues.steps.phrase
  "How a step is SAID: the common constructions of a worked solution as data.

   `phrase` turns a Step into phrase parts, a vector of
     [:text \"Let \"] [:math expr] [:text \" = \"] [:math expr] ...
   one method per construction (OCP): :let, :then, :hence, :apply-formula,
   :substitute-back, :evaluate, :split, :rewrite, :choose, :solve-for,
   :verify. Parts are data, so every renderer lays them out its own way:
   TeX in a notebook, chalk lines on the blackboard, plain text in a REPL.
   A step's :text, when present, replaces the construction's wording.

   Constructions compose: `then-phrase` joins the parts of several, and a
   construction may embed another's parts (:choose phrases its :let).

   Host-free .cljc.")

(defn text [& s] [:text (apply str s)])
(defn math [e] [:math e])

(defn- bindings-parts
  "u = sin x, du = cos x dx  as parts, in the order the bindings were given."
  [bindings]
  (->> bindings
       (map (fn [[k v]] [(math k) (text " = ") (math v)]))
       (interpose [(text ", ")])
       (apply concat)
       vec))

(defmulti phrase
  "Phrase parts of step `s`, chosen by its :construction."
  :construction)

(defmethod phrase :default [s]
  [(text "Then ") (math (:to s))])

(defmethod phrase :then [s]
  [(text "Then ") (math (:to s))])

(defmethod phrase :hence [s]
  [(text "Hence ") (math (:to s))])

(defmethod phrase :let [s]
  (into [(text "Let ")] (concat (bindings-parts (:bindings s))
                                [(text ". The integral becomes ") (math (:to s))])))

(defmethod phrase :choose [s]
  (let [{:keys [reason]} (:bindings s)]
    (into [(text "Choose ")]
          (concat (bindings-parts (dissoc (:bindings s) :reason))
                  (when reason [(text " (" reason ")")])))))

(defmethod phrase :apply-formula [s]
  (let [{:keys [formula]} (:bindings s)]
    [(text "By the formula ") (math formula) (text ": ") (math (:to s))]))

(defmethod phrase :split [s]
  [(text "Split the integral by linearity: ") (math (:to s))])

(defmethod phrase :rewrite [s]
  (let [{:keys [identity]} (:bindings s)]
    (cond-> [(text "Rewrite ")]
      identity (into [(text "using ") (math identity) (text ": ")])
      true (conj (math (:to s))))))

(defmethod phrase :table [s]
  [(text "From the table of integrals: ") (math (:to s))])

(defmethod phrase :substitute-back [s]
  (into [(text "Substitute back ")] (concat (bindings-parts (:bindings s))
                                            [(text ": ") (math (:to s))])))

(defmethod phrase :solve-for [s]
  (let [{:keys [unknown]} (:bindings s)]
    [(text "The integral reappeared. Solve for ") (math unknown) (text ": ") (math (:to s))]))

(defmethod phrase :evaluate [s]
  [(text "Evaluate at the bounds: ") (math (:to s))])

(defmethod phrase :verify [s]
  [(text "Check: differentiating gives back the integrand, ") (math (:to s))])

(defmethod phrase :cite [s]
  (let [{:keys [lemma]} (:bindings s)]
    [(text "By problem " (name lemma) ", already solved: ") (math (:from s)) (text " = ") (math (:to s))]))

(defmethod phrase :cover-up [s]
  (let [{:keys [factor root rest]} (:bindings s)]
    [(text "Cover the factor ") (math factor) (text " and set ") (math 'x) (text " = ") (math root)
     (text " in what is left, ") (math rest) (text ": ") (math (:to s))]))

(defn parts
  "The parts a renderer shows for `s`: its own :text when it has one."
  [s]
  (if-let [t (:text s)] [(text t)] (phrase s)))

(defn then-phrase
  "The parts of several steps joined by a sentence break."
  [steps]
  (vec (mapcat (fn [s] (conj (parts s) (text " "))) steps)))

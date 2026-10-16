(ns desargues.logic.notation
  "How each operator of the logic and set-theory vocabulary is written in TeX.

   `notation` is OPEN on the operator symbol. A notation is

     {:infix?      true when the operator is written between or around its
                   arguments (its children are then parenthesized by
                   precedence)
      :precedence  higher binds tighter
      :render      (fn [rendered-args]) -> TeX string}

   An operator with no notation is written as a function application, f(x, y).
   Emmy's TeX renderer does the parenthesization; desargues.logic.tex builds it
   from these methods."
  (:require [clojure.string :as str]))

(defmulti notation
  "Operator symbol -> its notation map, or nil."
  identity)

(defmethod notation :default [_] nil)

(defn- infix [tex prec]
  {:infix? true :precedence prec :render #(str/join (str " " tex " ") %)})

(defn- prefix [f] {:infix? false :render f})

;; ---------------------------------------------------------------------------
;; Connectives

(defmethod notation 'not [_] {:infix? true :precedence 6 :render (fn [[x]] (str "\\lnot " x))})
(defmethod notation 'and [_] (infix "\\land" 2))
(defmethod notation 'or [_] (infix "\\lor" 1))
(defmethod notation 'implies [_] (infix "\\Rightarrow" 0))
(defmethod notation 'iff [_] (infix "\\Leftrightarrow" -1))

;; ---------------------------------------------------------------------------
;; Quantifiers and class abstraction: (forall x y body), binders flattened

(defn- binder [q]
  (prefix (fn [args]
            (str q " " (str/join ", " (butlast args))
                 " \\left[ " (last args) " \\right]"))))

(defmethod notation 'forall [_] (assoc (binder "\\forall") :precedence -2))
(defmethod notation 'exists [_] (assoc (binder "\\exists") :precedence -2))
(defmethod notation 'exists! [_] (assoc (binder "\\exists!") :precedence -2))
(defmethod notation 'class [_]
  (prefix (fn [args] (str "\\{" (str/join ", " (butlast args)) " : " (last args) "\\}"))))

;; ---------------------------------------------------------------------------
;; Relations between classes and elements

(defmethod notation 'in [_] (infix "\\in" 3))
(defmethod notation 'notin [_] (infix "\\notin" 3))
(defmethod notation 'subset [_] (infix "\\subseteq" 3))
(defmethod notation 'proper-subset [_] (infix "\\subset" 3))
(defmethod notation 'leq [_] (infix "\\leq" 3))
(defmethod notation 'lt [_] (infix "<" 3))

;; ---------------------------------------------------------------------------
;; Class operations (Pinter: A' complement, A + B symmetric difference)

(defmethod notation 'union [_] (infix "\\cup" 4))
(defmethod notation 'inter [_] (infix "\\cap" 5))
(defmethod notation 'diff [_] (infix "-" 4))
(defmethod notation 'sym-diff [_] (infix "+" 4))
(defmethod notation 'compl [_] {:infix? true :precedence 8 :render (fn [[x]] (str x "'"))})
(defmethod notation 'inverse [_] {:infix? true :precedence 8 :render (fn [[x]] (str x "^{-1}"))})
(defmethod notation 'product [_] (infix "\\times" 5))
(defmethod notation 'compose [_] (infix "\\circ" 6))
(defmethod notation 'join [_] (infix "\\vee" 4))
(defmethod notation 'meet [_] (infix "\\wedge" 5))

(defmethod notation 'empty [_] (prefix (fn [_] "\\varnothing")))
(defmethod notation 'universe [_] (prefix (fn [_] "\\mathcal{U}")))
(defmethod notation 'pair [_] (prefix (fn [args] (str "\\left(" (str/join ", " args) "\\right)"))))
(defmethod notation 'power [_] (prefix (fn [[a]] (str "\\mathcal{P}\\left(" a "\\right)"))))
(defmethod notation 'dom [_] (prefix (fn [[g]] (str "\\operatorname{dom} " g))))
(defmethod notation 'ran [_] (prefix (fn [[g]] (str "\\operatorname{ran} " g))))
(defmethod notation 'apply [_] (prefix (fn [[f x]] (str f "\\left(" x "\\right)"))))
(defmethod notation 'image [_] (prefix (fn [[f a]] (str "\\bar{" f "}\\left(" a "\\right)"))))
(defmethod notation 'preimage [_] (prefix (fn [[f a]] (str "\\check{" f "}\\left(" a "\\right)"))))
(defmethod notation 'identity [_] (prefix (fn [[a]] (str "I_{" a "}"))))
(defmethod notation 'restrict [_] (prefix (fn [[f a]] (str f "|_{" a "}"))))
(defmethod notation 'big-union [_] (prefix (fn [[f]] (str "\\bigcup " f))))
(defmethod notation 'big-inter [_] (prefix (fn [[f]] (str "\\bigcap " f))))
(defmethod notation 'indexed-union [_]
  (prefix (fn [[i s a]] (str "\\bigcup_{" i " \\in " s "} " a))))
(defmethod notation 'indexed-inter [_]
  (prefix (fn [[i s a]] (str "\\bigcap_{" i " \\in " s "} " a))))
(defmethod notation 'quotient [_] (prefix (fn [[a r]] (str a "/" r))))
(defmethod notation 'class-of [_] (prefix (fn [[x r]] (str "[" x "]_{" r "}"))))

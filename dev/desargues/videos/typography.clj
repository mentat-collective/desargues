(ns desargues.videos.typography
  "Typography DSL for LaTeX rendering and equation morphing.

   Key patterns from 3b1b videos:
   1. t2c (text-to-color) - Map LaTeX substrings to colors
   2. TransformMatchingTex - Morph between equations matching parts
   3. TransformMatchingStrings - String-level matching
   4. isolate - Split LaTeX into addressable parts
   5. Tex indexing - Access submobjects by string
   6. make_number_changeable - Dynamic number display
   7. set_backstroke - Text outlines for visibility"
  (:require [clojure.string :as str]))

;; =============================================================================
;; Color Constants (Manim standard colors)
;; =============================================================================

(def colors
  {:white "#FFFFFF"
   :grey-a "#DDDDDD"
   :grey-b "#BBBBBB"
   :grey-c "#888888"
   :grey-d "#444444"
   :grey-e "#222222"
   :black "#000000"
   :blue "#58C4DD"
   :blue-a "#C7E9F1"
   :blue-b "#9CDCEB"
   :blue-c "#29ABCA"
   :blue-d "#1C758A"
   :blue-e "#236B8E"
   :teal "#5CD0B3"
   :teal-a "#ACEAD7"
   :teal-b "#76DDC0"
   :teal-c "#5CD0B3"
   :teal-d "#55C1A7"
   :teal-e "#49A88F"
   :green "#83C167"
   :green-a "#C9E2AE"
   :green-b "#A6CF8C"
   :green-c "#83C167"
   :green-d "#77B05D"
   :green-e "#699C52"
   :yellow "#FFFF00"
   :yellow-a "#FFF1B6"
   :yellow-b "#FFEA94"
   :yellow-c "#FFDC00"
   :yellow-d "#F4D345"
   :yellow-e "#E8C11C"
   :gold "#F0AC5F"
   :gold-a "#F7C797"
   :gold-b "#F9B775"
   :gold-c "#F0AC5F"
   :gold-d "#E1A158"
   :gold-e "#C78D46"
   :red "#FC6255"
   :red-a "#F7A1A3"
   :red-b "#FF8080"
   :red-c "#FC6255"
   :red-d "#E65A4C"
   :red-e "#CF5044"
   :maroon "#C55F73"
   :maroon-a "#ECABC1"
   :maroon-b "#EC92AB"
   :maroon-c "#C55F73"
   :maroon-d "#A24D61"
   :maroon-e "#94424F"
   :purple "#9A72AC"
   :purple-a "#CAA3E8"
   :purple-b "#B189C6"
   :purple-c "#9A72AC"
   :purple-d "#715582"
   :purple-e "#644172"
   :pink "#D147BD"
   :pink-a "#FF00FF"
   :pink-b "#FF69B4"
   :pink-c "#D147BD"
   :pink-d "#C232A5"
   :pink-e "#A6208F"
   :orange "#FF862F"
   :light-brown "#CD853F"
   :dark-brown "#8B4513"})

(defn get-color
  "Get color value by keyword or return the value if it's already a hex string."
  [color]
  (if (keyword? color)
    (get colors color color)
    color))

;; =============================================================================
;; Text-to-Color (t2c) Mapping
;; =============================================================================

(defrecord T2CMapping [mappings])

(defn t2c
  "Create a text-to-color mapping for LaTeX.

   Example:
   (t2c {:x :blue :y :red \"\\\\theta\" :yellow})

   This is the primary way to colorize mathematical expressions
   in 3b1b videos."
  [mapping]
  (->T2CMapping
   (into {}
         (map (fn [[k v]]
                [(if (keyword? k) (str k) (str k))
                 (get-color v)])
              mapping))))

(defn merge-t2c
  "Merge multiple t2c mappings."
  [& mappings]
  (->T2CMapping
   (reduce merge {} (map :mappings mappings))))

;; Common t2c patterns from 3b1b videos
(def t2c-time-freq
  "Standard time/frequency domain colors."
  (t2c {"{t}" :blue "{s}" :yellow}))

(def t2c-xy
  "Standard x/y coordinate colors."
  (t2c {"{x}" :blue "{y}" :green}))

(def t2c-theta-omega
  "Standard angle/angular velocity colors."
  (t2c {"\\theta" :blue "\\omega" :yellow "\\dot{\\theta}" :red}))

(def t2c-derivatives
  "Standard derivative colors (position, velocity, acceleration)."
  (t2c {"x(t)" :teal "x'(t)" :gold "x''(t)" :red}))

;; =============================================================================
;; LaTeX Expression Building
;; =============================================================================

(defrecord TexExpr [content t2c isolate font-size backstroke])

(defn tex
  "Create a LaTeX expression with optional styling.

   Options:
   - :t2c - text-to-color mapping
   - :isolate - list of strings to isolate for targeting
   - :font-size - font size (default 48)
   - :backstroke - [color width] for outline"
  [content & {:keys [t2c isolate font-size backstroke]
              :or {font-size 48}}]
  (->TexExpr content t2c isolate font-size backstroke))

(defn tex-text
  "Create text with LaTeX rendering (for words/sentences)."
  [content & opts]
  (apply tex content opts))

;; =============================================================================
;; Tex Expression Manipulation
;; =============================================================================

(defn tex-part
  "Get a part of a Tex expression by string or index.

   In Manim: tex[\"x_1\"] or tex[0]

   Example:
   (tex-part expr \"x_1\")
   (tex-part expr 0)"
  [tex-expr selector]
  {:expr tex-expr
   :selector selector
   :type :tex-part})

(defn get-part
  "Alias for tex-part - get a part of a tex expression by selector."
  [tex-expr selector]
  (tex-part tex-expr selector))

(defn tex-parts
  "Get multiple parts matching a pattern.

   In Manim: tex[\"x_1\"][0], tex.get_parts_by_tex(\"x\")

   Example:
   (tex-parts expr \"x\") ; all x's"
  [tex-expr pattern]
  {:expr tex-expr
   :pattern pattern
   :type :tex-parts})

(defn tex-slice
  "Get a slice of tex expression.

   In Manim: tex[0:3] or tex[::2]"
  [tex-expr start end & {:keys [step] :or {step 1}}]
  {:expr tex-expr
   :start start
   :end end
   :step step
   :type :tex-slice})

;; =============================================================================
;; Number Rendering (Dynamic)
;; =============================================================================

(defrecord DecimalNumber [value num-decimal-places color show-ellipsis
                          include-sign unit edge-to-fix])

(defn decimal-number
  "Create a decimal number display.

   Options:
   - :num-decimal-places - decimal places (default 2)
   - :color - text color
   - :show-ellipsis - show ... for long numbers
   - :include-sign - always show +/- sign
   - :unit - unit suffix (e.g., \"^\\circ\")
   - :edge-to-fix - which edge stays fixed on update"
  [value & {:keys [num-decimal-places color show-ellipsis
                   include-sign unit edge-to-fix]
            :or {num-decimal-places 2}}]
  (->DecimalNumber value num-decimal-places color show-ellipsis
                   include-sign unit edge-to-fix))

(defrecord IntegerNumber [value color unit])

(defn integer
  "Create an integer display."
  [value & {:keys [color unit]}]
  (->IntegerNumber value color unit))

(defn make-number-changeable
  "Mark a number in a Tex expression as changeable.

   In Manim: label.make_number_changeable(\"0.00\")

   Returns a reference that can be updated with set-value."
  [tex-expr number-string]
  {:expr tex-expr
   :number-string number-string
   :type :changeable-number})

;; =============================================================================
;; Equation Morphing / Transforms
;; =============================================================================

(defrecord TexTransform [source target transform-type options])

(defn transform-matching-tex
  "Create a transform that morphs between two Tex expressions,
   matching parts by their LaTeX content.

   Options:
   - :key-map - explicit mappings {\"old\" \"new\"}
   - :matched-keys - list of keys that should match
   - :matched-pairs - explicit pairs [[old new] ...]
   - :path-arc - arc angle for animation path
   - :lag-ratio - stagger ratio for parts
   - :run-time - animation duration

   Example:
   (transform-matching-tex
     (tex \"x^2 + y^2 = r^2\")
     (tex \"r = \\\\sqrt{x^2 + y^2}\")
     :key-map {\"=\" \"=\"}
     :path-arc (/ Math/PI 2))"
  [source target & {:keys [key-map matched-keys matched-pairs
                           path-arc lag-ratio run-time]
                    :as options}]
  (->TexTransform source target :matching-tex options))

(defn transform-matching-strings
  "Create a transform matching by string content (more flexible).

   Similar to transform-matching-tex but works on raw strings."
  [source target & {:keys [key-map matched-pairs path-arc]
                    :as options}]
  (->TexTransform source target :matching-strings options))

(defn transform-from-copy
  "Transform from a copy of the source to target."
  [source target & {:keys [path-arc run-time] :as options}]
  {:type :tex-transform
   :transform-type :transform-from-copy
   :source source
   :target target
   :options options})

(defn fade-transform-pieces
  "Fade transform with piece-by-piece animation."
  [source target & {:keys [run-time lag-ratio] :or {run-time 1.0 lag-ratio 0.5} :as options}]
  {:type :tex-transform
   :transform-type :fade-transform-pieces
   :source source
   :target target
   :options options})

(def t
  "Alias for tex function."
  tex)

(defn transform-matching-shapes
  "Transform matching by shape similarity."
  [source target & {:keys [path-arc] :as options}]
  (->TexTransform source target :matching-shapes options))

(defn replacement-transform
  "Replace source with target in the scene.

   In Manim: ReplacementTransform(source, target)"
  [source target & {:keys [path-arc run-time] :as options}]
  (->TexTransform source target :replacement options))

(defn fade-transform
  "Fade from source to target.

   In Manim: FadeTransform(source, target)"
  [source target & {:keys [stretch run-time] :as options}]
  (->TexTransform source target :fade options))

;; =============================================================================
;; Text Styling
;; =============================================================================

(defn set-color
  "Set color of a tex expression or part."
  [tex-expr color]
  {:expr tex-expr
   :color (get-color color)
   :type :set-color})

(defn set-backstroke
  "Add a backstroke (outline) to text for visibility.

   In Manim: label.set_backstroke(BLACK, 5)"
  [tex-expr color width]
  {:expr tex-expr
   :backstroke-color (get-color color)
   :backstroke-width width
   :type :set-backstroke})

(defn set-fill
  "Set fill color and opacity."
  [tex-expr color opacity]
  {:expr tex-expr
   :fill-color (get-color color)
   :fill-opacity opacity
   :type :set-fill})

(defn set-opacity
  "Set opacity of text."
  [tex-expr opacity]
  {:expr tex-expr
   :opacity opacity
   :type :set-opacity})

(defn set-submobject-colors-by-gradient
  "Color submobjects along a gradient.

   In Manim: expr.set_submobject_colors_by_gradient(BLUE, RED)"
  [tex-expr & colors]
  {:expr tex-expr
   :gradient (mapv get-color colors)
   :type :gradient-colors})

;; =============================================================================
;; Brace and Labels
;; =============================================================================

(defrecord Brace [mobject direction buff color label])

(defn brace
  "Create a brace around/next to a mobject.

   Options:
   - :direction - direction brace points (default DOWN)
   - :buff - buffer space
   - :color - brace color
   - :label - text label for the brace"
  [mobject & {:keys [direction buff color label]
              :or {direction :down buff 0.1}}]
  (->Brace mobject direction buff color label))

(defn brace-label
  "Get the label of a brace or create one."
  [brace-obj text & {:keys [font-size]}]
  {:brace brace-obj
   :text text
   :font-size font-size
   :type :brace-label})

(defrecord SurroundingRectangle [mobject buff color stroke-width
                                 fill-color fill-opacity corner-radius])

(defn surrounding-rectangle
  "Create a rectangle surrounding a mobject.

   Options:
   - :buff - padding
   - :color - stroke color
   - :stroke-width - stroke width
   - :fill-color - fill color
   - :fill-opacity - fill opacity
   - :corner-radius - rounded corners"
  [mobject & {:keys [buff color stroke-width fill-color fill-opacity corner-radius]
              :or {buff 0.25 stroke-width 2}}]
  (->SurroundingRectangle mobject buff color stroke-width
                          fill-color fill-opacity corner-radius))

;; =============================================================================
;; Mathematical Formula Patterns
;; =============================================================================

(defn fraction
  "Create a fraction LaTeX expression."
  [numerator denominator & {:keys [t2c]}]
  (tex (str "\\frac{" numerator "}{" denominator "}")
       :t2c t2c))

(defn sqrt
  "Create a square root expression."
  [content & {:keys [t2c]}]
  (tex (str "\\sqrt{" content "}")
       :t2c t2c))

(defn integral
  "Create an integral expression."
  [integrand & {:keys [lower upper var t2c]
                :or {var "x"}}]
  (let [bounds (cond
                 (and lower upper) (str "_{" lower "}^{" upper "}")
                 lower (str "_{" lower "}")
                 upper (str "^{" upper "}")
                 :else "")]
    (tex (str "\\int" bounds " " integrand " d" var)
         :t2c t2c)))

(defn sum
  "Create a summation expression."
  [term & {:keys [var lower upper t2c]
           :or {var "n"}}]
  (let [bounds (str "_{" var "=" (or lower "0") "}^{" (or upper "\\infty") "}")]
    (tex (str "\\sum" bounds " " term)
         :t2c t2c)))

(defn derivative
  "Create a derivative expression."
  [& {:keys [func var order t2c]
      :or {func "f" var "x" order 1}}]
  (let [d-str (if (= order 1) "d" (str "d^{" order "}"))]
    (tex (str "\\frac{" d-str func "}{" d-str var "}")
         :t2c t2c)))

(defn partial-derivative
  "Create a partial derivative expression."
  [& {:keys [func var order t2c]
      :or {func "f" var "x" order 1}}]
  (let [p-str (if (= order 1) "\\partial" (str "\\partial^{" order "}"))]
    (tex (str "\\frac{" p-str " " func "}{" p-str " " var "}")
         :t2c t2c)))

;; =============================================================================
;; Equation Systems
;; =============================================================================

(defn aligned-equations
  "Create aligned equations.

   Example:
   (aligned-equations
     [\"x + y\" \"=\" \"5\"]
     [\"2x - y\" \"=\" \"1\"])"
  [& rows]
  (let [aligned-str (str "\\begin{aligned}\n"
                         (str/join " \\\\\n"
                                   (map #(str/join " & " %) rows))
                         "\n\\end{aligned}")]
    (tex aligned-str)))

(defn cases
  "Create a cases expression (piecewise function).

   Example:
   (cases
     [\"0\" \"x < 0\"]
     [\"x\" \"x \\\\ge 0\"])"
  [& rows]
  (let [cases-str (str "\\begin{cases}\n"
                       (str/join " \\\\\n"
                                 (map #(str/join " & " %) rows))
                       "\n\\end{cases}")]
    (tex cases-str)))

(defn matrix
  "Create a matrix expression.

   Options:
   - :brackets - :round, :square, :curly, :pipes, :none"
  [rows & {:keys [brackets t2c]
           :or {brackets :square}}]
  (let [env (case brackets
              :round "pmatrix"
              :square "bmatrix"
              :curly "Bmatrix"
              :pipes "vmatrix"
              :double-pipes "Vmatrix"
              :none "matrix")]
    (tex (str "\\begin{" env "}\n"
              (str/join " \\\\\n"
                        (map #(str/join " & " %) rows))
              "\n\\end{" env "}")
         :t2c t2c)))

;; =============================================================================
;; Animation Write Patterns
;; =============================================================================

(defrecord TexAnimation [animation-type tex-expr options])

(defn write-tex
  "Write animation for LaTeX.

   In Manim: Write(tex)"
  [tex-expr & {:keys [run-time lag-ratio] :as options}]
  (->TexAnimation :write tex-expr options))

(defn fade-in-tex
  "Fade in LaTeX with optional direction.

   In Manim: FadeIn(tex, shift=UP)"
  [tex-expr & {:keys [shift lag-ratio run-time] :as options}]
  (->TexAnimation :fade-in tex-expr options))

(defn fade-out-tex
  "Fade out LaTeX."
  [tex-expr & {:keys [shift run-time] :as options}]
  (->TexAnimation :fade-out tex-expr options))

(defn indicate
  "Briefly highlight/indicate a tex expression.

   In Manim: Indicate(tex)"
  [tex-expr & {:keys [color scale-factor run-time]
               :or {scale-factor 1.2}
               :as options}]
  (->TexAnimation :indicate tex-expr options))

(defn circumscribe
  "Draw a circle/rectangle around tex.

   In Manim: Circumscribe(tex)"
  [tex-expr & {:keys [shape color stroke-width run-time fade-out]
               :or {shape :rectangle}
               :as options}]
  (->TexAnimation :circumscribe tex-expr options))

(defn flash
  "Flash animation on tex.

   In Manim: Flash(tex)"
  [tex-expr & {:keys [color line-length num-lines run-time]
               :as options}]
  (->TexAnimation :flash tex-expr options))

;; =============================================================================
;; Positioning Helpers
;; =============================================================================

(defn next-to
  "Position relative to another mobject."
  [tex-expr target direction & {:keys [buff aligned-edge]
                                :or {buff 0.25}}]
  {:expr tex-expr
   :target target
   :direction direction
   :buff buff
   :aligned-edge aligned-edge
   :type :next-to})

(defn align-to
  "Align edge of tex to edge of target."
  [tex-expr target edge]
  {:expr tex-expr
   :target target
   :edge edge
   :type :align-to})

(defn move-to
  "Move tex to a point or match another mobject's position."
  [tex-expr target & {:keys [aligned-edge]}]
  {:expr tex-expr
   :target target
   :aligned-edge aligned-edge
   :type :move-to})

(defn to-corner
  "Move tex to a corner of the frame."
  [tex-expr corner & {:keys [buff] :or {buff 0.5}}]
  {:expr tex-expr
   :corner corner
   :buff buff
   :type :to-corner})

(defn to-edge
  "Move tex to an edge of the frame."
  [tex-expr edge & {:keys [buff] :or {buff 0.5}}]
  {:expr tex-expr
   :edge edge
   :buff buff
   :type :to-edge})

(defn shift
  "Shift tex by a vector."
  [tex-expr & direction-amounts]
  {:expr tex-expr
   :shifts (partition 2 direction-amounts)
   :type :shift})

(defn scale
  "Scale tex expression."
  [tex-expr factor & {:keys [about-point about-edge]}]
  {:expr tex-expr
   :factor factor
   :about-point about-point
   :about-edge about-edge
   :type :scale})

;; =============================================================================
;; Common Equation Templates
;; =============================================================================

(defn ode
  "Create an ODE expression.

   Example:
   (ode :var \"\\\\theta\" :time \"t\" :order 2
        :rhs \"- (g/L) \\\\sin(\\\\theta)\")"
  [& {:keys [var time order rhs t2c]
      :or {var "x" time "t" order 1}}]
  (let [prime-str (str/join (repeat order "'"))]
    (tex (str var prime-str "(" time ") = " rhs)
         :t2c t2c)))

(defn laplace-transform
  "Create a Laplace transform expression."
  [func & {:keys [t2c]
           :or {t2c t2c-time-freq}}]
  (tex (str "\\mathcal{L}\\{" func "\\} = \\int_0^\\infty " func " e^{-st} dt")
       :t2c t2c))

(defn fourier-transform
  "Create a Fourier transform expression."
  [func & {:keys [t2c]}]
  (tex (str "\\mathcal{F}\\{" func "\\} = \\int_{-\\infty}^\\infty "
            func " e^{-2\\pi i \\xi t} dt")
       :t2c t2c))

(defn euler-formula
  "Euler's formula with optional customization."
  [& {:keys [t2c]}]
  (tex "e^{i\\theta} = \\cos(\\theta) + i\\sin(\\theta)"
       :t2c (or t2c (t2c {"\\theta" :yellow "i" :blue}))))

(defn taylor-series
  "Create a Taylor series expansion."
  [func & {:keys [var center terms t2c]
           :or {var "x" center "0" terms 4}}]
  (let [terms-str (str/join " + "
                            (map #(str "\\frac{f^{(" % ")}(" center ")}{" % "!}"
                                       "(" var " - " center ")^{" % "}")
                                 (range terms)))]
    (tex (str func "(" var ") = " terms-str " + \\cdots")
         :t2c t2c)))

;; =============================================================================
;; Compile to Manim Python
;; =============================================================================

(defmulti compile-tex
  "Compile typography DSL to Manim Python code."
  (fn [expr] (cond
               (instance? TexExpr expr) :tex-expr
               (instance? T2CMapping expr) :t2c
               (instance? TexTransform expr) :tex-transform
               (instance? TexAnimation expr) :tex-animation
               (instance? Brace expr) :brace
               (instance? SurroundingRectangle expr) :surrounding-rect
               (instance? DecimalNumber expr) :decimal
               (instance? IntegerNumber expr) :integer
               (map? expr) (:type expr)
               :else :default)))

(defmethod compile-tex :t2c
  [{:keys [mappings]}]
  (str "{"
       (str/join ", "
                 (map (fn [[k v]] (str "\"" k "\": \"" v "\""))
                      mappings))
       "}"))

(defmethod compile-tex :tex-expr
  [{:keys [content t2c isolate font-size backstroke]}]
  (let [opts (cond-> []
               t2c (conj (str "t2c=" (compile-tex t2c)))
               isolate (conj (str "isolate=" (pr-str isolate)))
               (not= font-size 48) (conj (str "font_size=" font-size)))]
    (str "Tex(R\"" content "\""
         (when (seq opts)
           (str ", " (str/join ", " opts)))
         ")"
         (when backstroke
           (str ".set_backstroke(" (first backstroke) ", " (second backstroke) ")")))))

(defmethod compile-tex :tex-transform
  [{:keys [source target transform-type options]}]
  (let [type-str (case transform-type
                   :matching-tex "TransformMatchingTex"
                   :matching-strings "TransformMatchingStrings"
                   :matching-shapes "TransformMatchingShapes"
                   :replacement "ReplacementTransform"
                   :fade "FadeTransform")]
    (str type-str "(" (compile-tex source) ", " (compile-tex target)
         (when-let [km (:key-map options)]
           (str ", key_map=" (pr-str km)))
         (when-let [pa (:path-arc options)]
           (str ", path_arc=" pa))
         ")")))

(defmethod compile-tex :default [expr]
  (str expr))

(defn render-typography
  "Render typography to Python code for Manim."
  [& exprs]
  (str/join "\n" (map compile-tex exprs)))

;; =============================================================================
;; Usage Examples
;; =============================================================================

(comment
  ;; Example 1: Basic equation with t2c
  (tex "x^2 + y^2 = r^2"
       :t2c (t2c {"x" :blue "y" :green "r" :red}))

  ;; Example 2: ODE from diffyq videos
  (tex "\\ddot{\\theta}(t) = -\\frac{g}{L}\\sin(\\theta(t))"
       :t2c t2c-theta-omega)

  ;; Example 3: Transform matching
  (transform-matching-tex
   (tex "P(\\max(x_1, x_2) = 0.7)")
   (tex "P(\\max(x_1, x_2) \\le 0.7)")
   :key-map {"=" "\\le"}
   :path-arc (/ Math/PI 2))

  ;; Example 4: Laplace transform equation
  (tex "F(s) = \\int_0^\\infty f(t) e^{-st} dt"
       :t2c t2c-time-freq)

  ;; Example 5: Matrix
  (matrix [["a" "b"]
           ["c" "d"]]
          :brackets :round)

  ;; Example 6: Aligned equations
  (aligned-equations
   ["x + y" "=" "5"]
   ["2x - y" "=" "1"])

  ;; Example 7: Piecewise function
  (cases
   ["0" "x < 0"]
   ["x^2" "0 \\le x < 1"]
   ["1" "x \\ge 1"])

  ;; Example 8: Dynamic numbers
  (-> (tex "x = 0.00")
      (make-number-changeable "0.00"))

  ;; Example 9: Braces and labels
  (brace (tex "x^2 + y^2") :direction :down :label "sum of squares")

  ;; Example 10: Animation sequence
  [(write-tex (tex "f(x) = \\sin(x)"))
   (transform-matching-tex
    (tex "f(x) = \\sin(x)")
    (tex "f'(x) = \\cos(x)"))])

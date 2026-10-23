(ns desargues.manim.mobjects
  "Manim Mobject bindings - all visual objects.
   
   Categories:
   - Basic shapes (Circle, Square, Rectangle, etc.)
   - Lines and arrows
   - Text and LaTeX
   - Graphs and coordinate systems
   - 3D objects
   - Groups and containers"
  (:require [desargues.manim.core :as core]
            [libpython-clj2.python :as py]))

;; ============================================================================
;; Helper for creating mobject constructors
;; ============================================================================

(defn- make-mobject-fn
  "Create a function that constructs a Manim mobject."
  [class-name]
  (fn [& {:as kwargs}]
    (let [cls (core/get-class class-name)]
      (if kwargs
        (py/call-attr-kw cls "__call__" [] kwargs)
        (cls)))))

(defmacro ^:private defmobject
  "Define a mobject constructor function."
  [fn-name class-name & [docstring]]
  `(defn ~fn-name
     ~(or docstring (str "Create a " class-name " mobject."))
     [& {:as kwargs#}]
     (let [cls# (core/get-class ~class-name)]
       (if kwargs#
         (py/call-attr-kw cls# "__call__" [] kwargs#)
         (cls#)))))

;; ============================================================================
;; Basic Shapes
;; ============================================================================

(defn circle
  "Create a Circle mobject.
   
   Options:
   - :radius - circle radius (default 1.0)
   - :color - stroke color
   - :fill_color - fill color
   - :fill_opacity - fill opacity (0-1)"
  [& {:as kwargs}]
  (let [cls (core/get-class "Circle")]
    (if kwargs
      (py/call-attr-kw cls "__call__" [] kwargs)
      (cls))))

(defn dot
  "Create a Dot mobject (small filled circle).
   
   Options:
   - :point - position [x y z]
   - :radius - dot radius
   - :color - dot color"
  [& {:as kwargs}]
  (let [cls (core/get-class "Dot")]
    (if kwargs
      (py/call-attr-kw cls "__call__" [] kwargs)
      (cls))))

(defn square
  "Create a Square mobject.
   
   Options:
   - :side_length - side length (default 2.0)
   - :color - stroke color
   - :fill_color - fill color
   - :fill_opacity - fill opacity"
  [& {:as kwargs}]
  (let [cls (core/get-class "Square")]
    (if kwargs
      (py/call-attr-kw cls "__call__" [] kwargs)
      (cls))))

(defn rectangle
  "Create a Rectangle mobject.
   
   Options:
   - :width - rectangle width
   - :height - rectangle height
   - :color - stroke color"
  [& {:as kwargs}]
  (let [cls (core/get-class "Rectangle")]
    (if kwargs
      (py/call-attr-kw cls "__call__" [] kwargs)
      (cls))))

(defn rounded-rectangle
  "Create a RoundedRectangle mobject."
  [& {:as kwargs}]
  (let [cls (core/get-class "RoundedRectangle")]
    (if kwargs
      (py/call-attr-kw cls "__call__" [] kwargs)
      (cls))))

(defn triangle
  "Create a Triangle mobject."
  [& {:as kwargs}]
  (let [cls (core/get-class "Triangle")]
    (if kwargs
      (py/call-attr-kw cls "__call__" [] kwargs)
      (cls))))

(defn polygon
  "Create a Polygon from vertices.
   
   vertices - sequence of [x y z] points"
  [& vertices]
  (let [cls (core/get-class "Polygon")]
    (apply cls (map core/->py-list vertices))))

(defn regular-polygon
  "Create a RegularPolygon.
   
   Options:
   - :n - number of sides"
  [& {:keys [n] :or {n 6} :as kwargs}]
  (let [cls (core/get-class "RegularPolygon")]
    (py/call-attr-kw cls "__call__" [n] (dissoc kwargs :n))))

(defn star
  "Create a Star mobject.
   
   Options:
   - :n - number of points (default 5)
   - :outer_radius - outer radius
   - :inner_radius - inner radius"
  [& {:as kwargs}]
  (let [cls (core/get-class "Star")]
    (if kwargs
      (py/call-attr-kw cls "__call__" [] kwargs)
      (cls))))

(defn ellipse
  "Create an Ellipse mobject.
   
   Options:
   - :width - ellipse width
   - :height - ellipse height"
  [& {:as kwargs}]
  (let [cls (core/get-class "Ellipse")]
    (if kwargs
      (py/call-attr-kw cls "__call__" [] kwargs)
      (cls))))

(defn annulus
  "Create an Annulus (ring/donut shape).
   
   Options:
   - :inner_radius
   - :outer_radius"
  [& {:as kwargs}]
  (let [cls (core/get-class "Annulus")]
    (if kwargs
      (py/call-attr-kw cls "__call__" [] kwargs)
      (cls))))

(defn sector
  "Create a Sector (pie slice).
   
   Options:
   - :outer_radius
   - :inner_radius
   - :angle - sector angle
   - :start_angle"
  [& {:as kwargs}]
  (let [cls (core/get-class "Sector")]
    (if kwargs
      (py/call-attr-kw cls "__call__" [] kwargs)
      (cls))))

(defn arc
  "Create an Arc.
   
   Options:
   - :radius
   - :start_angle
   - :angle - arc angle"
  [& {:as kwargs}]
  (let [cls (core/get-class "Arc")]
    (if kwargs
      (py/call-attr-kw cls "__call__" [] kwargs)
      (cls))))

;; ============================================================================
;; Lines and Arrows
;; ============================================================================

(defn line
  "Create a Line between two points.
   
   Usage:
   (line [0 0 0] [1 1 0])               ; positional
   (line [0 0 0] [1 1 0] :color RED)    ; with options
   
   Options:
   - :color
   - :stroke_width"
  [start end & {:as kwargs}]
  (let [cls (core/get-class "Line")]
    (if kwargs
      (py/call-attr-kw cls "__call__"
                       [(core/->py-list start) (core/->py-list end)]
                       kwargs)
      (cls (core/->py-list start) (core/->py-list end)))))

(defn dashed-line
  "Create a DashedLine between two points."
  [start end & {:as kwargs}]
  (let [cls (core/get-class "DashedLine")]
    (if kwargs
      (py/call-attr-kw cls "__call__"
                       [(core/->py-list start) (core/->py-list end)]
                       kwargs)
      (cls (core/->py-list start) (core/->py-list end)))))

(defn arrow
  "Create an Arrow between two points.
   
   Options:
   - :buff - buffer from endpoints
   - :stroke_width
   - :tip_length
   - :color"
  [start end & {:as kwargs}]
  (let [cls (core/get-class "Arrow")]
    (if kwargs
      (py/call-attr-kw cls "__call__"
                       [(core/->py-list start) (core/->py-list end)]
                       kwargs)
      (cls (core/->py-list start) (core/->py-list end)))))

(defn double-arrow
  "Create a DoubleArrow (arrows on both ends)."
  [start end & {:as kwargs}]
  (let [cls (core/get-class "DoubleArrow")]
    (if kwargs
      (py/call-attr-kw cls "__call__"
                       [(core/->py-list start) (core/->py-list end)]
                       kwargs)
      (cls (core/->py-list start) (core/->py-list end)))))

(defn curved-arrow
  "Create a CurvedArrow."
  [start end & {:as kwargs}]
  (let [cls (core/get-class "CurvedArrow")]
    (py/call-attr-kw cls "__call__"
                     [(core/->py-list start) (core/->py-list end)]
                     (or kwargs {}))))

(defn vector-arrow
  "Create a Vector (arrow from origin).
   
   direction - [x y z] direction vector"
  [direction & {:as kwargs}]
  (let [cls (core/get-class "Vector")]
    (py/call-attr-kw cls "__call__" [(core/->py-list direction)] (or kwargs {}))))

(defn elbow
  "Create an Elbow (right-angle line)."
  [& {:as kwargs}]
  (let [cls (core/get-class "Elbow")]
    (if kwargs
      (py/call-attr-kw cls "__call__" [] kwargs)
      (cls))))

;; ============================================================================
;; Text and LaTeX
;; ============================================================================

(defn text
  "Create a Text mobject.
   
   text-str - the text string
   
   Options:
   - :font - font name
   - :font_size - font size
   - :color - text color
   - :slant - NORMAL, ITALIC, OBLIQUE
   - :weight - NORMAL, BOLD, etc."
  [text-str & {:as kwargs}]
  (let [cls (core/get-class "Text")]
    (py/call-attr-kw cls "__call__" [text-str] (or kwargs {}))))

(defn tex
  "Create a Tex mobject (LaTeX text mode).
   
   tex-str - LaTeX string"
  [tex-str & {:as kwargs}]
  (let [cls (core/get-class "Tex")]
    (py/call-attr-kw cls "__call__" [tex-str] (or kwargs {}))))

(defn math-tex
  "Create a MathTex mobject (LaTeX math mode).
   
   tex-str - LaTeX math string (without $ delimiters)"
  [tex-str & {:as kwargs}]
  (let [cls (core/get-class "MathTex")]
    (py/call-attr-kw cls "__call__" [tex-str] (or kwargs {}))))

(defn markup-text
  "Create a MarkupText mobject (Pango markup)."
  [text-str & {:as kwargs}]
  (let [cls (core/get-class "MarkupText")]
    (py/call-attr-kw cls "__call__" [text-str] (or kwargs {}))))

(defn title
  "Create a Title mobject."
  [text-str & {:as kwargs}]
  (let [cls (core/get-class "Title")]
    (py/call-attr-kw cls "__call__" [text-str] (or kwargs {}))))

(defn bulleted-list
  "Create a BulletedList."
  [& items]
  (let [cls (core/get-class "BulletedList")]
    (apply cls items)))

(defn paragraph
  "Create a Paragraph mobject."
  [& lines]
  (let [cls (core/get-class "Paragraph")]
    (apply cls lines)))

(defn code
  "Create a Code mobject for syntax-highlighted code.
   
   Options:
   - :code - code string (alternative to file_name)
   - :file_name - path to code file
   - :language - programming language
   - :tab_width - tab width
   - :font - code font"
  [& {:as kwargs}]
  (let [cls (core/get-class "Code")]
    (py/call-attr-kw cls "__call__" [] kwargs)))

;; ============================================================================
;; Numbers and Variables
;; ============================================================================

(defn decimal-number
  "Create a DecimalNumber mobject.
   
   number - the number to display
   
   Options:
   - :num_decimal_places - decimal places
   - :include_sign - show + for positive
   - :group_with_commas - use comma separators"
  [number & {:as kwargs}]
  (let [cls (core/get-class "DecimalNumber")]
    (py/call-attr-kw cls "__call__" [number] (or kwargs {}))))

(defn integer
  "Create an Integer mobject."
  [number & {:as kwargs}]
  (let [cls (core/get-class "Integer")]
    (py/call-attr-kw cls "__call__" [number] (or kwargs {}))))

(defn variable
  "Create a Variable mobject (label = value).
   
   value - initial value
   
   Options:
   - :label - variable label
   - :var_type - DecimalNumber, Integer, etc.
   - :num_decimal_places"
  [value & {:as kwargs}]
  (let [cls (core/get-class "Variable")]
    (py/call-attr-kw cls "__call__" [value] (or kwargs {}))))

(defn value-tracker
  "Create a ValueTracker for animating numeric values.
   
   initial-value - starting value"
  [initial-value]
  (let [cls (core/get-class "ValueTracker")]
    (cls initial-value)))

(defn complex-value-tracker
  "Create a ComplexValueTracker for animating complex numbers."
  [initial-value]
  (let [cls (core/get-class "ComplexValueTracker")]
    (cls initial-value)))

;; ============================================================================
;; Coordinate Systems and Graphs
;; ============================================================================

(defn axes
  "Create an Axes coordinate system.
   
   Options:
   - :x_range - [min max step]
   - :y_range - [min max step]
   - :x_length - x-axis length
   - :y_length - y-axis length
   - :axis_config - config for both axes
   - :tips - show arrow tips"
  [& {:as kwargs}]
  (let [cls (core/get-class "Axes")]
    (if kwargs
      (py/call-attr-kw cls "__call__" [] kwargs)
      (cls))))

(defn number-plane
  "Create a NumberPlane (axes with grid).
   
   Options:
   - :x_range - [min max step]
   - :y_range - [min max step]
   - :background_line_style - grid line style"
  [& {:as kwargs}]
  (let [cls (core/get-class "NumberPlane")]
    (if kwargs
      (py/call-attr-kw cls "__call__" [] kwargs)
      (cls))))

(defn complex-plane
  "Create a ComplexPlane for complex number visualization."
  [& {:as kwargs}]
  (let [cls (core/get-class "ComplexPlane")]
    (if kwargs
      (py/call-attr-kw cls "__call__" [] kwargs)
      (cls))))

(defn polar-plane
  "Create a PolarPlane coordinate system."
  [& {:as kwargs}]
  (let [cls (core/get-class "PolarPlane")]
    (if kwargs
      (py/call-attr-kw cls "__call__" [] kwargs)
      (cls))))

(defn number-line
  "Create a NumberLine.
   
   Options:
   - :x_range - [min max step]
   - :length - line length
   - :include_numbers - show numbers
   - :include_tip - show arrow tip"
  [& {:as kwargs}]
  (let [cls (core/get-class "NumberLine")]
    (if kwargs
      (py/call-attr-kw cls "__call__" [] kwargs)
      (cls))))

(defn three-d-axes
  "Create a ThreeDAxes coordinate system.
   
   Options:
   - :x_range, :y_range, :z_range
   - :x_length, :y_length, :z_length"
  [& {:as kwargs}]
  (let [cls (core/get-class "ThreeDAxes")]
    (if kwargs
      (py/call-attr-kw cls "__call__" [] kwargs)
      (cls))))

;; ============================================================================
;; Function Graphs
;; ============================================================================

(defn function-graph
  "Create a FunctionGraph (plot a function on axes).
   
   Note: Use axes/plot instead for graphs on an Axes object.
   
   func - a Python callable (use core/->py-fn to convert Clojure fn)
   
   Options:
   - :x_range - [min max]
   - :color"
  [func & {:as kwargs}]
  (let [cls (core/get-class "FunctionGraph")]
    (py/call-attr-kw cls "__call__" [func] (or kwargs {}))))

(defn parametric-function
  "Create a ParametricFunction (parametric curve).
   
   func - function taking t and returning [x y z]
   
   Options:
   - :t_range - [min max step]"
  [func & {:as kwargs}]
  (let [cls (core/get-class "ParametricFunction")]
    (py/call-attr-kw cls "__call__" [func] (or kwargs {}))))

(defn implicit-function
  "Create an ImplicitFunction (plot f(x,y) = 0).
   
   func - function of x, y returning a value"
  [func & {:as kwargs}]
  (let [cls (core/get-class "ImplicitFunction")]
    (py/call-attr-kw cls "__call__" [func] (or kwargs {}))))

;; ============================================================================
;; Tables and Matrices
;; ============================================================================

(defn table
  "Create a Table mobject.
   
   table-data - 2D sequence of elements
   
   Options:
   - :row_labels
   - :col_labels
   - :include_outer_lines"
  [table-data & {:as kwargs}]
  (let [cls (core/get-class "Table")
        py-data (core/->py-list (map core/->py-list table-data))]
    (py/call-attr-kw cls "__call__" [py-data] (or kwargs {}))))

(defn matrix
  "Create a Matrix mobject.
   
   matrix-data - 2D sequence of elements"
  [matrix-data & {:as kwargs}]
  (let [cls (core/get-class "Matrix")
        py-data (core/->py-list (map core/->py-list matrix-data))]
    (py/call-attr-kw cls "__call__" [py-data] (or kwargs {}))))

(defn integer-matrix
  "Create an IntegerMatrix."
  [matrix-data & {:as kwargs}]
  (let [cls (core/get-class "IntegerMatrix")
        py-data (core/->py-list (map core/->py-list matrix-data))]
    (py/call-attr-kw cls "__call__" [py-data] (or kwargs {}))))

(defn decimal-matrix
  "Create a DecimalMatrix."
  [matrix-data & {:as kwargs}]
  (let [cls (core/get-class "DecimalMatrix")
        py-data (core/->py-list (map core/->py-list matrix-data))]
    (py/call-attr-kw cls "__call__" [py-data] (or kwargs {}))))

;; ============================================================================
;; Charts
;; ============================================================================

(defn bar-chart
  "Create a BarChart.
   
   values - sequence of values
   
   Options:
   - :bar_names - labels for bars
   - :y_range - [min max step]
   - :bar_colors"
  [values & {:as kwargs}]
  (let [cls (core/get-class "BarChart")]
    (py/call-attr-kw cls "__call__" [(core/->py-list values)] (or kwargs {}))))

;; ============================================================================
;; Groups and Containers
;; ============================================================================

(defn vgroup
  "Create a VGroup (group of VMobjects).
   
   Accepts multiple mobjects as arguments."
  [& mobjects]
  (let [cls (core/get-class "VGroup")]
    (apply cls mobjects)))

(defn group
  "Create a Group of mobjects."
  [& mobjects]
  (let [cls (core/get-class "Group")]
    (apply cls mobjects)))

(defn vdict
  "Create a VDict (dictionary of VMobjects).
   
   mapping - map of keys to mobjects"
  [mapping]
  (let [cls (core/get-class "VDict")]
    (cls (core/->py-dict mapping))))

;; ============================================================================
;; Annotations and Decorations
;; ============================================================================

(defn brace
  "Create a Brace for a mobject.
   
   mobject - the mobject to brace
   
   Options:
   - :direction - brace direction (DOWN, UP, LEFT, RIGHT)"
  [mobject & {:as kwargs}]
  (let [cls (core/get-class "Brace")]
    (py/call-attr-kw cls "__call__" [mobject] (or kwargs {}))))

(defn brace-label
  "Create a BraceLabel (brace with text).
   
   mobject - the mobject to brace
   text - label text"
  [mobject text & {:as kwargs}]
  (let [cls (core/get-class "BraceLabel")]
    (py/call-attr-kw cls "__call__" [mobject text] (or kwargs {}))))

(defn surrounding-rectangle
  "Create a SurroundingRectangle around a mobject."
  [mobject & {:as kwargs}]
  (let [cls (core/get-class "SurroundingRectangle")]
    (py/call-attr-kw cls "__call__" [mobject] (or kwargs {}))))

(defn background-rectangle
  "Create a BackgroundRectangle for a mobject."
  [mobject & {:as kwargs}]
  (let [cls (core/get-class "BackgroundRectangle")]
    (py/call-attr-kw cls "__call__" [mobject] (or kwargs {}))))

(defn cross
  "Create a Cross through a mobject."
  [mobject & {:as kwargs}]
  (let [cls (core/get-class "Cross")]
    (py/call-attr-kw cls "__call__" [mobject] (or kwargs {}))))

(defn underline
  "Create an Underline for a mobject."
  [mobject & {:as kwargs}]
  (let [cls (core/get-class "Underline")]
    (py/call-attr-kw cls "__call__" [mobject] (or kwargs {}))))

;; ============================================================================
;; Geometry Helpers
;; ============================================================================

(defn angle
  "Create an Angle between two lines.
   
   line1 - first line
   line2 - second line"
  [line1 line2 & {:as kwargs}]
  (let [cls (core/get-class "Angle")]
    (py/call-attr-kw cls "__call__" [line1 line2] (or kwargs {}))))

(defn right-angle
  "Create a RightAngle marker."
  [line1 line2 & {:as kwargs}]
  (let [cls (core/get-class "RightAngle")]
    (py/call-attr-kw cls "__call__" [line1 line2] (or kwargs {}))))

(defn tangent-line
  "Create a TangentLine to a curve at a point."
  [vmobject alpha & {:as kwargs}]
  (let [cls (core/get-class "TangentLine")]
    (py/call-attr-kw cls "__call__" [vmobject alpha] (or kwargs {}))))

;; ============================================================================
;; 3D Objects
;; ============================================================================

(defn sphere
  "Create a Sphere.
   
   Options:
   - :radius
   - :resolution - [u_res v_res]"
  [& {:as kwargs}]
  (let [cls (core/get-class "Sphere")]
    (if kwargs
      (py/call-attr-kw cls "__call__" [] kwargs)
      (cls))))

(defn cube
  "Create a Cube.
   
   Options:
   - :side_length"
  [& {:as kwargs}]
  (let [cls (core/get-class "Cube")]
    (if kwargs
      (py/call-attr-kw cls "__call__" [] kwargs)
      (cls))))

(defn cylinder
  "Create a Cylinder.
   
   Options:
   - :radius
   - :height
   - :direction"
  [& {:as kwargs}]
  (let [cls (core/get-class "Cylinder")]
    (if kwargs
      (py/call-attr-kw cls "__call__" [] kwargs)
      (cls))))

(defn cone
  "Create a Cone.
   
   Options:
   - :base_radius
   - :height"
  [& {:as kwargs}]
  (let [cls (core/get-class "Cone")]
    (if kwargs
      (py/call-attr-kw cls "__call__" [] kwargs)
      (cls))))

(defn torus
  "Create a Torus.
   
   Options:
   - :major_radius
   - :minor_radius"
  [& {:as kwargs}]
  (let [cls (core/get-class "Torus")]
    (if kwargs
      (py/call-attr-kw cls "__call__" [] kwargs)
      (cls))))

(defn prism
  "Create a Prism.
   
   Options:
   - :dimensions - [x y z]"
  [& {:as kwargs}]
  (let [cls (core/get-class "Prism")]
    (if kwargs
      (py/call-attr-kw cls "__call__" [] kwargs)
      (cls))))

(defn arrow-3d
  "Create a 3D Arrow."
  [start end & {:as kwargs}]
  (let [cls (core/get-class "Arrow3D")]
    (py/call-attr-kw cls "__call__"
                     [(core/->py-list start) (core/->py-list end)]
                     (or kwargs {}))))

(defn line-3d
  "Create a 3D Line."
  [start end & {:as kwargs}]
  (let [cls (core/get-class "Line3D")]
    (py/call-attr-kw cls "__call__"
                     [(core/->py-list start) (core/->py-list end)]
                     (or kwargs {}))))

(defn dot-3d
  "Create a 3D Dot."
  [& {:as kwargs}]
  (let [cls (core/get-class "Dot3D")]
    (if kwargs
      (py/call-attr-kw cls "__call__" [] kwargs)
      (cls))))

(defn surface
  "Create a Surface from a function.
   
   func - function of u, v returning [x y z]
   
   Options:
   - :u_range - [min max]
   - :v_range - [min max]
   - :resolution"
  [func & {:as kwargs}]
  (let [cls (core/get-class "Surface")]
    (py/call-attr-kw cls "__call__" [func] (or kwargs {}))))

;; Platonic Solids
(defn tetrahedron
  "Create a Tetrahedron."
  [& {:as kwargs}]
  (let [cls (core/get-class "Tetrahedron")]
    (if kwargs
      (py/call-attr-kw cls "__call__" [] kwargs)
      (cls))))

(defn octahedron
  "Create an Octahedron."
  [& {:as kwargs}]
  (let [cls (core/get-class "Octahedron")]
    (if kwargs
      (py/call-attr-kw cls "__call__" [] kwargs)
      (cls))))

(defn dodecahedron
  "Create a Dodecahedron."
  [& {:as kwargs}]
  (let [cls (core/get-class "Dodecahedron")]
    (if kwargs
      (py/call-attr-kw cls "__call__" [] kwargs)
      (cls))))

(defn icosahedron
  "Create an Icosahedron."
  [& {:as kwargs}]
  (let [cls (core/get-class "Icosahedron")]
    (if kwargs
      (py/call-attr-kw cls "__call__" [] kwargs)
      (cls))))

;; ============================================================================
;; Images and SVG
;; ============================================================================

(defn image-mobject
  "Create an ImageMobject from a file.
   
   filename - path to image file"
  [filename & {:as kwargs}]
  (let [cls (core/get-class "ImageMobject")]
    (py/call-attr-kw cls "__call__" [filename] (or kwargs {}))))

(defn svg-mobject
  "Create an SVGMobject from a file.
   
   filename - path to SVG file"
  [filename & {:as kwargs}]
  (let [cls (core/get-class "SVGMobject")]
    (py/call-attr-kw cls "__call__" [filename] (or kwargs {}))))

;; ============================================================================
;; Vector Fields
;; ============================================================================

(defn vector-field
  "Create a VectorField.
   
   func - function taking [x y z] and returning [vx vy vz]"
  [func & {:as kwargs}]
  (let [cls (core/get-class "VectorField")]
    (py/call-attr-kw cls "__call__" [func] (or kwargs {}))))

(defn arrow-vector-field
  "Create an ArrowVectorField."
  [func & {:as kwargs}]
  (let [cls (core/get-class "ArrowVectorField")]
    (py/call-attr-kw cls "__call__" [func] (or kwargs {}))))

(defn stream-lines
  "Create StreamLines for a vector field."
  [func & {:as kwargs}]
  (let [cls (core/get-class "StreamLines")]
    (py/call-attr-kw cls "__call__" [func] (or kwargs {}))))

;; ============================================================================
;; Special Objects
;; ============================================================================

(defn traced-path
  "Create a TracedPath that follows a point.
   
   traced-point-func - function returning the point to trace"
  [traced-point-func & {:as kwargs}]
  (let [cls (core/get-class "TracedPath")]
    (py/call-attr-kw cls "__call__" [traced-point-func] (or kwargs {}))))

(defn screen-rectangle
  "Create a ScreenRectangle (16:9 rectangle)."
  [& {:as kwargs}]
  (let [cls (core/get-class "ScreenRectangle")]
    (if kwargs
      (py/call-attr-kw cls "__call__" [] kwargs)
      (cls))))

(defn full-screen-rectangle
  "Create a FullScreenRectangle."
  [& {:as kwargs}]
  (let [cls (core/get-class "FullScreenRectangle")]
    (if kwargs
      (py/call-attr-kw cls "__call__" [] kwargs)
      (cls))))

;; ============================================================================
;; Mobject Methods (operate on any mobject)
;; ============================================================================

(defn set-color
  "Set the color of a mobject. Returns the mobject."
  [mobject color]
  (py/call-attr mobject "set_color" color)
  mobject)

(defn set-fill
  "Set the fill color and opacity. Returns the mobject."
  [mobject color & {:keys [opacity] :or {opacity 1.0}}]
  (py/call-attr-kw mobject "set_fill" [color] {:opacity opacity})
  mobject)

(defn set-stroke
  "Set the stroke color and width. Returns the mobject."
  [mobject & {:keys [color width opacity]
              :or {width 4 opacity 1.0}}]
  (let [kwargs (cond-> {:width width :opacity opacity}
                 color (assoc :color color))]
    (py/call-attr-kw mobject "set_stroke" [] kwargs))
  mobject)

(defn move-to
  "Move mobject center to a point. Returns the mobject."
  [mobject point]
  (py/call-attr mobject "move_to" (core/->py-list point))
  mobject)

(defn shift
  "Shift mobject by a vector. Returns the mobject."
  [mobject & vectors]
  (doseq [v vectors]
    (py/call-attr mobject "shift" (core/->py-list v)))
  mobject)

(defn scale
  "Scale mobject by a factor. Returns the mobject."
  [mobject factor & {:as kwargs}]
  (py/call-attr-kw mobject "scale" [factor] (or kwargs {}))
  mobject)

(defn rotate
  "Rotate mobject by an angle. Returns the mobject.
   
   Options:
   - :axis - rotation axis (default Z_AXIS)
   - :about_point - point to rotate around"
  [mobject angle & {:as kwargs}]
  (py/call-attr-kw mobject "rotate" [angle] (or kwargs {}))
  mobject)

(defn flip
  "Flip mobject. Returns the mobject.
   
   Options:
   - :axis - flip axis"
  [mobject & {:as kwargs}]
  (py/call-attr-kw mobject "flip" [] (or kwargs {}))
  mobject)

(defn next-to
  "Position mobject next to another. Returns the mobject.
   
   direction - e.g., RIGHT, UP, DOWN, LEFT
   
   Options:
   - :buff - buffer distance"
  [mobject other direction & {:as kwargs}]
  (py/call-attr-kw mobject "next_to" [other direction] (or kwargs {}))
  mobject)

(defn align-to
  "Align mobject to another. Returns the mobject."
  [mobject other direction]
  (py/call-attr mobject "align_to" other direction)
  mobject)

(defn to-edge
  "Move mobject to an edge of the frame. Returns the mobject."
  [mobject edge & {:keys [buff] :or {buff 0.5}}]
  (py/call-attr-kw mobject "to_edge" [edge] {:buff buff})
  mobject)

(defn to-corner
  "Move mobject to a corner of the frame. Returns the mobject."
  [mobject corner & {:keys [buff] :or {buff 0.5}}]
  (py/call-attr-kw mobject "to_corner" [corner] {:buff buff})
  mobject)

(defn center
  "Center the mobject on screen. Returns the mobject."
  [mobject]
  (py/call-attr mobject "center")
  mobject)

(defn get-center
  "Get the center point of a mobject."
  [mobject]
  (core/py->clj (py/call-attr mobject "get_center")))

(defn get-width
  "Get the width of a mobject."
  [mobject]
  (py/call-attr mobject "get_width"))

(defn get-height
  "Get the height of a mobject."
  [mobject]
  (py/call-attr mobject "get_height"))

(defn set-width
  "Set the width of a mobject. Returns the mobject."
  [mobject width & {:keys [stretch] :or {stretch false}}]
  (py/call-attr-kw mobject "set_width" [width] {:stretch stretch})
  mobject)

(defn set-height
  "Set the height of a mobject. Returns the mobject."
  [mobject height & {:keys [stretch] :or {stretch false}}]
  (py/call-attr-kw mobject "set_height" [height] {:stretch stretch})
  mobject)

(defn copy
  "Create a copy of a mobject."
  [mobject]
  (py/call-attr mobject "copy"))

(defn add-updater
  "Add an updater function to a mobject.
   
   updater-fn - function taking the mobject (and optionally dt)"
  [mobject updater-fn]
  (py/call-attr mobject "add_updater" (core/->py-updater updater-fn))
  mobject)

(defn remove-updater
  "Remove an updater from a mobject."
  [mobject updater-fn]
  (py/call-attr mobject "remove_updater" updater-fn)
  mobject)

(defn clear-updaters
  "Remove all updaters from a mobject."
  [mobject]
  (py/call-attr mobject "clear_updaters")
  mobject)

(defn suspend-updating
  "Suspend updating for a mobject."
  [mobject]
  (py/call-attr mobject "suspend_updating")
  mobject)

(defn resume-updating
  "Resume updating for a mobject."
  [mobject]
  (py/call-attr mobject "resume_updating")
  mobject)

(defn arrange
  "Arrange submobjects in a VGroup.
   
   direction - arrangement direction (RIGHT, DOWN, etc.)
   
   Options:
   - :buff - buffer between objects
   - :center - center the group"
  [vgroup direction & {:as kwargs}]
  (py/call-attr-kw vgroup "arrange" [direction] (or kwargs {}))
  vgroup)

(defn arrange-in-grid
  "Arrange submobjects in a grid.
   
   Options:
   - :rows
   - :cols
   - :buff"
  [vgroup & {:as kwargs}]
  (py/call-attr-kw vgroup "arrange_in_grid" [] (or kwargs {}))
  vgroup)

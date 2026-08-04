(ns desargues.brachistochrone
  "Brachistochrone problem: curve of fastest descent under gravity.

  Uses Emmy for symbolic computation of:
  - Lagrangian mechanics (L = T - V)
  - Action integral (S = ∫ L dt)
  - Calculus of variations (Euler-Lagrange equation)

  Uses Manim for visualization of:
  - Multiple candidate curves (straight, parabola, cycloid)
  - Bodies sliding down each path
  - Action values and comparison"
  (:require [emmy.env :as e :refer [->TeX D simplify sin cos square sqrt
                                    literal-function up partial]]
            [emmy.calculus.derivative :refer [D]]
            [emmy.mechanics.lagrange :as lag]
            [libpython-clj2.python :as py]
            [desargues.config :as config]
            [desargues.manim-quickstart :as mq]
            [desargues.emmy-manim :as em]))

;; ============================================================================
;; Physics: Lagrangian Mechanics
;; ============================================================================

(defn kinetic-energy
  "Kinetic energy T = (1/2) m v²"
  [m v]
  (e/* (e// 1 2) m (square v)))

(defn potential-energy
  "Potential energy V = m g y (taking y positive upward)"
  [m g y]
  (e/* m g y))

(defn lagrangian
  "Lagrangian L = T - V for a particle on a curve under gravity"
  [m g]
  (fn [[_ [x y] [vx vy]]]
    (let [v-squared (e/+ (square vx) (square vy))
          T (e/* (e// 1 2) m v-squared)
          V (e/* m g y)]
      (e/- T V))))

(defn lagrangian-parametric
  "Lagrangian for a curve y(x) parameterized by x.
  The particle descends under gravity starting from rest.

  From energy conservation: v² = 2gy (if starting from y=0)
  Arc length: ds = √(1 + (dy/dx)²) dx
  Time element: dt = ds/v

  This gives the functional to minimize:
  T = ∫ √[(1 + y'²)/(2gy)] dx"
  []
  (fn [y-of-x]
    (fn [x]
      (let [y (y-of-x x)
            dy-dx ((D y-of-x) x)
            ;; Arc length element squared
            ds-squared (e/+ 1 (square dy-dx))
            ;; Velocity squared from energy conservation (v² = 2g|y|)
            ;; Use absolute value since y is negative (below start point)
            v-squared (e/* 2 9.8 (e/abs y))]
        ;; Return 1/v to minimize time (since T = ∫ ds/v)
        (e// (sqrt ds-squared) (sqrt v-squared))))))

;; ============================================================================
;; Curves: Candidate Paths from A to B
;; ============================================================================

(defn straight-line
  "Straight line from (x0, y0) to (x1, y1)"
  [x0 y0 x1 y1]
  (fn [t]
    (let [x (e/+ x0 (e/* t (e/- x1 x0)))
          y (e/+ y0 (e/* t (e/- y1 y0)))]
      [x y])))

(defn parabolic-path
  "Parabolic path from (x0, y0) to (x1, y1) with extra dip"
  [x0 y0 x1 y1 depth]
  (fn [t]
    (let [x (e/+ x0 (e/* t (e/- x1 x0)))
          ;; Quadratic that goes through endpoints with dip in middle
          y (e/+ y0
                 (e/* t (e/- y1 y0))
                 (e/* (e/- depth)
                      (e/* t (e/- 1 t))
                      (e/abs (e/- y1 y0))))]
      [x y])))

(defn cycloid-path
  "Cycloid path: the brachistochrone solution.

  Parametric equations:
    x = r(θ - sin(θ))
    y = -r(1 - cos(θ))

  Scaled to go from (x0, y0) to (x1, y1)"
  [x0 y0 x1 y1]
  (fn [theta]
    (let [theta-max 2.5  ; Control the shape
          r (e// (e/- x1 x0) (e/- theta-max (sin theta-max)))
          ;; Standard cycloid
          x-raw (e/+ x0 (e/* r (e/- theta (sin theta))))
          y-raw (e/- y0 (e/* r (e/- 1 (cos theta))))
          ;; Scale y to reach endpoint
          y-end (e/- y0 (e/* r (e/- 1 (cos theta-max))))
          y-scale (e// (e/- y1 y0) (e/- y-end y0))
          y (e/+ y0 (e/* (e/- y-raw y0) y-scale))]
      [x-raw y])))

;; ============================================================================
;; Action Calculation
;; ============================================================================

(defn descent-time
  "Estimate time to descend along a curve.
  Uses energy conservation and arc length."
  [curve-fn t-start t-end n-points g]
  (let [dt (/ (- t-end t-start) n-points)
        points (map #(curve-fn (+ t-start (* % dt))) (range n-points))]
    (reduce +
            (map (fn [[x1 y1] [x2 y2]]
                   (let [dx (- x2 x1)
                         dy (- y2 y1)
                         ds (Math/sqrt (+ (* dx dx) (* dy dy)))
                         ;; Average height (negative, below start)
                         y-avg (/ (+ y1 y2) 2.0)
                         ;; Velocity from energy conservation
                         v (Math/sqrt (* 2 g (Math/abs y-avg)))]
                     (if (pos? v)
                       (/ ds v)
                       0.0)))
                 points
                 (rest points)))))

(defn action-integral
  "Compute action S = ∫ L dt for a path.
  This is a simplified calculation for demonstration."
  [curve-fn t-start t-end n-points m g]
  (let [dt (/ (- t-end t-start) n-points)
        points (map #(curve-fn (+ t-start (* % dt))) (range (inc n-points)))]
    (reduce +
            (map (fn [[x1 y1] [x2 y2]]
                   (let [dx (- x2 x1)
                         dy (- y2 y1)
                         vx (/ dx dt)
                         vy (/ dy dt)
                         v-sq (+ (* vx vx) (* vy vy))
                         y-avg (/ (+ y1 y2) 2.0)
                         ;; L = T - V
                         T (* 0.5 m v-sq)
                         V (* m g y-avg)
                         L (- T V)]
                     (* L dt)))
                 points
                 (rest points)))))

;; ============================================================================
;; LaTeX Generation for Display
;; ============================================================================

(defn generate-lagrangian-latex
  "Generate LaTeX for the Lagrangian"
  []
  (let [;; Symbolic Lagrangian
        L (lagrangian 'm 'g)
        ;; Create a local tuple for symbolic evaluation
        local-tuple (up 't (up 'x 'y) (up 'vx 'vy))
        L-expr (L local-tuple)]
    (->TeX (simplify L-expr))))

(defn generate-euler-lagrange-latex
  "Generate LaTeX for Euler-Lagrange equation"
  []
  ;; The general form
  "\\frac{\\partial L}{\\partial y} - \\frac{d}{dt}\\frac{\\partial L}{\\partial \\dot{y}} = 0")

(defn generate-cycloid-solution-latex
  "Generate LaTeX for cycloid parametric equations"
  []
  ["x = r(\\theta - \\sin\\theta)"
   "y = r(1 - \\cos\\theta)"])

(defn generate-action-functional-latex
  "Generate LaTeX for the action functional to minimize"
  []
  "S = \\int_{t_0}^{t_1} L(q, \\dot{q}, t) \\, dt")

(defn generate-brachistochrone-functional-latex
  "Generate LaTeX for the specific brachistochrone functional"
  []
  "T = \\int_{x_0}^{x_1} \\frac{\\sqrt{1 + (y')^2}}{\\sqrt{2gy}} \\, dx")

;; ============================================================================
;; Example: Compute for Specific Points
;; ============================================================================

(defn compare-paths
  "Compare descent times and actions for different paths from A to B"
  [x0 y0 x1 y1]
  (let [g 9.8  ; gravity
        m 1.0  ; mass (cancels out in many calculations)
        n-points 100

        ;; Define the three curves
        straight (straight-line x0 y0 x1 y1)
        parabola (parabolic-path x0 y0 x1 y1 2.0)
        cycloid (cycloid-path x0 y0 x1 y1)

        ;; Compute times and actions
        time-straight (descent-time straight 0.0 1.0 n-points g)
        time-parabola (descent-time parabola 0.0 1.0 n-points g)
        time-cycloid (descent-time cycloid 0.0 2.5 n-points g)

        action-straight (action-integral straight 0.0 1.0 n-points m g)
        action-parabola (action-integral parabola 0.0 1.0 n-points m g)
        action-cycloid (action-integral cycloid 0.0 2.5 n-points m g)]

    {:straight {:time time-straight :action action-straight}
     :parabola {:time time-parabola :action action-parabola}
     :cycloid {:time time-cycloid :action action-cycloid}}))

;; ============================================================================
;; Manim Integration
;; ============================================================================

(defn create-brachistochrone-data
  "Create all the data needed for the Manim animation"
  [x0 y0 x1 y1]
  (let [comparison (compare-paths x0 y0 x1 y1)
        lagrangian-latex (generate-lagrangian-latex)
        euler-lagrange-latex (generate-euler-lagrange-latex)
        cycloid-latex (generate-cycloid-solution-latex)
        action-latex (generate-action-functional-latex)
        brach-functional-latex (generate-brachistochrone-functional-latex)]

    {:start-point [x0 y0 0]
     :end-point [x1 y1 0]
     :curves {:straight {:time (get-in comparison [:straight :time])
                         :action (get-in comparison [:straight :action])}
              :parabola {:time (get-in comparison [:parabola :time])
                         :action (get-in comparison [:parabola :action])}
              :cycloid {:time (get-in comparison [:cycloid :time])
                        :action (get-in comparison [:cycloid :action])}}
     :latex {:lagrangian lagrangian-latex
             :euler-lagrange euler-lagrange-latex
             :cycloid cycloid-latex
             :action action-latex
             :functional brach-functional-latex}}))

(defn render-brachistochrone-scene
  "Render the brachistochrone animation using Manim"
  []
  (mq/init!)

  ;; Compute all the physics
  (let [data (create-brachistochrone-data -4.0 2.0 4.0 -2.0)]

    (println "\n=== Brachistochrone Problem ===")
    (println "Start point A:" (:start-point data))
    (println "End point B:" (:end-point data))
    (println "\nDescent times:")
    (println "  Straight line:" (format "%.3f s" (get-in data [:curves :straight :time])))
    (println "  Parabola:" (format "%.3f s" (get-in data [:curves :parabola :time])))
    (println "  Cycloid:" (format "%.3f s" (get-in data [:curves :cycloid :time])))
    (println "\nActions:")
    (println "  Straight line:" (format "%.2f" (get-in data [:curves :straight :action])))
    (println "  Parabola:" (format "%.2f" (get-in data [:curves :parabola :action])))
    (println "  Cycloid:" (format "%.2f" (get-in data [:curves :cycloid :action])))
    (println "\nLagrangian (LaTeX):")
    (println "  " (get-in data [:latex :lagrangian]))

    ;; Import and render the Python scene
    (config/add-project-to-syspath! "py")

    (let [scenes (py/import-module "brachistochrone_scene")
          BrachistochroneProblem (py/get-attr scenes "BrachistochroneProblem")
          scene (BrachistochroneProblem
                 :start_point (:start-point data)
                 :end_point (:end-point data)
                 :gravity 9.8)]
      (mq/render-scene! scene))))

(defn render-derivation-scene
  "Render the mathematical derivation scene"
  []
  (mq/init!)

  (config/add-project-to-syspath! "py")

  (let [scenes (py/import-module "brachistochrone_scene")
        BrachistochroneDerivation (py/get-attr scenes "BrachistochroneDerivation")
        scene (BrachistochroneDerivation)]
    (mq/render-scene! scene)))

(defn render-combined-scene
  "Render scene with both math and animation"
  []
  (mq/init!)

  (let [data (create-brachistochrone-data -4.0 2.0 4.0 -2.0)]
    (config/add-project-to-syspath! "py")

    (let [scenes (py/import-module "brachistochrone_scene")
          BrachistochroneWithMath (py/get-attr scenes "BrachistochroneWithMath")
          scene (BrachistochroneWithMath
                 :lagrangian_latex (get-in data [:latex :lagrangian])
                 :euler_lagrange_latex (get-in data [:latex :euler-lagrange])
                 :solution_latex (first (get-in data [:latex :cycloid])))]
      (mq/render-scene! scene))))

;; ============================================================================
;; REPL Examples
;; ============================================================================

(comment
  ;; Initialize
  (mq/init!)

  ;; Explore the Lagrangian
  (generate-lagrangian-latex)
  ;; => LaTeX string for L = T - V

  ;; Compare the three paths
  (compare-paths -4.0 2.0 4.0 -2.0)
  ;; => {:straight {:time ..., :action ...}
  ;;     :parabola {:time ..., :action ...}
  ;;     :cycloid {:time ..., :action ...}}

  ;; Generate all data
  (create-brachistochrone-data -4.0 2.0 4.0 -2.0)

  ;; Render the main animation
  (render-brachistochrone-scene)

  ;; Render the derivation
  (render-derivation-scene)

  ;; Render combined scene
  (render-combined-scene)

  ;; Explore the action functional
  (generate-brachistochrone-functional-latex)

  ;; Explore cycloid equations
  (generate-cycloid-solution-latex))

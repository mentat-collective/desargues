(ns desargues.specs.common
  "Common spec definitions shared across the desargues codebase.
   
   These specs define the fundamental primitives used throughout:
   - Numeric types (positive, non-negative, unit interval)
   - Geometric types (points, vectors, ranges)
   - Color types (keywords, RGB tuples)
   - Time/duration types"
  (:require [clojure.spec.alpha :as s]))

;; =============================================================================
;; Numeric Primitives
;; =============================================================================

(s/def ::number number?)

(s/def ::positive-number (s/and number? pos?))

(s/def ::non-negative-number (s/and number? #(>= % 0)))

(s/def ::negative-number (s/and number? neg?))

;; A number in the range [0, 1]
(s/def ::unit-interval (s/and number? #(<= 0 % 1)))

;; An angle in radians
(s/def ::angle number?)

;; A small angle suitable for pendulum approximations
(s/def ::small-angle (s/and number? #(< (abs %) (/ Math/PI 2))))

;; =============================================================================
;; Geometric Primitives
;; =============================================================================

(s/def ::coordinate number?)

;; A 2D point as [x y]
(s/def ::point-2d (s/tuple ::coordinate ::coordinate))

;; A 3D point as [x y z]
(s/def ::point-3d (s/tuple ::coordinate ::coordinate ::coordinate))

;; A point in 2D or 3D space
(s/def ::point
  (s/or :2d ::point-2d
        :3d ::point-3d))

;; A 2D vector
(s/def ::vector-2d (s/tuple number? number?))

;; A 3D vector
(s/def ::vector-3d (s/tuple number? number? number?))

;; A range specification as [min max step]
(s/def ::axis-range
  (s/and (s/tuple number? number? ::positive-number)
         (fn [[min-val max-val _]]
           (< min-val max-val))))

(s/def ::x-range ::axis-range)
(s/def ::y-range ::axis-range)
(s/def ::z-range ::axis-range)

;; =============================================================================
;; Direction/Position Constants
;; =============================================================================

;; A cardinal direction keyword
(s/def ::direction
  #{:up :down :left :right :ul :ur :dl :dr
    :UP :DOWN :LEFT :RIGHT :UL :UR :DL :DR})

;; An alignment specification
(s/def ::alignment #{:center :left :right :top :bottom})

;; =============================================================================
;; Color Types
;; =============================================================================

;; A named color keyword
(s/def ::color-keyword
  #{:white :black :red :green :blue :yellow :orange :purple
    :cyan :magenta :pink :gray :grey :brown
    :WHITE :BLACK :RED :GREEN :BLUE :YELLOW :ORANGE :PURPLE
    :CYAN :MAGENTA :PINK :GRAY :GREY :BROWN
    ;; Manim-specific colors
    :BLUE_A :BLUE_B :BLUE_C :BLUE_D :BLUE_E
    :GREEN_A :GREEN_B :GREEN_C :GREEN_D :GREEN_E
    :RED_A :RED_B :RED_C :RED_D :RED_E
    :YELLOW_A :YELLOW_B :YELLOW_C :YELLOW_D :YELLOW_E
    :GOLD :GOLD_A :GOLD_B :GOLD_C :GOLD_D :GOLD_E
    :TEAL :TEAL_A :TEAL_B :TEAL_C :TEAL_D :TEAL_E
    :MAROON :MAROON_A :MAROON_B :MAROON_C :MAROON_D :MAROON_E
    :PURPLE_A :PURPLE_B :PURPLE_C :PURPLE_D :PURPLE_E})

(s/def ::rgb-component ::unit-interval)

;; An RGB color tuple with components in [0, 1]
(s/def ::rgb-tuple (s/tuple ::rgb-component ::rgb-component ::rgb-component))

;; An RGBA color tuple with components in [0, 1]
(s/def ::rgba-tuple (s/tuple ::rgb-component ::rgb-component ::rgb-component ::rgb-component))

;; A hex color string like "#FF0000"
(s/def ::hex-color
  (s/and string?
         #(re-matches #"^#[0-9A-Fa-f]{6}$" %)))

;; Any valid color representation
(s/def ::color
  (s/or :keyword ::color-keyword
        :rgb ::rgb-tuple
        :rgba ::rgba-tuple
        :hex ::hex-color))

;; =============================================================================
;; Time/Duration Types
;; =============================================================================

;; A positive duration in seconds
(s/def ::duration ::positive-number)

;; Animation run time in seconds
(s/def ::run-time ::positive-number)

;; A time step for numerical integration
(s/def ::time-step (s/and ::positive-number #(< % 1.0)))

;; Animation progress value in [0, 1]
(s/def ::alpha ::unit-interval)

;; =============================================================================
;; Scale/Size Types
;; =============================================================================

;; A positive scale factor
(s/def ::scale-factor ::positive-number)

;; Stroke width (non-negative)
(s/def ::stroke-width ::non-negative-number)

;; Opacity value in [0, 1]
(s/def ::opacity ::unit-interval)

;; Font size (positive)
(s/def ::font-size ::positive-number)

;; =============================================================================
;; String Types
;; =============================================================================

(s/def ::non-empty-string (s/and string? #(pos? (count %))))

;; A LaTeX-formatted string
(s/def ::latex-string ::non-empty-string)

;; Python code as a string
(s/def ::python-code string?)

;; =============================================================================
;; Identifier Types
;; =============================================================================

;; A unique identifier (symbol or keyword)
(s/def ::id
  (s/or :symbol symbol?
        :keyword keyword?))

;; A name (string, symbol, or keyword)
(s/def ::name
  (s/or :string string?
        :symbol symbol?
        :keyword keyword?))

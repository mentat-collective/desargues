(ns desargues.manim.all
  "Complete Manim bindings - convenience namespace that requires all submodules.
   
   This namespace provides a single require for all Manim functionality:
   
   (require '[desargues.manim.all :as manim])
   
   Or with aliases for each submodule:
   
   (require '[desargues.manim.core :as core]
            '[desargues.manim.mobjects :as m]
            '[desargues.manim.animations :as a]
            '[desargues.manim.scenes :as s]
            '[desargues.manim.constants :as c])
   
   Quick Start Example:
   
   (require '[desargues.manim.all :as manim])
   (manim/init!)
   
   (manim/run-scene
     (let [circle (manim/circle :color @manim/BLUE)]
       (manim/play! self (manim/create circle))
       (manim/wait! self 1)
       (manim/play! self (manim/fade-out circle))))
   
   Available Namespaces:
   - desargues.manim.core       - Initialization, Python interop, base functions
   - desargues.manim.mobjects   - All visual objects (shapes, text, graphs, 3D)
   - desargues.manim.animations - All animation types
   - desargues.manim.scenes     - Scene types and camera operations  
   - desargues.manim.constants  - Colors, directions, mathematical constants"
  (:require [desargues.manim.core :as core]
            [desargues.manim.mobjects :as mobjects]
            [desargues.manim.animations :as animations]
            [desargues.manim.scenes :as scenes]
            [desargues.manim.constants :as constants]))

;; ============================================================================
;; Re-export Core Functions
;; ============================================================================

(def init! core/init!)
(def manim core/manim)
(def get-class core/get-class)
(def get-constant core/get-constant)
(def render-scene! core/render-scene!)
(def play! core/play!)
(def wait! core/wait!)
(def add! core/add!)
(def remove! core/remove!)
(def clear! core/clear!)

;; Python interop
(def ->py-list core/->py-list)
(def ->py-dict core/->py-dict)
(def ->py-tuple core/->py-tuple)
(def py->clj core/py->clj)

;; ============================================================================
;; Re-export Mobjects
;; ============================================================================

;; Basic shapes
(def circle mobjects/circle)
(def dot mobjects/dot)
(def square mobjects/square)
(def rectangle mobjects/rectangle)
(def rounded-rectangle mobjects/rounded-rectangle)
(def triangle mobjects/triangle)
(def polygon mobjects/polygon)
(def regular-polygon mobjects/regular-polygon)
(def star mobjects/star)
(def ellipse mobjects/ellipse)
(def annulus mobjects/annulus)
(def sector mobjects/sector)
(def arc mobjects/arc)

;; Lines and arrows
(def line mobjects/line)
(def dashed-line mobjects/dashed-line)
(def arrow mobjects/arrow)
(def double-arrow mobjects/double-arrow)
(def curved-arrow mobjects/curved-arrow)
(def vector-arrow mobjects/vector-arrow)
(def elbow mobjects/elbow)

;; Text and LaTeX
(def text mobjects/text)
(def tex mobjects/tex)
(def math-tex mobjects/math-tex)
(def markup-text mobjects/markup-text)
(def title mobjects/title)
(def bulleted-list mobjects/bulleted-list)
(def paragraph mobjects/paragraph)
(def code mobjects/code)

;; Numbers
(def decimal-number mobjects/decimal-number)
(def integer mobjects/integer)
(def variable mobjects/variable)
(def value-tracker mobjects/value-tracker)

;; Coordinate systems
(def axes mobjects/axes)
(def number-plane mobjects/number-plane)
(def complex-plane mobjects/complex-plane)
(def polar-plane mobjects/polar-plane)
(def number-line mobjects/number-line)
(def three-d-axes mobjects/three-d-axes)

;; Graphs
(def function-graph mobjects/function-graph)
(def parametric-function mobjects/parametric-function)
(def implicit-function mobjects/implicit-function)

;; Tables and matrices
(def table mobjects/table)
(def matrix mobjects/matrix)
(def bar-chart mobjects/bar-chart)

;; Groups
(def vgroup mobjects/vgroup)
(def group mobjects/group)
(def vdict mobjects/vdict)

;; Annotations
(def brace mobjects/brace)
(def brace-label mobjects/brace-label)
(def surrounding-rectangle mobjects/surrounding-rectangle)
(def background-rectangle mobjects/background-rectangle)
(def cross mobjects/cross)
(def underline mobjects/underline)

;; 3D objects
(def sphere mobjects/sphere)
(def cube mobjects/cube)
(def cylinder mobjects/cylinder)
(def cone mobjects/cone)
(def torus mobjects/torus)
(def prism mobjects/prism)
(def arrow-3d mobjects/arrow-3d)
(def line-3d mobjects/line-3d)
(def dot-3d mobjects/dot-3d)
(def surface mobjects/surface)
(def tetrahedron mobjects/tetrahedron)
(def octahedron mobjects/octahedron)
(def dodecahedron mobjects/dodecahedron)
(def icosahedron mobjects/icosahedron)

;; Images
(def image-mobject mobjects/image-mobject)
(def svg-mobject mobjects/svg-mobject)

;; Vector fields
(def vector-field mobjects/vector-field)
(def arrow-vector-field mobjects/arrow-vector-field)
(def stream-lines mobjects/stream-lines)

;; Mobject methods
(def set-color mobjects/set-color)
(def set-fill mobjects/set-fill)
(def set-stroke mobjects/set-stroke)
(def move-to mobjects/move-to)
(def shift mobjects/shift)
(def scale mobjects/scale)
(def rotate mobjects/rotate)
(def flip mobjects/flip)
(def next-to mobjects/next-to)
(def align-to mobjects/align-to)
(def to-edge mobjects/to-edge)
(def to-corner mobjects/to-corner)
(def center mobjects/center)
(def get-center mobjects/get-center)
(def get-width mobjects/get-width)
(def get-height mobjects/get-height)
(def set-width mobjects/set-width)
(def set-height mobjects/set-height)
(def copy mobjects/copy)
(def add-updater mobjects/add-updater)
(def arrange mobjects/arrange)
(def arrange-in-grid mobjects/arrange-in-grid)

;; ============================================================================
;; Re-export Animations
;; ============================================================================

;; Creation
(def create animations/create)
(def uncreate animations/uncreate)
(def draw-border-then-fill animations/draw-border-then-fill)
(def write animations/write)
(def unwrite animations/unwrite)
(def show-increasing-subsets animations/show-increasing-subsets)

;; Fade
(def fade-in animations/fade-in)
(def fade-out animations/fade-out)
(def fade-to-color animations/fade-to-color)
(def fade-transform animations/fade-transform)

;; Transform
(def transform animations/transform)
(def replacement-transform animations/replacement-transform)
(def transform-from-copy animations/transform-from-copy)
(def clockwise-transform animations/clockwise-transform)
(def counterclockwise-transform animations/counterclockwise-transform)
(def transform-matching-shapes animations/transform-matching-shapes)
(def transform-matching-tex animations/transform-matching-tex)

;; Movement
(def move-to-target animations/move-to-target)
(def move-along-path animations/move-along-path)
(def rotate-anim animations/rotate-anim)
(def rotating animations/rotating)

;; Scaling
(def scale-in-place animations/scale-in-place)
(def shrink-to-center animations/shrink-to-center)
(def grow-from-center animations/grow-from-center)
(def grow-from-point animations/grow-from-point)
(def grow-from-edge animations/grow-from-edge)
(def grow-arrow animations/grow-arrow)
(def spin-in-from-nothing animations/spin-in-from-nothing)
(def spiral-in animations/spiral-in)

;; Indication
(def indicate animations/indicate)
(def circumscribe animations/circumscribe)
(def flash animations/flash)
(def focus-on animations/focus-on)
(def wiggle animations/wiggle)
(def show-passing-flash animations/show-passing-flash)
(def apply-wave animations/apply-wave)

;; Text animations
(def add-text-letter-by-letter animations/add-text-letter-by-letter)
(def add-text-word-by-word animations/add-text-word-by-word)
(def type-with-cursor animations/type-with-cursor)

;; Number animations
(def change-decimal-to-value animations/change-decimal-to-value)
(def changing-decimal animations/changing-decimal)

;; Function animations
(def apply-function animations/apply-function)
(def apply-complex-function animations/apply-complex-function)
(def apply-matrix animations/apply-matrix)
(def homotopy animations/homotopy)

;; Updater animations
(def update-from-func animations/update-from-func)
(def update-from-alpha-func animations/update-from-alpha-func)
(def restore animations/restore)

;; Composition
(def animation-group animations/animation-group)
(def lagged-start animations/lagged-start)
(def lagged-start-map animations/lagged-start-map)
(def succession animations/succession)
(def wait-anim animations/wait-anim)

;; Helpers
(def generate-target! animations/generate-target!)
(def save-state! animations/save-state!)
(def set-run-time animations/set-run-time)
(def set-rate-func animations/set-rate-func)

;; ============================================================================
;; Re-export Scenes
;; ============================================================================

(def create-scene scenes/create-scene)
(def create-3d-scene scenes/create-3d-scene)
(def create-moving-camera-scene scenes/create-moving-camera-scene)
(def create-zoomed-scene scenes/create-zoomed-scene)

(def scene-class scenes/scene-class)
(def three-d-scene-class scenes/three-d-scene-class)
(def moving-camera-scene-class scenes/moving-camera-scene-class)

;; Scene methods (with scene as first arg)
(def play! scenes/play!)
(def wait! scenes/wait!)
(def add! scenes/add!)
(def remove! scenes/remove!)
(def clear! scenes/clear!)
(def bring-to-front! scenes/bring-to-front!)
(def bring-to-back! scenes/bring-to-back!)

;; Camera
(def get-camera scenes/get-camera)
(def get-frame scenes/get-frame)
(def move-camera! scenes/move-camera!)
(def set-camera-orientation! scenes/set-camera-orientation!)
(def begin-ambient-camera-rotation! scenes/begin-ambient-camera-rotation!)
(def stop-ambient-camera-rotation! scenes/stop-ambient-camera-rotation!)

;; Rendering
(def render! scenes/render!)
(def quick-render! scenes/quick-render!)
(def preview! scenes/preview!)
(def run! scenes/run!)

;; Builder
(def scene-builder scenes/scene-builder)
(def with-object scenes/with-object)
(def with-animation scenes/with-animation)
(def with-wait scenes/with-wait)
(def build-and-render! scenes/build-and-render!)

;; Macros need special handling - import them
(defmacro defscene [& args] `(scenes/defscene ~@args))
(defmacro def3dscene [& args] `(scenes/def3dscene ~@args))
(defmacro run-scene [& body] `(scenes/run-scene ~@body))
(defmacro run-3d-scene [& body] `(scenes/run-3d-scene ~@body))

;; ============================================================================
;; Re-export Constants (as delays - deref with @)
;; ============================================================================

;; Colors
(def WHITE constants/WHITE)
(def BLACK constants/BLACK)
(def RED constants/RED)
(def RED_A constants/RED_A)
(def RED_B constants/RED_B)
(def RED_C constants/RED_C)
(def RED_D constants/RED_D)
(def RED_E constants/RED_E)
(def BLUE constants/BLUE)
(def BLUE_A constants/BLUE_A)
(def BLUE_B constants/BLUE_B)
(def BLUE_C constants/BLUE_C)
(def BLUE_D constants/BLUE_D)
(def BLUE_E constants/BLUE_E)
(def GREEN constants/GREEN)
(def GREEN_A constants/GREEN_A)
(def GREEN_B constants/GREEN_B)
(def GREEN_C constants/GREEN_C)
(def GREEN_D constants/GREEN_D)
(def GREEN_E constants/GREEN_E)
(def YELLOW constants/YELLOW)
(def GOLD constants/GOLD)
(def PURPLE constants/PURPLE)
(def MAROON constants/MAROON)
(def TEAL constants/TEAL)
(def ORANGE constants/ORANGE)
(def PINK constants/PINK)
(def GRAY constants/GRAY)
(def GREY constants/GREY)

;; Directions
(def UP constants/UP)
(def DOWN constants/DOWN)
(def LEFT constants/LEFT)
(def RIGHT constants/RIGHT)
(def IN constants/IN)
(def OUT constants/OUT)
(def ORIGIN constants/ORIGIN)
(def UL constants/UL)
(def UR constants/UR)
(def DL constants/DL)
(def DR constants/DR)
(def X_AXIS constants/X_AXIS)
(def Y_AXIS constants/Y_AXIS)
(def Z_AXIS constants/Z_AXIS)

;; Math
(def PI constants/PI)
(def TAU constants/TAU)
(def DEGREES constants/DEGREES)

;; Buffers
(def SMALL_BUFF constants/SMALL_BUFF)
(def MED_SMALL_BUFF constants/MED_SMALL_BUFF)
(def MED_LARGE_BUFF constants/MED_LARGE_BUFF)
(def LARGE_BUFF constants/LARGE_BUFF)

;; Utility functions
(def color constants/color)
(def direction constants/direction)
(def degrees->radians constants/degrees->radians)
(def radians->degrees constants/radians->degrees)
(def rgb constants/rgb)
(def rgb255 constants/rgb255)
(def hex-color constants/hex-color)
(def vec-add constants/vec-add)
(def vec-sub constants/vec-sub)
(def vec-scale constants/vec-scale)
(def vec-normalize constants/vec-normalize)
(def rotate-vector constants/rotate-vector)

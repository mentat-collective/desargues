(ns desargues.player.frame
  "CORE stratum of the player: the Frame value object.

   A Frame is what any renderer draws at one instant. It is plain data, so a
   JavaFX canvas, a GTK or wgpu surface reached through cljrs, or a browser
   canvas reached through cljw/cljs can all consume it. Host-free .cljc: no
   interop, no reader conditionals.

   A Frame is {:t seconds, :shapes [shape ...]} in draw order. A shape is the
   scene-graph node (desargues.scene.data) it came from, resolved at t:
   {:id :node :at [x y] :opacity 0..1 :draw 0..1 ...per-kind fields}, where
   :node is the kind (:dot, :line ...). Coordinates
   are scene units (Manim's frame, y up); mapping to pixels is the viewport's
   job, not the frame's.")

(def default-run-time
  "Seconds an animation lasts when it carries no :run-time (Manim's default)."
  1.0)

(def shape-kinds
  "The node kinds a Frame can carry. Closed on purpose: a renderer must know
   how to draw every member. :svg-path carries an SVG path `:d` string in
   scene units (y up), so sources that think in SVG (alexandria's medium
   scenes) keep their curves exactly."
  #{:text :tex :circle :dot :rectangle :rounded-rectangle :decimal :line :path :svg-path})

(def palette
  "Semantic color keyword -> #RRGGBB, Manim's palette, so a live frame matches
   the rendered video. Renderers parse the hex their own way; unknown keywords
   fall back to :white."
  {:white "#FFFFFF" :black "#000000" :grey "#888888" :gray "#888888"
   :blue "#58C4DD" :teal "#5CD0B3" :green "#83C167" :yellow "#FFFF00"
   :gold "#F0AC5F" :red "#FC6255" :maroon "#C55F73" :purple "#9A72AC"
   :pink "#D147BD" :orange "#FF862F"})

(defn shape
  "A shape from a scene-graph `node`, hidden and undrawn until animated."
  [node]
  (assoc node :opacity 0.0 :draw 0.0))

(defn visible?
  "A shape a renderer should paint at all."
  [s]
  (pos? (double (:opacity s 0.0))))

(defn frame
  "The Frame at time `t` holding `shapes` in draw order."
  [t shapes]
  {:t t :shapes (vec shapes)})

(defn visible-shapes
  "The shapes of `f` a renderer paints, in draw order."
  [f]
  (filter visible? (:shapes f)))

(defn viewport
  "A viewport mapping scene units onto a `w` x `h` pixel surface whose visible
   scene window is `scene-w` x `scene-h` units centred on the origin
   (Manim's default frame is 14.22 x 8)."
  ([w h] (viewport w h (/ 128.0 9.0) 8.0))
  ([w h scene-w scene-h]
   {:w w :h h :scale (min (/ w scene-w) (/ h scene-h))}))

(defn ->px
  "Pixel [px py] of scene point [x y] under viewport `vp`. Scene y points up,
   pixel y points down."
  [{:keys [w h scale]} [x y]]
  [(+ (/ w 2.0) (* scale (double x)))
   (- (/ h 2.0) (* scale (double y)))])

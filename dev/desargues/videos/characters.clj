(ns desargues.videos.characters
  "Pi Creature character system DSL.

   The Pi creature is the iconic mascot of 3Blue1Brown videos.
   This namespace provides:
   1. PiCreature definition with modes and expressions
   2. Character animations (blink, look-at, change-mode)
   3. Speech/thought bubbles
   4. Teacher-student scene setup
   5. Character variants (Randolph, Mortimer, etc.)"
  (:require [clojure.string :as str]
            [desargues.videos.typography :as typ]))

;; =============================================================================
;; Pi Creature Modes (Expressions)
;; =============================================================================

(def modes
  "Available Pi creature expression modes.
   Each mode corresponds to an SVG file in the pi_creature_images folder."
  #{:plain
    :happy
    :sad
    :hooray
    :confused
    :thinking
    :pondering
    :maybe
    :speaking
    :surprised
    :hesitant
    :pleading
    :erm
    :angry
    :sassy
    :shruggie
    :tired
    :sick
    :horrified
    :gracious
    :dance-1
    :dance-2
    :dance-3
    :raise-right-hand
    :raise-left-hand
    :raise-both-hands
    :tease
    :coin-flip-1
    :coin-flip-2
    :concentrating})

;; =============================================================================
;; Pi Creature Colors (Character Variants)
;; =============================================================================

(def character-colors
  "Standard colors for different Pi creature characters."
  {:randolph "#58C4DD" ; Blue (default)
   :mortimer "#8B7355" ; Grey-brown
   :student-1 "#1C758A" ; Blue-D
   :student-2 "#236B8E" ; Blue-E
   :student-3 "#29ABCA" ; Blue-C
   :pink "#D147BD"
   :green "#83C167"
   :grey "#888888"})

;; =============================================================================
;; Pi Creature Record
;; =============================================================================

(defrecord PiCreature [name color mode height flip? position bubble looking-at])

(defn pi-creature
  "Create a Pi creature character.

   Options:
   - :name - character name (for identification)
   - :color - body color (keyword or hex)
   - :mode - expression mode (default :plain)
   - :height - character height (default 3)
   - :flip? - whether to flip horizontally
   - :position - initial position
   - :looking-at - what the character is looking at"
  [& {:keys [name color mode height flip? position looking-at]
      :or {color :randolph mode :plain height 3 flip? false}}]
  (->PiCreature
   name
   (get character-colors color color)
   mode
   height
   flip?
   position
   nil
   looking-at))

;; Named character constructors
(defn randolph
  "Create Randolph (the default blue Pi creature)."
  [& opts]
  (apply pi-creature :name "randolph" :color :randolph opts))

(defn mortimer
  "Create Mortimer (the grey-brown Pi creature, typically the teacher)."
  [& opts]
  (apply pi-creature :name "mortimer" :color :mortimer :flip? true opts))

(defn student
  "Create a student Pi creature."
  [n & opts]
  (let [color-key (keyword (str "student-" n))]
    (apply pi-creature
           :name (str "student-" n)
           :color (get character-colors color-key :randolph)
           opts)))

(defn baby-pi-creature
  "Create a baby Pi creature (smaller with larger eyes)."
  [& opts]
  (apply pi-creature :height 1.5 opts))

;; =============================================================================
;; Character State Changes
;; =============================================================================

(defrecord CharacterChange [creature new-mode look-at])

(defn change-mode
  "Change a Pi creature's mode/expression.

   In Manim: pi_creature.change_mode(\"happy\")"
  [creature new-mode & {:keys [look-at]}]
  (->CharacterChange creature new-mode look-at))

(defn look-at
  "Make a Pi creature look at something.

   In Manim: pi_creature.look_at(target)"
  [creature target]
  {:creature creature
   :look-at target
   :type :look-at})

(defn look
  "Make a Pi creature look in a direction (vector).

   In Manim: pi_creature.look(direction)"
  [creature direction]
  {:creature creature
   :direction direction
   :type :look})

;; Fluent API for character changes
(defn change
  "Fluent API for changing Pi creature state.

   Example:
   (-> randy
       (change :happy)
       (looking-at equation))"
  ([creature new-mode]
   (assoc creature :mode new-mode))
  ([creature new-mode target]
   (-> creature
       (assoc :mode new-mode)
       (assoc :looking-at target))))

(defn looking-at
  "Set what the creature is looking at."
  [creature target]
  (assoc creature :looking-at target))

(defn flip
  "Flip the creature horizontally."
  [creature]
  (update creature :flip? not))

;; =============================================================================
;; Bubbles (Speech and Thought)
;; =============================================================================

(defrecord Bubble [bubble-type content direction creature config])

(defn speech-bubble
  "Create a speech bubble for a Pi creature.

   In Manim: pi_creature.get_bubble(content, bubble_type=SpeechBubble)"
  [creature content & {:keys [direction config]
                       :or {direction :left}}]
  (->Bubble :speech content direction creature config))

(defn thought-bubble
  "Create a thought bubble for a Pi creature.

   In Manim: pi_creature.get_bubble(content, bubble_type=ThoughtBubble)"
  [creature content & {:keys [direction config]
                       :or {direction :left}}]
  (->Bubble :thought content direction creature config))

;; Convenience methods matching 3b1b API
(defn says
  "Make a Pi creature say something.

   In Manim: pi_creature.says(content)"
  [creature content & {:keys [mode look-at]
                       :or {mode :speaking}
                       :as opts}]
  {:type :says
   :creature creature
   :content content
   :mode mode
   :look-at look-at
   :bubble (apply speech-bubble creature content (mapcat identity opts))})

(defn thinks
  "Make a Pi creature think something.

   In Manim: pi_creature.thinks(content)"
  [creature content & {:keys [mode look-at]
                       :or {mode :thinking}
                       :as opts}]
  {:type :thinks
   :creature creature
   :content content
   :mode mode
   :look-at look-at
   :bubble (apply thought-bubble creature content (mapcat identity opts))})

(defn debubble
  "Remove a Pi creature's bubble.

   In Manim: pi_creature.debubble()"
  [creature & {:keys [mode look-at]
               :or {mode :plain}}]
  {:type :debubble
   :creature creature
   :mode mode
   :look-at look-at})

;; =============================================================================
;; Character Animations
;; =============================================================================

(defrecord CharacterAnimation [animation-type creature options])

(defn blink
  "Make a Pi creature blink.

   In Manim: Blink(pi_creature)"
  [creature & {:as opts}]
  (->CharacterAnimation :blink creature opts))

(defn pi-creature-bubble-introduction
  "Animate introducing a bubble.

   In Manim: PiCreatureBubbleIntroduction(...)"
  [creature content & {:keys [target-mode look-at bubble-type
                              bubble-direction bubble-config]
                       :or {target-mode :speaking bubble-type :speech}
                       :as opts}]
  (->CharacterAnimation :bubble-introduction creature
                        (assoc opts
                               :content content
                               :target-mode target-mode
                               :bubble-type bubble-type)))

(defn remove-bubble
  "Animate removing a bubble.

   In Manim: RemovePiCreatureBubble(...)"
  [creature & {:keys [target-mode look-at]
               :or {target-mode :plain}
               :as opts}]
  (->CharacterAnimation :remove-bubble creature opts))

;; =============================================================================
;; Pi Creature Scene Patterns
;; =============================================================================

(defrecord PiCreatureScene [creatures primary-creature config])

(defn pi-creature-scene
  "Create a Pi creature scene setup.

   Options:
   - :creatures - list of Pi creatures (default: single Randolph)
   - :start-on-screen? - whether creatures start visible
   - :seconds-to-blink - automatic blink interval"
  [& {:keys [creatures start-on-screen? seconds-to-blink]
      :or {start-on-screen? true seconds-to-blink 3}}]
  (let [cs (or creatures [(randolph :position :dl)])]
    (->PiCreatureScene cs (first cs)
                       {:start-on-screen? start-on-screen?
                        :seconds-to-blink seconds-to-blink})))

;; =============================================================================
;; Teacher-Students Scene
;; =============================================================================

(defrecord TeacherStudentsScene [teacher students screen background-color config])

(defn teacher-students-scene
  "Create a classic 3b1b teacher-students scene.

   This is the iconic setup with Mortimer (teacher) on the right
   and three Randolph variants (students) on the left.

   Options:
   - :student-colors - colors for students
   - :teacher-color - color for teacher
   - :background-color - scene background
   - :screen-height - height of the screen rectangle
   - :student-scale - scale factor for students"
  [& {:keys [student-colors teacher-color background-color
             screen-height student-scale]
      :or {student-colors [:blue-d :blue-e :blue-c]
           teacher-color :mortimer
           background-color :grey-e
           screen-height 4
           student-scale 0.8}}]
  (let [students (mapv (fn [i color]
                         (student i :color color
                                  :position [:bottom :left]))
                       (range 1 4)
                       student-colors)
        teacher (mortimer :position [:bottom :right])]
    (->TeacherStudentsScene
     teacher
     students
     {:height screen-height :position [:top :left]}
     background-color
     {:student-scale student-scale})))

;; Teacher-Students scene helper functions
(defn teacher-says
  "Make the teacher say something."
  [scene content & opts]
  (apply says (:teacher scene) content opts))

(defn teacher-thinks
  "Make the teacher think something."
  [scene content & opts]
  (apply thinks (:teacher scene) content opts))

(defn student-says
  "Make a student say something.

   Options:
   - :index - which student (0, 1, or 2, default 2)"
  [scene content & {:keys [index target-mode bubble-direction]
                    :or {index 2 bubble-direction :left}}]
  (let [student (nth (:students scene) index)]
    (says student content
          :mode (or target-mode
                    (rand-nth [:raise-right-hand :raise-left-hand])))))

(defn student-thinks
  "Make a student think something."
  [scene content & {:keys [index target-mode]
                    :or {index 2}}]
  (let [student (nth (:students scene) index)]
    (thinks student content :mode target-mode)))

(defn change-students
  "Change all students' modes.

   In Manim: self.play_student_changes(*modes)"
  [scene & modes]
  {:type :change-students
   :students (:students scene)
   :modes modes})

(defn play-all-student-changes
  "Change all students to the same mode."
  [scene mode]
  (apply change-students scene (repeat (count (:students scene)) mode)))

(defn teacher-holds-up
  "Animate teacher holding up a mobject.

   In Manim: self.teacher_holds_up(mobject)"
  [scene mobject & {:keys [target-mode]
                    :or {target-mode :raise-right-hand}}]
  {:type :teacher-holds-up
   :teacher (:teacher scene)
   :mobject mobject
   :target-mode target-mode})

;; =============================================================================
;; Eye Contact and Looking
;; =============================================================================

(defn make-eye-contact
  "Make two Pi creatures look at each other.

   In Manim: pi1.make_eye_contact(pi2)"
  [creature1 creature2]
  {:type :eye-contact
   :creature1 creature1
   :creature2 creature2})

(defn all-look-at
  "Make all creatures in a scene look at something."
  [scene target]
  {:type :all-look-at
   :creatures (concat (:students scene) [(:teacher scene)])
   :target target})

;; =============================================================================
;; Compile to Manim Python
;; =============================================================================

(defn- compile-position [pos]
  (cond
    (keyword? pos) (str/upper-case (name pos))
    (vector? pos) (str/join " + " (map #(str/upper-case (name %)) pos))
    :else (str pos)))

(defmulti compile-character
  "Compile character DSL to Manim Python code."
  (fn [expr] (cond
               (instance? PiCreature expr) :pi-creature
               (instance? CharacterChange expr) :char-change
               (instance? Bubble expr) :bubble
               (instance? CharacterAnimation expr) :char-animation
               (instance? TeacherStudentsScene expr) :teacher-students
               (map? expr) (:type expr)
               :else :default)))

(defmethod compile-character :pi-creature
  [{:keys [name color mode height flip? position]}]
  (let [class-name (cond
                     (= name "mortimer") "Mortimer"
                     (str/starts-with? (or name "") "student") "Randolph"
                     :else "PiCreature")]
    (str class-name "("
         "mode=\"" (clojure.core/name mode) "\", "
         "color=\"" color "\", "
         "height=" height
         (when flip? ", flip_at_start=True")
         ")"
         (when position
           (str ".to_corner(" (compile-position position) ")")))))

(defmethod compile-character :char-change
  [{:keys [creature new-mode look-at]}]
  (str (compile-character creature) ".change(\""
       (clojure.core/name new-mode) "\""
       (when look-at (str ", " look-at))
       ")"))

(defmethod compile-character :bubble
  [{:keys [bubble-type content direction creature]}]
  (let [bubble-class (if (= bubble-type :speech)
                       "SpeechBubble"
                       "ThoughtBubble")]
    (str (compile-character creature)
         ".get_bubble(\"" content "\", bubble_type=" bubble-class ")")))

(defmethod compile-character :char-animation
  [{:keys [animation-type creature options]}]
  (case animation-type
    :blink (str "Blink(" (compile-character creature) ")")
    :bubble-introduction
    (str "PiCreatureBubbleIntroduction("
         (compile-character creature) ", "
         "\"" (:content options) "\", "
         "target_mode=\"" (name (:target-mode options)) "\""
         ")")
    :remove-bubble
    (str "RemovePiCreatureBubble("
         (compile-character creature)
         ")")))

(defmethod compile-character :default [expr]
  (str expr))

;; =============================================================================
;; Usage Examples
;; =============================================================================

(comment
  ;; Example 1: Create basic Pi creature
  (randolph :mode :happy)

  ;; Example 2: Create teacher-students scene
  (def scene (teacher-students-scene))

  ;; Example 3: Teacher says something
  (teacher-says scene "Let me show you...")

  ;; Example 4: Student raises hand and asks
  (student-says scene "But what about...?" :index 1)

  ;; Example 5: Character state changes
  (-> (randolph)
      (change :thinking)
      (looking-at 'equation))

  ;; Example 6: Blink animation
  (blink (randolph))

  ;; Example 7: Make creatures look at each other
  (make-eye-contact (randolph) (mortimer))

  ;; Example 8: Full scene with animations
  (let [scene (teacher-students-scene)
        eq (typ/tex "E = mc^2")]
    [(teacher-holds-up scene eq)
     (change-students scene :surprised :confused :thinking)
     (teacher-says scene "This is Einstein's famous equation")
     (student-says scene "Can you explain it?" :index 0)]))

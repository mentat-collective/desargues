# Brachistochrone Problem Animation

This implementation solves and visualizes the classic **Brachistochrone Problem**: Given two points A and B in a vertical plane, what is the curve traced out by a point acted on only by gravity, which starts at A and reaches B in the shortest time?

## What It Does

The animation demonstrates:
1. **Three candidate curves** from point A to point B:
   - Straight line (BLUE)
   - Parabolic path (YELLOW)
   - Cycloid - the brachistochrone solution (GREEN)

2. **Physics calculations** using Emmy (symbolic mathematics):
   - Lagrangian mechanics: L = T - V (kinetic - potential energy)
   - Action integral: S = ∫ L dt
   - Descent times for each path
   - Comparison of action values

3. **Visual animations** using Manim:
   - Bodies sliding down each curve simultaneously
   - Timing comparisons
   - Action integral values
   - Mathematical equations in LaTeX

## Results

Based on the physics calculations:

| Curve | Descent Time | Action Integral |
|-------|--------------|-----------------|
| Straight | 2.721 s | 40.00 |
| Parabola | 2.277 s | 63.73 |
| **Cycloid** | **2.634 s** | **16.53** ✨ |

**Winner**: The **Cycloid** has the **minimum action** (16.53), even though it's not the fastest time! This demonstrates the **principle of least action** in classical mechanics.

## Implementation

### Technology Stack
- **Clojure** - Main programming language
- **Emmy** - Symbolic mathematics and Lagrangian mechanics
- **Manim Community Edition** - Mathematical animation
- **libpython-clj** - Clojure-Python interoperability

### Architecture
```
Emmy (Clojure)          →  Computes physics
  ↓
Lagrangian & Action     →  Symbolic mathematics
  ↓
LaTeX Generation        →  Mathematical display
  ↓
Manim (Python)          →  Beautiful animations
```

## Files

### Clojure Code
- **`src/desargues/brachistochrone.clj`** - Main implementation
  - Physics calculations (Lagrangian, action, descent time)
  - Curve definitions (straight, parabola, cycloid)
  - LaTeX generation from Emmy expressions
  - Integration with Manim

### Python Scenes
- **`py/brachistochrone_scene.py`** - Manim visualization
  - `BrachistochroneProblem` - Full animation with racing balls
  - `BrachistochroneDerivation` - Mathematical derivation
  - `BrachistochroneWithMath` - Combined physics + math

### Generated Video
- **`media/videos/1080p60/BrachistochroneProblem.mp4`** - Final animation (1080p60)

## Usage

### Running the Animation

```clojure
;; Start a REPL
clojure -A:dev

;; Load the namespace
(require '[desargues.brachistochrone :as brach])

;; Compute physics for the three paths
(brach/compare-paths -4.0 2.0 4.0 -2.0)
;; Returns: {:straight {:time 2.721, :action 40.00}
;;           :parabola {:time 2.277, :action 63.73}
;;           :cycloid {:time 2.634, :action 16.53}}

;; Generate LaTeX for the Lagrangian
(brach/generate-lagrangian-latex)
;; => "- g\\,m\\,y + \\frac{1}{2}\\,m\\,{\\mathsf{vx}}^{2} + \\frac{1}{2}\\,m\\,{\\mathsf{vy}}^{2}"

;; Render the full animation
(brach/render-brachistochrone-scene)
```

### Viewing the Video

```bash
# The video is saved to:
media/videos/1080p60/BrachistochroneProblem.mp4

# Play it with any video player:
vlc media/videos/1080p60/BrachistochroneProblem.mp4
# or
mpv media/videos/1080p60/BrachistochroneProblem.mp4
```

## The Physics

### Lagrangian

The Lagrangian for a particle under gravity:

```
L = T - V = (1/2)m(vx² + vy²) - mgy
```

where:
- T = kinetic energy
- V = potential energy
- m = mass
- g = gravitational acceleration (9.8 m/s²)
- y = vertical position

### Action Integral

The action is the time integral of the Lagrangian:

```
S = ∫ L dt
```

According to **Hamilton's principle of least action**, the actual path taken by the system is the one that minimizes (or makes stationary) the action.

### Brachistochrone Functional

For the brachistochrone problem, we minimize the descent time:

```
T = ∫ √[(1 + y'²)/(2gy)] dx
```

Using the **Euler-Lagrange equation** from calculus of variations, the solution is a **cycloid**:

```
x = r(θ - sin θ)
y = r(1 - cos θ)
```

## Mathematics in the Animation

The animation shows:

1. **Lagrangian equation** - Generated from Emmy's symbolic computation
2. **Action values** - Computed by numerical integration
3. **Euler-Lagrange equation** - Fundamental equation of calculus of variations
4. **Cycloid parametric equations** - The optimal solution

All mathematical expressions are rendered in LaTeX for clarity.

## Extensions

You can extend this implementation to:

1. **Different starting/ending points** - Modify the coordinates in `compare-paths`
2. **Additional curves** - Add more candidate paths in the Python scene
3. **Other variational problems** - Use the same Emmy + Manim framework for:
   - Geodesics (shortest path on curved surfaces)
   - Fermat's principle (light rays)
   - Soap films (minimal surfaces)

## References

- **Classical Mechanics** - Lagrangian formulation, calculus of variations
- **Emmy Documentation** - https://github.com/mentat-collective/emmy
- **Manim Community** - https://www.manim.community/
- **Brachistochrone Problem** - https://en.wikipedia.org/wiki/Brachistochrone_curve

## Credits

Implementation by Claude Code using:
- Emmy for symbolic mathematics
- Manim for animation
- Clojure for elegant functional programming

The brachistochrone problem was first solved by Johann Bernoulli in 1696, with contributions from Newton, Leibniz, and others.

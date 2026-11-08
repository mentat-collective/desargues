(ns desargues.steps.protocols
  "PORTS of the step solver: the two seams everything else plugs into.

   IRule is one way of making progress on a goal. Given a goal (an Emmy
   expression to integrate or differentiate, with its variable), `moves`
   returns every move the rule licenses as [step goal'] pairs: an empty seq
   when it does not apply, several when it can apply in more than one way
   (two candidate substitutions). A move whose goal' is nil closes the goal.
   Rules are values, so they compose: desargues.steps.tactics builds
   sequences, choices and repetitions out of rules, and every combinator is
   itself an IRule (Composite).

   INumeric is the numeric backend: definite quadrature, Riemann sums and
   sampling of a function on an interval. A backend is chosen by key
   (desargues.steps.numeric/backend): :jvm is built in, :raster runs on the
   raster backend desargues already uses for trajectories. The solver and the
   notebooks only ever see this protocol (DIP).

   Host-free .cljc.")

(defprotocol IRule
  (rule-id [rule] "Keyword naming the rule, e.g. :integral/by-parts.")
  (moves [rule goal] "Every move this rule licenses on `goal`: a seq of [step goal'] (goal' nil when solved)."))

(defprotocol INumeric
  (integrate [backend f a b] "The definite integral of f over [a b].")
  (riemann [backend f a b n method] "The Riemann sum of f over [a b] with n cells; method is :left :right :mid or :trapezoid.")
  (sample [backend f a b n] "n + 1 points [x (f x)] evenly spaced over [a b]."))

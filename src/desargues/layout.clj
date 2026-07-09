(ns desargues.layout
  "Single-require facade for the layout DSL (mirrors `desargues.manim.all`).

   `(require '[desargues.layout :as ly])` then use ly/row, ly/text, ly/realize …
   Pulls in the manim backend (`realize`), so unlike `desargues.layout.core`
   this namespace is NOT python-free. For pure use, require the core directly."
  (:require [desargues.layout.core :as core]
            [desargues.layout.realize :as realize]))

;; constructors (pure) --------------------------------------------------------
(def el     core/el)
(def box    core/box)
(def row    core/row)
(def column core/column)
(def col    core/col)
(def spacer core/spacer)
(def text   core/text)
(def math   core/math)
(def image  core/image)

;; pure resolver --------------------------------------------------------------
(def resolve-layout core/resolve-layout)

;; realization (manim backend) -----------------------------------------------
(def realize realize/realize)
(def by-id   realize/by-id)

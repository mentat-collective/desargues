(ns desargues.manim.mobject-cache-protocols
  "The mobject-reuse ports. Pure, no Python. Adapters live in
   desargues.manim.mobject-cache.

   IMobjectCache: built backend objects kept under a key that is a value
   describing them, handed out as independent copies.

   IKeptStore: where the kept builds live and how copies are made. A store
   owns its objects: a Manim mobject must be held by Python itself, because
   a libpython-clj handle made inside a Python callback (a Scene.construct)
   is released when the callback returns.")

(defprotocol IMobjectCache
  (-active? [cache]
    "False when the cache never keeps anything, so a caller may skip
     computing keys.")
  (-fetch! [cache key build]
    "The object for `key` as {:mobject m :meta x :hit? bool}. On a hit, m is
     a fresh copy of the kept build. On a miss, (build) is called; it returns
     {:mobject m :meta x}, a copy of m is kept with x and m itself is
     returned. Every m handed out is independent of the kept build and of
     every other m.")
  (-meta [cache key]
    "The meta kept with `key`'s build, or nil. Never builds or copies.")
  (-stats [cache]
    "{:hits :misses :size :max} since the last -clear!.")
  (-clear! [cache]
    "Drop every kept build and reset the counters."))

(defprotocol IKeptStore
  (-keep! [store slot obj]
    "Keep an independent copy of `obj` under the integer `slot`.")
  (-copy-of [store slot]
    "A fresh independent copy of the object kept under `slot`.")
  (-drop! [store slot]
    "Forget the object kept under `slot`.")
  (-drop-all! [store]
    "Forget every kept object."))

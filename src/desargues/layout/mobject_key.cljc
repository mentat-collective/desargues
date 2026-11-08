(ns desargues.layout.mobject-key
  "Pure keys under which a realized layout node's built mobject can be
   reused (desargues.manim.mobject-cache-protocols/IMobjectCache).

   A key is a value: everything the build reads, as data. Two equal keys
   build equal mobjects, so a kept build may stand in for a new one.

   - leaf-key: a leaf's natural-size mobject (pass 1 measure): its type,
     content and effective style, plus the backend stamp.
   - subtree-key: a node's placed VGroup (pass 3): the PLACED node (every
     descendant with its box, attrs and content), the node's effective style
     (descendant styles derive from it and their own attrs), the frame and
     margin, plus the backend stamp.

   The stamp is the backend's build-relevant settings (pixel size, renderer,
   TeX template ...), opaque here. A node is not keyed (nil) when its build
   reads something outside the description: an image file, or an :id whose
   registry entry must point at the built object itself."
  (:require [malli.core :as m]))

(def LeafNode
  (m/schema [:map [:type keyword?] [:content {:optional true} any?]]))

(defn- nodes [node]
  (tree-seq (comp seq :children) :children node))

(defn- outside-description? [node]
  (or (= :image (:type node))
      (some? (get-in node [:attrs :id]))))

(defn leaf-key
  "Key of a leaf's natural-size build, or nil when it is not keyable."
  [node style stamp]
  (when-not (outside-description? node)
    [::leaf (:type node) (:content node) (or style {}) stamp]))

(defn subtree-keyable?
  "Can `node`'s placed build be reused? False when any node in its subtree
   carries an :id or is an image."
  [node]
  (not-any? outside-description? (nodes node)))

(defn subtree-key
  "Key of a placed node's VGroup, or nil when it is not keyable. `frame` is
   [frame-width frame-height margin]."
  [placed-node style frame stamp]
  (when (subtree-keyable? placed-node)
    [::subtree placed-node (or style {}) frame stamp]))

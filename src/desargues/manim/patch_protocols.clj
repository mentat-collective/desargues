(ns desargues.manim.patch-protocols
  "The Manim-patch port: a reversible change to Manim's Python classes. Pure,
   no Python. Adapters live in desargues.manim.patches.")

(defprotocol IManimPatch
  (-patch-id [patch]
    "Keyword naming the patch, e.g. :svg-path-shared-copy.")
  (-install! [patch]
    "Apply the patch. Idempotent. Returns true when it is in place.")
  (-uninstall! [patch]
    "Restore Manim's original behaviour. Idempotent. Returns true when a patch was removed.")
  (-installed? [patch]
    "Is the patch in place right now?"))

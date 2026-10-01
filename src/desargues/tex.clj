(ns desargues.tex
  "TeX labels in live scenes: a manifest cache of typeset SVGs and the swap of
   label nodes for :image nodes.

   A manifest maps [latex hex-colour] to {:file href :w pt :h pt}; it lives as
   manifest.edn inside an assets directory, next to the SVGs it names (file
   names are a content hash of the key).

     plan / texify            pure: what is missing; graph + manifest -> graph
     ensure!                  boundary: typeset what is missing through an
                              ITypesetter, write SVGs and manifest
     read-manifest / write-manifest!   the manifest file

   The label half (placing a label in a scene) is desargues.tex.label; the
   Typesetter port and its adapters are desargues.tex.typeset."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.pprint :as pp]
            [desargues.tex.label :as label]
            [desargues.tex.typeset :as ts]))

(def manifest-name "manifest.edn")

(def design-size-pt
  "The TeX design size labels are typeset at: one em, in points."
  10.0)

(def px-per-unit 60.0)

;; ---------------------------------------------------------------- pure

(defn asset-name
  "The SVG file name of a manifest key: 12 hex digits of its SHA-1, \".svg\"."
  [k]
  (let [md (java.security.MessageDigest/getInstance "SHA-1")
        bs (.digest md (.getBytes (pr-str k) "UTF-8"))]
    (str (subs (apply str (map #(format "%02x" (bit-and % 0xff)) bs)) 0 12) ".svg")))

(defn plan
  "The labels of `graphs` the `manifest` lacks, as sorted [{:key [latex hex]
   :name svg-file-name}]."
  [graphs manifest]
  (->> (label/wanted-keys graphs)
       (remove #(contains? manifest %))
       sort
       (mapv (fn [k] {:key k :name (asset-name k)}))))

(defn image-opts
  "The :image width and height in world units of a label typeset at extent
   w x h pt, drawn at font size `size` px: one em is size/60 units and one em
   is the 10 pt design size."
  [size w h]
  (let [em (/ size px-per-unit)]
    {:width (* em (/ w design-size-pt)) :height (* em (/ h design-size-pt))}))

(defn- swap-node [manifest nd]
  (if-let [{:keys [file w h]} (manifest (label/node-key nd))]
    (-> nd
        (dissoc :text)
        (assoc :node :image :content file
               :opts (image-opts (label/node-size nd) w h)))
    (assoc nd :text (label/node-latex nd))))

(defn texify
  "Swap each TeX label node of a recorded scene `graph` for an :image of its
   typeset SVG from `manifest`. A label the manifest lacks stays a text node
   showing its LaTeX source. Returns {:graph graph' :missing #{latex ...}}."
  [graph manifest]
  (let [labels (filter (comp label/tex-node? val) (:nodes graph))]
    {:graph (reduce (fn [g [id nd]] (assoc-in g [:nodes id] (swap-node manifest nd)))
                    graph labels)
     :missing (set (for [[_ nd] labels :when (not (contains? manifest (label/node-key nd)))]
                     (label/node-latex nd)))}))

;; ---------------------------------------------------------------- boundary

(defn read-manifest
  "The manifest in assets directory `dir`, or {} when there is none."
  [dir]
  (let [f (io/file dir manifest-name)]
    (if (.exists f) (edn/read-string (slurp f)) {})))

(defn write-manifest!
  "Write `manifest` as manifest.edn in `dir`, keys sorted. Returns it."
  [dir manifest]
  (io/make-parents (io/file dir manifest-name))
  (spit (io/file dir manifest-name)
        (with-out-str (pp/pprint (into (sorted-map) manifest))))
  manifest)

(defn ensure!
  "Typeset, through ITypesetter `typesetter`, every label of `graphs` that the
   manifest in assets directory `dir` lacks; write each SVG into `dir` and the
   updated manifest. Manifest :file entries are (str href-prefix name), where
   :href-prefix defaults to `dir` with a trailing slash.
   Returns {:manifest m :added [key ...] :failed {key error}}."
  [typesetter dir graphs & {:keys [href-prefix]}]
  (let [prefix (or href-prefix (str dir "/"))
        before (read-manifest dir)
        results (for [{k :key nm :name} (plan graphs before)]
                  (let [[latex hex] k
                        r (ts/typeset typesetter latex hex)]
                    (if (:error r)
                      [:failed k (:error r)]
                      (do (io/make-parents (io/file dir nm))
                          (spit (io/file dir nm) (:svg r))
                          [:added k {:file (str prefix nm) :w (:w r) :h (:h r)}]))))
        results (doall results)
        added (into {} (for [[tag k v] results :when (= tag :added)] [k v]))
        manifest (merge before added)]
    (when (seq added) (write-manifest! dir manifest))
    {:manifest manifest
     :added (vec (keys added))
     :failed (into {} (for [[tag k e] results :when (= tag :failed)] [k e]))}))

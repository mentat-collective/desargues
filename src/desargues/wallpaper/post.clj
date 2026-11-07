(ns desargues.wallpaper.post
  "Post effects of desargues.wallpaper.render: image-space passes applied
   after every layer, in order. An effect is a map dispatched on :type:

     {:type :bloom    :radius 12 :strength 0.9}       glow: the image blurred, added back
     {:type :dof      :focus 0.5 :band 0.12 :radius 10} depth of field: rows farther than
                                                       `band` (fraction of height) from
                                                       `focus` blend into a blur
     {:type :chroma   :shift 3}                       red right, blue left, by `shift` px
     {:type :vignette :strength 0.5}                  darken by 1 - k·r², r = 1 at a corner
     {:type :fade     :level 0.4}                     scale every channel

   A new effect is a defmethod of `effect!`."
  (:import (java.awt.image BufferedImage DataBufferInt)
           (java.util.function IntConsumer)
           (java.util.stream IntStream)))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defn- pixels ^ints [^BufferedImage img]
  (.getData ^DataBufferInt (.getDataBuffer (.getRaster img))))

(defn- par! [^long n f]
  (.forEach (.parallel (IntStream/range 0 (int n)))
            (reify IntConsumer (accept [_ i] (f i)))))

(defn- split
  "Channels of `px` as three float arrays in [0, 255]."
  [^ints px]
  (let [n (alength px)
        r (float-array n) g (float-array n) b (float-array n)]
    (dotimes [i n]
      (let [c (aget px i)]
        (aset r i (float (bit-and (bit-shift-right c 16) 0xFF)))
        (aset g i (float (bit-and (bit-shift-right c 8) 0xFF)))
        (aset b i (float (bit-and c 0xFF)))))
    [r g b]))

(defn- clamp8 ^long [^double v] (if (< v 0.0) 0 (if (> v 255.0) 255 (long v))))

(defn- join! [^ints px ^floats r ^floats g ^floats b]
  (dotimes [i (alength px)]
    (aset px i (unchecked-int (bit-or (bit-shift-left (clamp8 (aget r i)) 16)
                                      (bit-shift-left (clamp8 (aget g i)) 8)
                                      (clamp8 (aget b i)))))))

(defn- box-pass!
  "One box blur of radius `rad` of `src` into `dst`: `lines` lines of `len`
   samples, line l starting at l·`line-stride`, samples `step` apart."
  [^floats src ^floats dst lines len line-stride step rad]
  (let [lines (long lines) len (long len) line-stride (long line-stride)
        step (long step) rad (long rad)
        last-k (dec len)
        inv (float (/ 1.0 (double (inc (* 2 rad)))))]
    (par! lines
          (fn [^long l]
            (let [base (* l line-stride)]
              (loop [j (- rad) acc (float 0.0)]
                (if (<= j rad)
                  (recur (inc j) (+ acc (aget src (+ base (* step (max 0 (min last-k j)))))))
                  (loop [k 0 acc acc]
                    (when (< k len)
                      (aset dst (+ base (* step k)) (* acc inv))
                      (recur (inc k)
                             (+ acc (- (aget src (+ base (* step (min last-k (+ k rad 1)))))
                                       (aget src (+ base (* step (max 0 (- k rad)))))))))))))))))

(defn blur
  "A copy of channel `ch` (w x h, row-major) blurred by three box passes
   per axis of radius `rad`, close to a gaussian."
  ^floats [^floats ch ^long w ^long h ^long rad]
  (if (< rad 1)
    (aclone ch)
    (let [a (aclone ch) b (float-array (alength ch))]
      (dotimes [_ 3]
        (box-pass! a b h w w 1 rad)
        (box-pass! b a w h 1 w rad))
      a)))

(defn px-at
  "Length `l`, given in pixels of a 1080-high frame, in pixels of `img`."
  ^long [^BufferedImage img l]
  (Math/round (* (double l) (/ (.getHeight img) 1080.0))))

(defmulti effect!
  "Apply post effect `e` to BufferedImage `img` (TYPE_INT_RGB) in place."
  (fn [e _img] (:type e)))

(defmethod effect! :default [e _]
  (throw (ex-info (str "Unknown post effect " (pr-str (:type e))) {:effect e})))

(defmethod effect! :bloom [{:keys [radius strength] :or {radius 12 strength 0.8}} ^BufferedImage img]
  (let [w (.getWidth img) h (.getHeight img) px (pixels img)
        k (float strength)
        rad (px-at img radius)
        [r g b] (split px)
        add! (fn [^floats c]
               (let [^floats bl (blur c w h rad)]
                 (dotimes [i (alength c)]
                   (aset c i (+ (aget c i) (* k (aget bl i)))))))]
    (add! r) (add! g) (add! b)
    (join! px r g b)
    img))

(defmethod effect! :dof [{:keys [focus band radius fade] :or {focus 0.5 band 0.12 radius 10 fade 0.3}}
                         ^BufferedImage img]
  (let [w (.getWidth img) h (.getHeight img) px (pixels img)
        [r g b] (split px)
        rad (px-at img radius)
        [br bg bb] (mapv #(blur % w h rad) [r g b])
        focus (double focus) band (double band) fade (double fade)]
    (par! h (fn [^long y]
              (let [d (Math/abs (- (/ (+ y 0.5) h) focus))
                    t (let [s (max 0.0 (min 1.0 (/ (- d band) fade)))] (* s s (- 3.0 (* 2.0 s))))
                    row (* y w)]
                (dotimes [x w]
                  (let [i (+ row x)
                        mix (fn [^floats a ^floats z] (aset a i (float (+ (aget a i) (* t (- (aget z i) (aget a i)))))))]
                    (mix r br) (mix g bg) (mix b bb))))))
    (join! px r g b)
    img))

(defmethod effect! :chroma [{:keys [shift] :or {shift 3}} ^BufferedImage img]
  (let [w (.getWidth img) h (.getHeight img) px (pixels img)
        src (aclone px)
        s (px-at img shift)]
    (par! h (fn [^long y]
              (let [row (* y w)]
                (dotimes [x w]
                  (let [rx (max 0 (min (dec w) (- x s)))
                        bx (max 0 (min (dec w) (+ x s)))
                        r (bit-and (aget src (+ row rx)) 0xFF0000)
                        g (bit-and (aget src (+ row x)) 0x00FF00)
                        b (bit-and (aget src (+ row bx)) 0x0000FF)]
                    (aset px (+ row x) (unchecked-int (bit-or r g b))))))))
    img))

(defn- scale-row! [^ints px ^long row ^long w f]
  (dotimes [x w]
    (let [i (+ row x)
          c (aget px i)
          k (double (f x))
          r (clamp8 (* k (bit-and (bit-shift-right c 16) 0xFF)))
          g (clamp8 (* k (bit-and (bit-shift-right c 8) 0xFF)))
          b (clamp8 (* k (bit-and c 0xFF)))]
      (aset px i (unchecked-int (bit-or (bit-shift-left r 16) (bit-shift-left g 8) b))))))

(defmethod effect! :vignette [{:keys [strength] :or {strength 0.5}} ^BufferedImage img]
  (let [w (.getWidth img) h (.getHeight img) px (pixels img)
        k (double strength) cx (/ w 2.0) cy (/ h 2.0)
        norm (/ 1.0 (+ (* cx cx) (* cy cy)))]
    (par! h (fn [^long y]
              (let [dy (- (+ y 0.5) cy)]
                (scale-row! px (* y w) w
                            (fn [^long x] (let [dx (- (+ x 0.5) cx)]
                                            (max 0.0 (- 1.0 (* k norm (+ (* dx dx) (* dy dy)))))))))))
    img))

(defmethod effect! :fade [{:keys [level] :or {level 1.0}} ^BufferedImage img]
  (let [w (.getWidth img) h (.getHeight img) px (pixels img) k (double level)]
    (par! h (fn [^long y] (scale-row! px (* y w) w (fn [_] k))))
    img))

(defn apply-all!
  "Apply effects `es` to `img` in order; returns `img`."
  [es img]
  (doseq [e es] (effect! e img))
  img)

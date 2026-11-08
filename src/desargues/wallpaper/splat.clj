(ns desargues.wallpaper.splat
  "BOUNDARY stratum: a fast additive splat rasteriser for very many glowing
   points. Each row [x y sigma r g b] (scene units, y up; sigma the gaussian
   spread; r g b the light at the centre of the splat) is added into a float
   buffer, then tone mapped over the image with 1 - (1 - img) e^(-exposure b),
   so overlapping splats saturate smoothly instead of clipping.

   Used by desargues.wallpaper.render as the optional spec layer
   (:splat (fn [u] rows)), after :frame and before :light.
   opts: {:view [w h] :exposure 1.0}."
  (:import (java.awt.image BufferedImage DataBufferInt)))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:private min-sigma-px 0.7)

(defn accumulate!
  "Add `rows` into float buffers `r` `g` `b` (w x h, row-major). `scale` is
   pixels per scene unit. Returns nil."
  [^floats r ^floats g ^floats b w h scale rows]
  (let [w (long w) h (long h) scale (double scale)
        cx (/ (double w) 2.0) cy (/ (double h) 2.0)]
    (doseq [row rows]
      (let [[x y s pr pg pb] row
            sig (max min-sigma-px (* scale (double s)))
            px (+ cx (* scale (double x)))
            py (- cy (* scale (double y)))
            reach (long (Math/ceil (* 3.0 sig)))
            x0 (max 0 (- (long (Math/floor px)) reach))
            x1 (min (dec w) (+ (long (Math/floor px)) reach))
            y0 (max 0 (- (long (Math/floor py)) reach))
            y1 (min (dec h) (+ (long (Math/floor py)) reach))
            ;; peak-normalised gaussian: r g b is the colour at the centre
            norm 1.0
            inv (/ -1.0 (* 2.0 sig sig))
            pr (double pr) pg (double pg) pb (double pb)]
        (loop [yy y0]
          (when (<= yy y1)
            (let [dy (- (+ yy 0.5) py)]
              (loop [xx x0]
                (when (<= xx x1)
                  (let [dx (- (+ xx 0.5) px)
                        k (* norm (Math/exp (* inv (+ (* dx dx) (* dy dy)))))
                        i (+ (* yy w) xx)]
                    (aset r i (float (+ (aget r i) (* k pr))))
                    (aset g i (float (+ (aget g i) (* k pg))))
                    (aset b i (float (+ (aget b i) (* k pb))))
                    (recur (inc xx))))))
            (recur (inc yy))))))))

(defn splat!
  "Splat `rows` over BufferedImage `img` (TYPE_INT_RGB) in place; returns it.
   opts: {:view [w h] (the spec's scene window) :exposure 1.0}."
  [^BufferedImage img rows {:keys [view exposure] :or {exposure 1.0}}]
  (let [w (.getWidth img) h (.getHeight img)
        [vw vh] view
        scale (min (/ (double w) (double vw)) (/ (double h) (double vh)))
        n (* w h)
        r (float-array n) g (float-array n) b (float-array n)
        px ^ints (.getData ^DataBufferInt (.getDataBuffer (.getRaster img)))
        e (double exposure)]
    (accumulate! r g b w h scale rows)
    (dotimes [i n]
      (let [c (aget px i)
            ch (fn ^long [^long shift ^floats buf]
                 (let [base (/ (double (bit-and (bit-shift-right c shift) 0xFF)) 255.0)
                       out (- 1.0 (* (- 1.0 base) (Math/exp (- (* e (aget buf i))))))]
                   (long (Math/round (* 255.0 (max 0.0 (min 1.0 out)))))))]
        (aset px i (unchecked-int (bit-or (bit-shift-left (ch 16 r) 16)
                                          (bit-shift-left (ch 8 g) 8)
                                          (ch 0 b))))))
    img))

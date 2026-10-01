(ns desargues.board.linalg
  "Pure layer of boards: homogeneous linear algebra over Emmy's generics.

   Every function takes numbers or symbolic expressions alike, so one formula
   serves the kernel (symbols, lowered to raster) and a numeric group table
   (doubles). A point in dimension d is a d-vector; its homogeneous form has
   d + 1 entries and a map is a (d+1)x(d+1) matrix acting on it."
  (:require [emmy.env :as e]))

(defn h "The homogeneous form of point p: p with a trailing 1." [p] (conj (vec p) 1))

(defn cross
  "The cross product of homogeneous 3-vectors: the line through two points,
   or the meet of two lines."
  [[a b c] [d f g]]
  [(e/- (e/* b g) (e/* c f)) (e/- (e/* c d) (e/* a g)) (e/- (e/* a f) (e/* b d))])

(defn dot [u v] (reduce e/+ (map e/* u v)))

(defn size "The order of square matrix M." [M] (count M))

(defn mat*
  "The product AB of square matrices of one order."
  [A B]
  (let [n (size A)]
    (vec (for [i (range n)]
           (vec (for [j (range n)]
                  (reduce e/+ (for [k (range n)] (e/* (get-in A [i k]) (get-in B [k j]))))))))))

(defn identity-matrix [n]
  (vec (for [i (range n)] (vec (for [j (range n)] (if (= i j) 1 0))))))

(defn apply-mat
  "Map H applied to point p (a (count H)-1 vector): its image, dehomogenised."
  [H p]
  (let [n (size H)
        hp (h p)
        img (vec (for [i (range n)] (reduce e/+ (for [k (range n)] (e/* (get-in H [i k]) (nth hp k))))))
        w (peek img)]
    (mapv #(e/divide % w) (pop img))))

(defn det
  "The determinant, by cofactor expansion along the first row."
  [M]
  (case (size M)
    1 (get-in M [0 0])
    2 (e/- (e/* (get-in M [0 0]) (get-in M [1 1])) (e/* (get-in M [0 1]) (get-in M [1 0])))
    (reduce e/+
            (for [j (range (size M))
                  :let [minor (mapv (fn [row] (vec (concat (subvec row 0 j) (subvec row (inc j)))))
                                    (subvec (vec M) 1))
                        term (e/* (get-in M [0 j]) (det minor))]]
              (if (even? j) term (e/- 0 term))))))

(defn trace "The trace: the sum of the diagonal." [M]
  (reduce e/+ (map #(get-in M [% %]) (range (size M)))))

(defn sub [p q] (mapv e/- p q))

(defn add [p q] (mapv e/+ p q))

(defn scale [k p] (mapv #(e/* k %) p))

(defn norm [v] (e/sqrt (dot v v)))

(defn lerp "p + s (q - p)." [p q s] (add p (scale s (sub q p))))

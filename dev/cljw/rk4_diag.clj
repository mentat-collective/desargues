;; Localise the cljw/JVM trajectory divergence: print the system, one derivative
;; evaluation, and the first two RK4 states with full precision on each runtime.
;;   cljw -cp src dev/cljw/rk4_diag.clj   |   clojure -M dev/cljw/rk4_diag.clj
(require '[desargues.videos.physics :as phys])

(def p (phys/make-pendulum {:length 3.0 :damping 0.12 :initial-theta 1.1 :top-point [0 2.2 0]}))

(prn :system (into {} p))
(prn :sin-1.1 (Math/sin 1.1))
(prn :derivs (phys/derivatives p (phys/get-state p)))
(prn :dt-halves (/ 0.01 2) (/ 0.01 6))
(prn :merge (merge-with + {:theta 1.1 :omega 0.0} {:theta 0.0 :omega -0.5}))
(prn :step1 (phys/get-state (phys/step phys/rk4 p 0.01)))
(prn :traj (mapv (juxt :time :state) (take 3 (phys/evolve p {:dt 0.01 :duration 0.05}))))
(prn :traj-len (count (phys/evolve p {:dt 0.01 :duration 8.0})))

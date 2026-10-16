(ns desargues.physics.evolution
  "APPLICATION stratum of the physics bounded context: evolve one system or
   a batch through whichever TrajectorySolver the caller names.

   Validates the time span, resolves :solver through the open `solver` port
   and probes the optional BatchTrajectorySolver capability; it never learns
   which engine it has (LSP).

   Requires desargues.physics.{protocols,stepper,integrators,solver}."
  (:require [desargues.physics.integrators :as integrators]
            [desargues.physics.protocols :as p]
            [desargues.physics.solver :as solver]
            [desargues.physics.stepper :as stepper]))

(defn check-evolve-opts!
  "Reject a :dt that is not finite positive or a :duration that is not finite
   non-negative (a NaN step never reaches the duration and would spin forever)."
  [dt duration]
  (when-not (and (number? dt) (Double/isFinite (double dt)) (pos? dt))
    (throw (ex-info "evolve: :dt must be a finite positive number" {:dt dt})))
  (when-not (and (number? duration) (Double/isFinite (double duration)) (not (neg? duration)))
    (throw (ex-info "evolve: :duration must be a finite non-negative number" {:duration duration}))))

(defn evolve
  "Evolve a physical system over time.

   Arguments:
   - system: Any PhysicalSystem
   - opts: Map with :dt (time step), :duration (total time), and either
     :integrator (a step-based Integrator, wrapped in a StepperSolver; default
     rk4) or :solver, which takes precedence: a TrajectorySolver, a backend
     key (:jvm, :raster) or [backend-key opts], resolved through `solver`.

   :dt must be a finite positive number and :duration a finite non-negative
   number; anything else (NaN in particular) is rejected up front, because a
   NaN step never reaches the duration and would spin forever.

   Returns a sequence of {:time t :state state} maps."
  [system {:keys [dt duration integrator solver]
           :or {dt 0.01 duration 10.0 integrator integrators/rk4}}]
  (check-evolve-opts! dt duration)
  (p/solve-trajectory (or (solver/->solver solver) (stepper/stepper-solver integrator))
                      system
                      {:dt dt :duration duration}))

(defn evolve-batch
  "evolve for every system in `systems` with the same opts (see evolve).
   A :solver that implements BatchTrajectorySolver gets them in one call
   (the raster kernel solver integrates the whole batch inside one kernel);
   any other solver is mapped. Returns a vector of trajectories."
  [systems {:keys [dt duration solver] :or {dt 0.01 duration 10.0} :as opts}]
  (check-evolve-opts! dt duration)
  (let [systems (vec systems)
        solver (solver/->solver solver)
        opts (cond-> opts solver (assoc :solver solver))]
    (if (and solver (satisfies? p/BatchTrajectorySolver solver))
      (p/solve-batch solver systems {:dt dt :duration duration})
      (mapv #(evolve % opts) systems))))

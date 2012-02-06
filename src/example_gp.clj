(ns example_gp
  (:use
    simulator
    [utils :only (pd
                   ave
                   unless-false)]))

(def terminals
  '[0
    1
    (case *good*
      :water 15
      :food 10
      5)
    *good-holding*
    (unless-false (-> *information* :water :pogs) 0)
    (*holdings* :currency)
    (unless-false (-> *relative-values* :silverware) 0)
    (unless-false (*good-info-fn* :water) 0)
    (unless-false (*good-rel-val-fn* :food) 0)
    (ave (map *holdings* *all-goods*))])

(def functions
  '[+ - * pd])

(defn random-DVE
  "Generates a random DVE, as tree of given depth (or 0 depth if no argument passed). Right now, all it does is simply arithmetic between highly constrained terminals."
  ([]
    (random-DVE 0))
  ([depth]
    (list
      (rand-nth functions)
      (rand-nth terminals)
      (if (= depth 0)
        (rand-nth terminals)
        (random-DVE (dec depth))))))

;Here is the setup, where you can tell the program how to generate DVEs for new fleas that are born, by passing to 'setup' an appropriate expression paired with :generate-DVE_expr. Given what I've written here, the conditions say: if the age of any flea is greater than 0, half the time, give the new flea a cloned DVE from one of the existing DVEs. Otherwise, if not (or the other half the time) generate a random DVE.
(setup
  :generate-DVE_expr '(if (and (< (rand) 0.5)
                               (some #(> (- *world-time* (:date-of-birth %))
                                         0)
                                     *fleas-list*))
                        (rand-nth *all-determine-value-exprs*)
                        (example_gp/random-DVE (rand-int 10))))

;Here is the run, where you can change the size of the population, and the duration of the run. In the future, I would like to change the possible semantics here, so that you could also pass a condition based upon which the run should end (instead of just a constant).
(run :size 30
     :duration 100)

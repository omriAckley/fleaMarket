(ns example
  (:use clans
        core
        fleas
        markets
        trading
        utils
        belt.collections
        belt.general-utils
        belt.hash-maps
        belt.jacked-def
        belt.math))

(def holding-based
  (clan :size 100
        :generate-DVE_expr ''(pd 100 *good-holding*)))

(def evolving-exchange-table
  (clan :size 100
        :generate-DVE_expr '(list '*good*
                                  (if (empty? *accessible-fleas*)
                                    {:food (rand-int 100)
                                     :water (rand-int 100)
                                     :pogs (rand-int 100)
                                     :novelty-clocks (rand-int 100)
                                     :silverware (rand-int 100)
                                     :currency (rand-int 100)}
                                    (let [[parent1
                                           parent2] [(-> *accessible-fleas*
                                                         rand-nth
                                                         :determine-value_expr
                                                         second)
                                                     (-> *accessible-fleas*
                                                         rand-nth
                                                         :determine-value_expr
                                                         second)]
                                          crossover-point (dec (rand-int 7))]
                                      (update-all (into {}
                                                        (concat (take crossover-point parent1)
                                                                (drop crossover-point parent2)))
                                                  #(if (> 0.2 (rand))
                                                     (+ %
                                                        (- (rand-int 21)
                                                           10))
                                                     %)))))))

(def random
  (clan :size 100
        :generate-DVE_expr ''(rand-int 100)))

(def food-hording
  (clan :size 100
        :generate-DVE_expr '(list '*good*
                                  {:food 1000
                                   :water 50
                                   :pogs 1
                                   :novelty-clocks 1
                                   :silverware 1
                                   :currency 1})))

(def market1
  (market :clans [holding-based
                  evolving-exchange-table
                  random
                  food-hording]))

(run-simulation 10 market1
                "C:/Users/Omnomnomri/Desktop/FleaMarketExperimentsData/001.txt" )
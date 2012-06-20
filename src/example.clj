(ns example
  (:use clans
        core
        fleas
        markets
        trading
        utils
        belt.collections
        belt.combinations
        belt.general-utils
        belt.hash-maps
        belt.jacked-def
        belt.math
        belt.probability))

(def clan1 (clan :size 10 :generate-DVE_expr ''(pd 100 *good-holding*)))

(def clan2 (clan :size 20 :generate-DVE_expr ''(pd 100 *good-holding*)))

(def clan3 (clan :size 30 :generate-DVE_expr ''(pd 100 *good-holding*)))

(def market1 (market :clans [clan1 clan2 clan3]))

(run-simulation 100 market1)
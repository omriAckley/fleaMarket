(ns gp
  (:use
    simulator
    [utils :only (pd)]))

(setup)

(run :size 100
     :duration 100)

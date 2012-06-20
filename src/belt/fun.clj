(ns belt.fun)

(declare fibs)

(def fibs
  (cons 1 (reductions + 1 fibs)))

(def primes
  (filter (fn [n] (not-any? zero? (map #(mod n %) primes)))
          (iterate inc 2)))
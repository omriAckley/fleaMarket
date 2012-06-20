(ns math
  (:use [belt.general-utils :only [<?
                              unless]]))

(defn +-
  "Returns a range from (- n size) to (+ n size), inclusive."
  ([n size]
    (range (- n size) (+ n size 1)))
  ([n]
    (range (- n) (inc n))))

(defn +|-
  "Returns a vector of the result of (+ x y) and (- x y). Given more arguments, makes more and more lists."
  ([x]
    ((juxt + -) x))
  ([x y]
    ((juxt + -) x y))
  ([x y & more]
    (map #(apply +|- % more) (+|- x y))))

(defn abs-diff
  "Returns the absolute difference of args."
  [& args]
  (Math/abs (apply - args)))

(defn abs-ratio
  "Returns the \"absolute\" ratio of n/m--where aboslute is multiplicative distance from 1. That is, if n/m is less than 1, returns m/n."
  [n m]
  (unless (<? 1)
          (/ n m)
          (/ m n)))

(defn ave
  "Returns the average of the collection of numbers passed to it."
  [coll]
  (/ (reduce + coll) (count coll)))

(defn geometric-mean
  "Returns the geometric mean of the collection of numbers passed to it."
  [coll]
  (if (= 2 (count coll))
    (Math/sqrt (apply *' coll))
    (let [exponent (/ 1 (count coll))]
      (reduce #(*' %1 (Math/pow %2 exponent))
              (cons 1 coll)))))

(defn inverse
  "Returns 1 divided by n."
  [n]
  (/ 1 n))

(def ^{:doc "Casts into float after rounding."}
  my-round
  (comp #(Math/round %) float))

(defn pd
  "Same as division, but any errors (i.e. divide by zero) are caught, in which case 0 is returned."
  [& nums]
  (try (apply / nums) (catch Exception e 0)))

(defn sum
  "Returns the sum of the numbers passed to it."
  [coll]
  (reduce + coll))

(ns belt.probability
  (:use [belt.hash-maps :only [update-all]]
        [belt.math :only [+-
                     sum]]))

(defn rand-expand
  "Will expand n by +-1, then with probability prob will select rand-nth from this list. If not, will continue to expand (by +-2, then +-3, etc.) until it successfully selects something."
  [n prob]
  (loop [width 1]
    (if (> (rand) prob)
      (recur (inc width))
      (rand-nth (+- n width)))))

(defn rand-inc
  "Given a starting n, and an increment probabilty p, will return n with probability 1-p. Otherwise it will recur with rand-inc of (inc n) and p."
  [n p]
  (if (> (rand) p)
    n
    (recur (inc n) p)))

(defn rand-populate
  "Argumetns passed should be in the form of any number of [n obj] collections. Returns a shuffled list of all objs (either functions or otherwise) repeated n times."
  [& reps-objs]
  (shuffle 
    (mapcat (fn [[n obj]] ((if (fn? obj) repeatedly repeat) n obj)) reps-objs)))

(defn rand-ratio
  "Returns a random ratio of (/ (inc (rand-int n)) (inc (rand-int m))) or (/ (inc (rand-int m)) (inc (rand-int m))) with equal probability between those two options, where m defaults to 1 if not given."
  ([n]
    (rand-ratio n 1))
  ([n m]
    (let [[num
           denom] (shuffle [(inc (rand-int n))
                            (inc (rand-int m))])]
    (/ num denom))))

(defn weighted-select
  "Given a map of [val prob] being [key value], will return any given val with probability prob. If all of the probs do not add up to exactly 1, throws an exception."
  [m]
  (if-not (= 1 (->> m vals (map rationalize) sum))
    (throw (Exception. "Probabilities do not sum to 1."))
    (loop [r (rand)
           _m m]
      (let [[ret prob] (first _m)]
        (if (< r prob)
          ret
          (recur (- r prob) (rest _m)))))))

(defn weighted-select-unchecked
  "Like weighted select, but does not require that vals sum to 1. Instead, given a map of [val n] being [key value], will return any given val with probability (/ n *sum-of-all-ns*)."
  [m]
  (let [total (sum (vals m))
        new-m (update-all
                m
                #(/ % total))]
    (weighted-select new-m)))

(defn with-prob
  "Returns true with probability prob (number between 0 and 1)."
  [prob]
  (< (rand) prob))

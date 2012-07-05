(ns belt.collections
  (:use [clojure.walk]
        [belt.general-utils :only [List
                              Map
                              MapEntry
                              Meta
                              Set
                              Vector
                              merge-meta>>]]
        [belt.math :only [sum]])
  (:require [clojure.zip :as z]))

(defn all-into
  "Given to, from, and a predicate, does a depth-first traversal, replacing all cases where (pred x) returns true with (into to x)."
  [to from pred]
  (postwalk
    (fn [x]
      (if (pred x)
        (into to x)
        x))
    from))

(defn append
  "Return coll with x concat-ed onto the end."
  [x coll]
  (concat coll [x]))

(defn coll-zip
  "Given a root, outputs a generic zipper that will maintain nested types."
  [root]
  (z/zipper coll?
            seq
            (fn [b-node c]
              (condp instance? b-node
                List (apply list c)
                Vector (vec c)
                Set (set c)
                Map (->> c (apply concat) (apply hash-map))
                MapEntry (vec c)))
            root))

(defn count-all
  "Counts all nested elements in form, where a collection counts as an element, as do the elements (if any) it contains."
  [form]
  (postwalk (fn [elt]
              (if (coll? elt)
                (inc (sum elt))
                1))
            form))

(defn count-all-by
  "Counts all elements in form for which (pred element) return true."
  [pred form]
  (postwalk (fn [elt]
              (if (coll? elt)
                (+ (sum elt)
                   (if (pred elt)
                     1
                     0))
                (if (pred elt)
                  1
                  0)))
            form))

(defn postwalk-indexed
  "Similar to 'map-indexed', only this covers all elements in a data structure of any structure or size. Indexes are depth-first post-order, just like postwalk."
  ([f form]
    (postwalk-indexed f 0 form))
  ([f idx form]
    ((if (coll? form)
       #(into (condp instance? form
                Map {}
                List '()
                Set #{}
                [])
              %)
       identity)
         (map (fn [sub-idx sub-form]
                (if (coll? sub-form)
                  (f sub-idx
                     (postwalk-indexed f
                                       (->> (count-all sub-form)
                                            dec
                                            (- sub-idx))
                                       sub-form))
                  (f sub-idx
                     sub-form)))
              (rest (reductions (fn [a b]
                                  (+ a (count-all b)))
                                (dec idx)
                                form))
              form))))

(defn cycle-shuffle
  "Takes a collection, cycles it n times, then returns the shuffle of that."
  [n coll]
  (shuffle (take n (cycle coll))))

(defn drop-at
  "Removes the first element in coll for which (pred element) returns true, and returns the resultant list."
  [coll pred]
  (concat
    (take-while (complement pred) coll)
    (rest (drop-while (complement pred) coll))))

(defn drop-at-nth
  "Removes the nth element in coll for which (pred element) returns true, and returns the resultant list."
  [coll pred n]
  (cond (< n 1) coll
        (not-any? pred coll) coll
        (= 1 n) (drop-at coll pred)
        :default (let [after-pred (drop-while (complement pred) coll)]
                   (concat
                     (take-while (complement pred) coll)
                     [(first after-pred)]
                     (drop-at-nth (rest after-pred)
                                  pred
                                  (dec n))))))

(defn index-at
  "Returns a list of the instances of finding n in coll."
  [n coll]
  (keep-indexed (fn [elt item] (if (= item n) elt)) coll))

(defn intervals
  "Returns the relative intervals between each successive element in colll."
  [coll]
  (map - (rest coll) coll))

(defn least
  "Where (f element) is the least in coll, returns element."
  [f coll]
  (first (sort-by f coll)))

(defn leasts
  "Returns the list of elements in coll for which (f element) is the least in coll."
  [f coll]
  (let [sorted-coll (sort-by f coll)]
    (take-while #(= (f (first sorted-coll)) (f %))
                sorted-coll)))

(defn my-map-indexed
  "My version of 'map-indexed', which is completely copied from the original, but slightly changed to be able to take an optional 'start-n' argument, so that the index can start at something other than 0."
  ([f coll]
    (map-indexed f 0 coll))
  ([f start-n coll]
    (letfn [(mapi [idx coll]
                  (lazy-seq
                    (when-let [s (seq coll)]
                      (if (chunked-seq? s)
                        (let [c (chunk-first s)
                              size (int (count c))
                              b (chunk-buffer size)]
                          (dotimes [i size]
                            (chunk-append b (f (+ idx i) (.nth c i))))
                          (chunk-cons (chunk b) (mapi (+ idx size) (chunk-rest s))))
                        (cons (f idx (first s)) (mapi (inc idx) (rest s)))))))]
      (mapi start-n coll))))

(defn map-struct
  "Returns a list of a struct mapped through coll."
  [stct coll]
  (map (partial apply struct stct) coll))

(defn mapcat-indexed
  "Combination of mapcat and map-indexed."
  [f coll]
  (apply concat (map-indexed f coll)))

(defn most
  "Where (f element) is the greatest in coll, returns element."
  [f coll]
  (first (sort-by f > coll)))

(defn mosts
  "Returns the list of elements in coll for which (f element) is the greatest in coll."
  [f coll]
  (let [sorted-coll (sort-by f > coll)]
    (take-while #(= (f (first sorted-coll)) (f %))
                sorted-coll)))

(defn multi-reduce
  "Like reduce, but with multiple argument collections. For example:
   (multi-reduce assoc {} [:a :b :c] [1 2 3])
   => {:c 3, :b 2, :a 1}"
  [f val & colls]
  (reduce (fn [x [& args]]
            (apply f x args))
          val
          (vec (apply map vector colls))))

(defn my-shuffle
  "My clojure-based shuffle function. It is much slower than the standard shuffle function."
  [coll]
  (if (= (count coll) 1)
    coll
    (let [elt (rand-nth coll)]
      (->> (drop-at coll #{elt})
           my-shuffle
           (cons elt)))))

(defn reduce-with-limit
  "Will reduce f down coll (with optional initial value), but will stop reducing after the given limit is reached."
  ([limit f coll]
    (reduce-with-limit limit f (first coll) (next coll)))
  ([limit f init coll]
    (last (take limit (reductions f init coll)))))

(defn seq-to-int
  "Converts a collection of digits in base 10 into a number of the given base. For example:
   (seq-to-int [1 2 3] 10)
   => 123"
  [coll base]
  (let [base-seq (iterate (partial * base) 1)]
    (sum (map * (reverse coll) base-seq))))

(defn super-shuffle
  "Shuffles everything in coll, including nested colls."
  [form]
  (postwalk (fn [sub-form]
              (if (coll? sub-form)
                (shuffle sub-form)
                sub-form))))

(defn walk-with-meta
  "Walk variant that conserves metadata."
  [inner outer form]
  (cond
    (list? form) (merge-meta>> (outer (merge-meta>>
                                        (apply list (map inner form))
                                        (meta form)))
                               (meta form))
    (instance? clojure.lang.IMapEntry form) (outer (vec (map inner form)))
    (seq? form) (merge-meta>> (outer (merge-meta>>
                                       (doall (map inner form))
                                       (meta form)))
                              (meta form))
    (coll? form) (merge-meta>> (outer (merge-meta>>
                                        (into (empty form) (map inner form))
                                        (meta form)))
                               (meta form))
    :else (outer form)))

(defn prewalk-with-meta
  "Prewalk variant that conserves metadata."
  [f form]
  (walk-with-meta (partial prewalk-with-meta f)
                  identity
                  (if (instance? Meta form)
                    (merge-meta>> (f form) (meta form))
                    form)))

(defn postwalk-with-meta
  "Postwalk variant that conserves metadata."
  [f form]
  (walk-with-meta (partial postwalk-with-meta f) f form))

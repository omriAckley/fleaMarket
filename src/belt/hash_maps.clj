(ns belt.hash-maps)

(defn assoc-all
  "Changes all the values in a hash-map to val."
  [m val]
  (apply assoc m (interleave (keys m) (repeat val))))

(defn assoc-by
  "Changes all values in m to val when (pred map-entry) returns true."
  [m pred val]
  (apply assoc m (interleave (map first (filter pred m)) (repeat val))))

(defn map-corresponding
  "Given a function and a variable number of hash-maps, returns a new hash-map, in which keys are now paired with the result of calling that function on corresponding values. If any key is not present in all of the given hash-maps, it will not appear in the resultant hash-map."
  [f & ms]
  (let [ks (keys (first ms))
        vss (map vals ms)
        new-f (fn [k & vs]
                [k (apply f vs)])]
    (into {} (apply map new-f ks vss))))

(defn remove-hash-map
  "Like remove, but removes key value pairs from a hash-map if (pred value) returns true."
  [pred m]
  (into {}
        (remove #(pred (val %))
                m)))

(defn reverse-map
  "Reverse the keys/values of a map."
  [m]
  (into {} (map (fn [[k v]] [v k]) m)))

(defn shuffle-hash-map
  "Like shuffle, but for hash-maps."
  [m]
  (->> m
       vec
       shuffle
       (into {})))

(defn update
  "When applied to a map, returns a new map of the same (hashed/sorted) type, that contains the mapping of key(s) to the result of calling f(s) on the corresponding val(s) at key(s). When applied to a vector, similar, but with key(s) used as index(es). Note - index must be <= (count vector)."
  [m k f & kfs]
  (let [ret (assoc m k (f (get m k)))]
    (if kfs
      (recur ret (first kfs) (second kfs) (nnext kfs))
      ret)))

(defn nested-alter
  "A better way of doing multiple updates/assocs on elements in a nested associative structure. For example:
   (nested-alter {:a {:b 1 :c 2} :d 3}
                 {:a {:c 10} :d dec})
   => {:a {:b 1 :c 10} :d 2}"
  [m ks_objs]
  (reduce (fn [m [k obj]]
            (if (map? obj)
              (update m k #(nested-alter % obj))
              (if (fn? obj)
                (update m k obj)
                (assoc m k obj))))
          m
          (vec ks_objs)))

(defn nested-merge
  "A better way of doing multiple assoc's on elements in a nested associative structure. For example:
   (nested-merge {:a {:b 1 :c 2} :d 3}
                 {:a {:c 10} :d 20})
   => {:a {:b 1 :c 10} :d 20}"
  [m new-m]
  (reduce (fn [m [k v]]
            (if (and (map? v)
                     (map? (m k)))
              (update m k #(nested-merge % v))
              (assoc m k v)))
          m
          (vec new-m)))

(defn nested-update
  "A better way of doing multiple updates on elements in a nested associative structure. For example:
   (nested-update {:a {:b 1 :c 2} :d 3}
                  {:a {:c inc} :d dec})
   => {:a {:b 1 :c 3} :d 2}"
  [m ks_objs]
  (reduce (fn [m [k obj]]
            (if (fn? obj)
              (update m k obj)
              (update m k #(nested-update % obj))))
          m
          (vec ks_objs)))

(defn update-by
  "Updates all values in m where (pred map-entry) returns true, by calling f on each."
  [m pred f]
  (if (empty? m)
    m
    (apply update m (interleave (map first (filter pred m)) (repeat f)))))

(defn sort-hash-map-nested-by
  "Sorts m and all nested hash-maps in m by comp."
  [comp m]
  (if (some #(map? (val %)) m)
    (update-by
      m
      #(map? (val %))
      #(sort-hash-map-nested-by comp %))
    (into {}
          (sort-by
            val
            (fn [x y] (if (or (map? x) (map? y))
                        false
                        (comp x y)))
            m))))

(defn update-all
  "Updates all values in m by calling f on each."
  [m f]
  (if (empty? m)
    m
    (apply update m (interleave (keys m) (repeat f)))))

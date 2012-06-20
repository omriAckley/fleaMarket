(ns combinations)

(defn all-unique-pairs
  "Given a collection, returns a list of all the unique pairs of the elements in the collection."
  [coll]
  (loop [unpaired coll
         paired []]
    (if (= 1 (count unpaired))
      paired
      (recur (rest unpaired)
             (concat paired
                     (map (partial vector (first unpaired))
                          (rest unpaired)))))))

(defn permute
  "Returns a list of mapping f through all the possible permutations given a series of collections--using each element in the first collection as the first argument to f, each element in the second collections as the second argument to f, etc."
  [f & colls]
  (letfn [(partial-fs+args
            [f-coll arg-coll]
            (flatten
              (map (fn [_f]
                     (map (fn [_arg]
                            (partial _f _arg))
                          arg-coll))
                   f-coll)))]                 
    (mapcat (fn [elem-of-last-coll]
              (for
                [part-f (reduce partial-fs+args [f] (drop-last colls))]
                (part-f elem-of-last-coll)))
            (last colls))))

(defn permutations
  "Given n and a coll, returns a list of all permutations of elements in coll for an n-sized list."
  [n coll]
  (if (< n 1)
    '(())
    (mapcat (fn [x] (map #(cons x %)
                         (permutations (dec n)
                                       coll)))
            coll)))

(defn combinations
  "Given n and a coll, returns a list of all combinations of elements in coll for an n-sized list."
  [n coll]
  (loop [to-be-consed-seq (map list coll)
         ret '(())]
    (if (< (count (first ret)) n)
      (recur (reverse
               (reductions (fn [prev curr]
                             (repeat (+ (count prev) (count curr))
                                     (first curr)))
                           (reverse to-be-consed-seq)))
             (mapcat (fn [to-be-consed]
                       (map (fn [ret-elt cons-elt]
                              (cons cons-elt ret-elt))
                            (take-last (count to-be-consed)
                                       ret)
                            to-be-consed))
                     to-be-consed-seq))
      ret)))

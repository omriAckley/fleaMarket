(ns utils
  (:use
    clojure.walk
    [clojure.set :only (intersection)]))





;;----------------------------------------------------------------
;;CONSTANTS
;;----------------------------------------------------------------

(def Keyword clojure.lang.Keyword)
(def Vector clojure.lang.PersistentVector)
(def Map clojure.lang.PersistentArrayMap)
(def Collection clojure.lang.IPersistentCollection)
(def Function clojure.lang.Fn)
(def Regex java.util.regex.Pattern)
(def Set clojure.lang.PersistentHashSet)

;;Unecessary, because are already def'ed as such...
#_(def Integer java.lang.Integer)
#_(def Number java.lang.Number)
#_(def String java.lang.String)
#_(def Character java.lang.Character)





;;----------------------------------------------------------------
;;FUNCTIONS
;;----------------------------------------------------------------

;;;;;;;;;;;;
;;;;GENERAL UTILITY
;;;;;;;;;;;;

(def any? (complement not-any?))

(defn char-to-dig
  "Converts a character to a digit."
  [ch]
  (case ch
    \0 0
    \1 1
    \2 2
    \3 3
    \4 4
    \5 5
    \6 6
    \7 7
    \8 8
    \9 9))

(defmulti constrain
  "Constrain n to be no less than low and no greater than high."
  (fn [obj low high] (type obj)))
(defmethod constrain Collection
  [coll low high]
  (remove (fn [item] (or (> low item) (< high item))) coll))
(defmethod constrain :default
  [n low high]
  (cond (< n low) low (> n high) high :else n))

(defn fn>
  "Similar to 'partial'. Given a function f and arguments, returns a partial version f with those arguments. This partial of f can take any number of additional arguments, which will be inserted at the beginning of f."
  [f & args]
  (fn [& additional_args] (apply f (concat additional_args args))))

;;Literally the same as 'partial'. Given a function f and arguments, returns a partial version f with those arguments. This partial of f can take any number of additional arguments, which will be inserted at the end of f.
(def fn>>
  partial)

(defn iterate-nth
  "Takes the nth of (iterate f x)."
  [f x n]
  (nth (iterate f x) n))

(defn var+
  "The regular var function does not evaluate its argument, leading to problems. Var+ calls var after evaluating the object it is passed."
  [obj]
  (eval `(var ~obj)))

(defn meta+
  "My own version of the meta function, one that uses var+ so that something like (#(meta+ %) 'obj-name) will work, whereas (#(meta %) obj-name) will not always work."
  [obj]
  (meta (var+ obj)))

(def not-zero? (complement zero?))

(defn quote+
  "My own version of quote, that will evaluate a form, then return, quoted, that return value."
  [form]
  (eval `(quote (quote ~form))))

(defn unless-false
  "Returns first argument unless it evaluates to logical false. In which case, returns second argument."
  [unless-false else]
  (if-let [then unless-false]
    then
    else))



;;;;;;;;;;;;
;;;;MATH
;;;;;;;;;;;;

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

(defn abs
  "(abs n) is the absolute value of n"
  [n]
  (cond
   (not (number? n)) (throw (IllegalArgumentException. "abs requires a number"))
   (neg? n) (- n)
   :else n))

(defn abs-diff
  "Returns the absolute difference of args."
  [& args]
  (Math/abs (apply - args)))

(defn ave
  "Returns the average of the numbers passed to it."
  [coll]
  (/ (reduce + coll) (count coll)))

(defn geometric-mean
  "Returns the geometric mean of the collection of numbers passed to it."
  [coll]
  (if (= 2 (count coll))
    (Math/sqrt (apply * coll))
    (let [exponent (/ 1 (count coll))]
      (reduce #(* %1 (Math/pow %2 exponent))
              (cons 1 coll)))))

(defn inverse
  "Returns 1 divided by n."
  [n]
  (/ 1 n))

(def my-round
  (comp #(Math/round %) float))

(defn pd
  "Same as division, but any errors (i.e. divide by zero) are caught, in which case 0 is returned."
  [& nums]
  (try (apply / nums) (catch Exception e 0)))

(defn sum
  "Returns the sum of the numbers passed to it."
  [coll]
  (reduce + coll))



;;;;;;;;;;;;
;;;;PROBABILITY AND COMBINATIONS
;;;;;;;;;;;;

;     *Notes on combins function* 
; The local function, "partial-fs+args" returns a list of 
;   partial functions using all fs in f-coll and all args 
;   in arg-coll. 
; A list of growing partial functions is recursively 
;   produced by the reduce function. The list of partial 
;   functions will encompass all but the last set of 
;   arguments (those of the last-coll in colls). 
; Mapcating down the elements in the last collection, the 
;   list of partials is then called sequentially (in a 
;   for) on each element of the last collection.
(defn combins
  "Returns a list of mapping f through all the possible combinations given a series of collections--using each element in the first collection as the first argument to f, each element in the second collections as the second argument to f, etc."
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

(declare update-all)
(defn weighted-select-unchecked
  "Like weighted select, but does not require that vals sum to 1. Instead, given a map of [val n] being [key value], will return any given val with probability (/ n *sum-of-all-ns*)."
  [m]
  (let [total (sum (vals m))
        new-m (update-all
                m
                #(/ % total))]
    (weighted-select new-m)))



;;;;;;;;;;;;
;;;;COLLECTION MANIPULATION
;;;;;;;;;;;;

(defn append
  "Return coll with n concat-ed onto the end."
  [coll n]
  (concat coll [n]))

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

(def intersect intersection)

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

(defn my-shuffle
  "My clojure-based shuffle function. It is much slower than the standard shuffle function."
  [coll]
  (if (= (count coll) 1)
    coll
    (let [elt (rand-nth coll)]
      (->> (drop-at coll #{elt})
           my-shuffle
           (cons elt)))))

(defn postwalk-indexed
  ([f form]
    (postwalk-indexed f 0 form))
  ([f idx form]
    (walk (partial postwalk-indexed f (inc (inc idx))) #(f (inc idx) %) form)))

(defn postwalk-indexed
  ([f form]
    (postwalk-indexed f 0 form))
  ([f idx form]
    (if (coll? form)
      (f idx (map #(postwalk-indexed f %1 %2) (iterate inc (inc idx)) form))
      (f idx form))))

(defn seq-to-int
  "Converts a collection of digits in base 10 into a number of the given base. Collection should be in reverse order.
   >> (seq-to-int [1 2 3] 10)
   >> 321"
  [coll base]
  (let [base-seq (iterate (partial * base) 1)]
    (sum (map * coll base-seq))))

(defmulti super-shuffle
  "Shuffles EVERYTHING in coll, including nested colls."
  type)
(defmethod super-shuffle Collection [coll] (map super-shuffle (shuffle coll)))
(defmethod super-shuffle :default [not-coll] not-coll)



;;;;;;;;;;;;
;;;;HASH-MAP MANIPULATION
;;;;;;;;;;;;

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
  "A better way of doing multiple updates/assocs on elements in a nested associative structure. E.g. (nested-alter {:a {:b 1 :c 2} :d 3} {:a {:c 10} :d dec}) will return {:a {:b 1 :c 10} :d 2}."
  [m ks_objs]
  (reduce (fn [m [k obj]]
            (if (isa? (type obj) Map)
              (update m k #(nested-alter % obj))
              (if (isa? (type obj) Function)
                (update m k obj)
                (assoc m k obj))))
          m
          (vec ks_objs)))

(defn nested-update
  "A better way of doing multiple updates on elements in a nested associative structure. E.g. (nested-update {:a {:b 1 :c 2} :d 3} {:a {:c inc} :d dec}) will return {:a {:b 1 :c 3} :d 2}."
  [m ks_objs]
  (reduce (fn [m [k obj]]
            (if (isa? (type obj) Function)
              (update m k obj)
              (update m k #(nested-update % obj))))
          m
          (vec ks_objs)))

(defn update-all
  "Updates all values in m by calling f on each."
  [m f]
  (apply update m (interleave (keys m) (repeat f))))





;;----------------------------------------------------------------
;;MACROS
;;----------------------------------------------------------------
(defmacro %->
  "Threads the expr through the forms. Inserts x in the position specified by % markers, unless the form is not inside a list, in which case it simply makes a list in which x is the only argument passed to form (form x). If there are more forms (which there usually are), inserts the composite form as the 'x' for the next form."
  ([x] x)
  ([x form] (if (seq? form)
              (with-meta (postwalk-replace {'% x} form) (meta form))
              (list form x)))
  ([x form & more] `(%-> (%-> ~x ~form) ~@more)))

(defmacro catch-nested
  "If any exceptions are caught inside form, it will return a custom error message, specifying the (innermost) element in form which failed. Otherwise, simply returns the result of form."
  [form]
  (let [msg (str "<<ERROR AT:" form ">> ... ")]
    (list 'try
          (if (seq? form)
            (for [x form]
              (list 'catch-nested x))
            (eval form))
          (list 'catch 'Exception 'e
                (list 'throw
                      (list 'eval
                            (list 'list
                                  ''new
                                  '(type e)
                                  (list 'str
                                        msg
                                        (list '.getMessage 'e)))))))))

;     *Notes on combins macro*
; The macro returns and evaluates a for list comprehension 
;   with a vector of bindings derived by interleaving a 
;   list of infinite "anonymous" symbols (_1 _2 _3 ...) 
;   with the series of collections (colls).
; In the interleave, coll must be backquoted and then 
;   unquoted to avoid premature evaluation problems.
; Only (count colls) should be taken from the infinite
;   symbol list when it f is prepended to it.
#_(defmacro combins
    "Returns a list of mapping f through all the possible combinations given a series of collections--using each element in the first collection as the first argument to f, each element in the second collections as the second argument to f, etc."
    [f & colls]
    (let [inf-sym-list (repeatedly gensym)]
      `(for ~(vec (interleave inf-sym-list `~colls))
         (~f ~@(take (count colls) inf-sym-list)))))

(defmacro def-
  "Same as def, yielding private def."
  [name value]
  (list `def (with-meta name (assoc (meta name) :private true)) value))

(defmacro defmulti-
  "Same as defmulti, yielding private def."
  [name & more]
  (list* `defmulti (with-meta name (assoc (meta name) :private true)) more))

;     *Notes on defatom macro*
; The macro below is pretty baller, but it takes up a silly
;   amount of space--every time it defines an atom it also
;   defines a function !atom-name that will alter the value
;   inside the atom.
(defmacro defatom
  "Defines an atom with name and optional value. Also defines !name as a macro that changes the value of that atom."
  ([name]
    `(defatom ~name nil))
  ([name value]
    `(do
       (def ~name (atom ~value))
       (defmacro
         ~(symbol (str "!" name))
         ~['obj]
         (list
           ~'(if (eval `(fn? ~obj)) `swap! `reset!)
           (quote ~name)
           ~'obj)))))

(defmacro delivered?
  [prom]
  (list 'let ['fut (list 'future (list 'deref prom))]
        (list 'Thread/sleep 1)
        (list 'future-done? 'fut)))

(defmacro multi-reduce
    "Like reduce, but with multiple argument collections. For example:
     (multi-reduce assoc {} [:a :b :c] [1 2 3])
     => {:c 3, :b 2, :a 1}"
    [f val & colls]
    `(reduce (fn ~'[x [& args]]
               (apply ~f ~'x ~'args))
             ~val
             ~(vec (apply map vector colls))))











;;----------------------------------------------------------------
;;GRAVEYARD
;;----------------------------------------------------------------
#_(
    ;;Update function--a different version. Fewer lines and conceptually simpler, but slower.
    (defn update
      "When applied to a map, returns a new map of the same (hashed/sorted) type, that contains the mapping of key(s) to the result of calling f(s) on the corresponding val(s) at key(s). When applied to a vector, similar, but with key(s) used as index(es). Note - index must be <= (count vector)."
      ([m k f] (. clojure.lang.RT (assoc m k (f (get m k)))))
      ([m k f & kfs]
        (apply update (update m k f) kfs)))
    
    ;;Unknown
    (defn silly-fn
      [n]
      ((if (odd? n) - +) n 1)
      #_(->> (mod n 2) (* 2) (- 1) (+ n)))
    (defn thing
      [fnd]
      (keep-indexed
        (fn [n item] (if (= fnd item) (nth ksvs (silly-fn n))))
        ksvs))
    
    ;;Update--macro version, with format example (update m k1 (+ 1) k2 (- 2)) as opposed to (update m k1 #(+ % 1) k2 #(+ % 2)).
    (defmacro update
      "When applied to a map, returns a new map of the same (hashed/sorted) type, that contains the mapping of key(s) to the result of evaluating form(s) with val(s) at corresponding key(s) inserted as the first argument(s). When applied to a vector, similar, but with key(s) used as index(es). Note - index must be <= (count vector)."
      ([m k form]
        (. clojure.lang.RT (assoc m k
                                  (if (seq? form)
                                    `(~(first form) ~(get m k) ~@(rest form))
                                    `(~form ~(get m k))))))
      ([m k form & kfs]
        `(update
           ~(. clojure.lang.RT (assoc m k
                                      (if (seq? form)
                                        `(~(first form) ~(get m k) ~@(rest form))
                                        `(~form ~(get m k)))))
           ~(first kfs) ~(second kfs) ~@(nnext kfs))))
    
    ;;Combins
    (defn combins-test
      [f c1 c2]
      (map (fn [n] (map (partial f n) c2)) c1))
    (defmacro combins-test
      [f & colls]
      (let [inf-sym-list (repeatedly gensym)]
        `(let ~(vector 'symbol-list `(take ~(count colls) inf-sym-list))
           (eval `(for ~(vec (interleave ~'symbol-list ~(vec colls)))
                    (~~f ~@~'symbol-list))))))
    
    ;;Defatom etc.
    (defmacro defatom
      "Defines an atom with name and optional value."
      ([name]
        `(defatom ~name nil))
      ([name value]
        `(def ~name (atom ~value))))
    (defmacro !
      "Changes the value of an atom."
      [name obj]
      (list
        (if (eval `(fn? ~obj)) `swap! `reset!)
        name
        obj)))
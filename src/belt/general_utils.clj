(ns belt.general-utils
  (:use clojure.walk)
  (:require [clojure.string :as s :only [blank?]]
            [clojure.zip :as z]))

(def Keyword clojure.lang.Keyword)
(def Vector clojure.lang.APersistentVector)
(def Map clojure.lang.APersistentMap)
(def MapEntry clojure.lang.IMapEntry)
(def Collection clojure.lang.IPersistentCollection)
(def List clojure.lang.IPersistentList)
(def Sequence clojure.lang.ASeq)
(def Function clojure.lang.Fn)
(def Regex java.util.regex.Pattern)
(def Set clojure.lang.APersistentSet)
(def ClojureObject clojure.lang.IObj)
(def Atom clojure.lang.Atom)
(def Agent clojure.lang.Agent)
(def Ref clojure.lang.Ref)
(def Meta clojure.lang.IMeta)

;;Unecessary, because are already def'ed as such...
#_(def Integer java.lang.Integer)
#_(def Number java.lang.Number)
#_(def String java.lang.String)
#_(def Character java.lang.Character)


(defn =?
  "Returns a predicate function that itself returns true if x is equal to the argument passed to it."
  [x]
  (fn [y]
    (= y x)))

(defn <?
  "Returns a predicate function that itself returns true if x is less than the argument passed to it."
  [x]
  (fn [y]
    (< y x)))

(defn >?
  "Returns a predicate function that itself returns true if x is greater than the argument passed to it."
  [x]
  (fn [y]
    (> y x)))

(defmacro %->
  "Threads the expr through the forms. Inserts x in the position specified by % markers, unless the form is not inside a list, in which case it simply makes a list in which x is the only argument passed to form (form x). If there are more forms (which there usually are), inserts the composite form as the 'x' for the next form."
  ([x] x)
  ([x form] (if (seq? form)
              (with-meta (postwalk-replace {'% x} form) (meta form))
              (list form x)))
  ([x form & more] `(%-> (%-> ~x ~form) ~@more)))

(defn all-empty?
  "Returns true if coll has no non-empty nested colls in it."
  [coll]
  (empty? (flatten coll)))

(def any? some)

(def any-pred some-fn)

(defn atom?
  "Returns true if the passed object is an atom."
  [obj]
  (instance? Atom obj))

(defn boolean?
  "Returns true if the passed object is a boolean."
  [obj]
  (instance? Boolean obj))

(defmacro casep
  "Like case, except instead of matching by equality, matches by corresponding predicate functions."
  [e & clauses]
  `(condp ~(fn [f x] (f x)) ~e
     ~@clauses))

;;Probably doesn't work!
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

(defn clojure-object?
  "Returns true if x is a clojure object (clojure.lang.IObj)."
  [x]
  (instance? ClojureObject x))

(defn merge-meta>>
  "Merges the current metadata of obj with m. Where conflicts arise, (meta obj) takes precedence."
  [obj m]
  (vary-meta obj #(merge m %)))

(defn conserve-meta
  "Given a function and an object, returns (f obj) with any new metadata (that may have been added) merged with the old metadata of object. Where conflicts arise between new and old metadata, the new is given precedence."
  [f obj]
  (merge-meta>> (f obj) (meta obj)))

(defn constrain
  "Constrain n to be no less than low and no greater than high."
  [n low high]
  (cond (< n low) low
        (> n high) high
        :else n))

(defn count=
  "Returns true if count of coll is equal to n."
  [n coll]
  (= n (count coll)))

(defn count=?
  "Returns a predicate function that itself returns true if n is equal to the count of the collection passed to it."
  [n]
  (fn [coll]
    (count= n coll)))

(defmacro eval-args
  "Takes a macro and a sequence of args, evaluates the args, then calls the macro on the evaluated results."
  [nm & args]
  (list* nm (map eval args)))

(defn fn>
  "Similar to 'partial'. Given a function f and arguments, returns a partial version f with those arguments. This partial of f can take any number of additional arguments, which will be inserted at the beginning of f."
  [f & args]
  (fn [& additional_args] (apply f (concat additional_args args))))

(def ^{:doc "Literally the same as 'partial'. Given a function f and arguments, returns a partial version f with those arguments. This partial of f can take any number of additional arguments, which will be inserted at the end of f."}
  fn>> partial)

(defn if-then-fn
  "Returns then if test is true, else returns (f then)."
  [test then f]
  (if test
    then
    (f then)))

(defn iterate-nth
  "Takes the nth of (iterate f x)."
  [f x n]
  (nth (iterate f x) n))

(defn key-to-sym
  "Converts a keyword to a symbol."
  [k]
  (symbol (name k)))

(defmacro let-literal
  "Given bindings [name1 value1 ..etc], replaces the literal of the name with the literal of the value, then evaluates the body."
  [bindings & body]
  (list* 'do
         (map #(postwalk-replace (into {} [bindings]) %)
              body)))

(defn merge-meta>
  "Merges the current metadata of obj with m. Where conflicts arise, m takes precedence."
  [obj m]
  (vary-meta obj merge m))

(defn meta?
  "Returns true if obj can have metadata."
  [obj]
  (instance? Meta obj))

(defn mutable?
  "Returns true if x is an atom, agent, or ref."
  [obj]
  (or (instance? Atom obj)
      (instance? Agent obj)
      (instance? Ref obj)))

(defn var+
  "The regular var function does not evaluate its argument, leading to problems. Var+ calls var after evaluating the object it is passed."
  [obj]
  (eval `(var ~obj)))

(defn meta+
  "My own version of the meta function, one that uses var+ so that something like (#(meta+ %) 'obj-name) will work, whereas (#(meta %) obj-name) will not always work."
  [obj]
  (meta (var+ obj)))

(def not-zero? (complement zero?))

(def ^{:doc "Predicate that returns true if passed something that is 0, nil, false, composed of only empty collections, or a blank string."}
  nothing?
  (any-pred (every-pred number? zero?)
            (every-pred coll? all-empty?)
            (every-pred string? s/blank?)
            nil?
            false?))

(def something?
  (complement nothing?))

#_(defn path-to
  "Given a sub-form and an expression, returns the list of super-expressions leading to sub-form."
  [sub-form expr]
  (let [path (atom [])
        found? (atom false)]
    (postwalk (fn [form]
                (let [super-form (anywhere? #{sub-form} form)]
                  (if (#{sub-form} super-form)
                    (reset! found? true))
                  (when super-form
                    (println super-form)
                    (swap! path
                           conj super-form)))
                form)
              expr)
    @path))

#_(defn anywhere?
  "Returns form if pred is true anywhere within form, otherwise nil."
  [pred form]
  (postwalk (fn [obj]
              (if (or (pred obj)
                      (not (coll? obj))
                      (some pred obj))
                obj))
            form))

(defn prepeatedly
  "Same as repeatedly, except that it must be finite, and it evaluates the functions in parallel."
  [n f]
  (pmap (fn [_] (f)) (range n)))

(declare str-to-literal-str)
(declare unless-then-fn)
#_(defn print-exception-form
  "Will print the exception, as well as the deepest sub-expression in exprs which is the cause of the exception. Optional second argument include-path?, which, if true, will also print the path of super-epxressions leading to the failing form."
  ([expr]
    (postwalk (fn [form]
                (try (do (eval form)
                         form)
                     (catch Exception e
                            (do (println e)
                                (println
                                  (str "#<At form: "
                                       form
                                       ">"))))))
              expr))
  ([expr include-path?]
    (postwalk (fn [form]
                (try (do (eval form)
                         form)
                     (catch Exception e
                            (do (println e)
                                (println
                                  (str "#<At form: "
                                       form
                                       ">"
                                       ))
                                (when include-path?
                                  (println
                                    (str "#<Path: "
                                         blah
                                         ">")))))))
              expr)))

(defn quote+
  "My own version of quote, that will evaluate a form, then return, quoted, that return value."
  [form]
  (eval `(quote (quote ~form))))

(defn ref?
  "Returns true if object is a ref."
  [obj]
  (instance? Ref obj))

(defmacro reflexive
  "Given a form, creates a reflexive structure. In the syntax, 'this' represents the whole form--e.g. (reflexive {:a 1 :b (inc (this :a))}) will return {:a 1 :b 2}. Reflexivity will only be valid if it is 'serial'--which is to say, the definition of any given element cannot depend upon an element subsequent to it with respect to a depth-first traversal of the form being defined."
  [form]
  (letfn [(local-zip
            [root]
            (z/zipper (every-pred coll?
                                  (complement list?)
                                  (complement seq?))
                      seq
                      (fn [b-node c]
                        (condp instance? b-node
                          Vector (vec c)
                          Set (set c)
                          Map (->> c (apply concat) (apply hash-map))
                          MapEntry (vec c)))
                      root))
          (make-safe
            [unsafe-form]
            (prewalk (fn [sub-form]
                       (if (and (seq? sub-form)
                                (some #{'this} (flatten sub-form)))
                         (let [message (str "<reflexive exception> Non-serial self-reference in definition."
                                            "\nFor value: " sub-form)]
                           `(throw (Exception. ~message)))
                         sub-form))
                     unsafe-form))]
    (loop [loc (local-zip form)]
      (if (z/end? loc)
        (z/root loc)
        (recur
          (z/next
            (if (list? (z/node loc))
              (z/edit loc (fn [list-form]
                            (postwalk (fn [form]
                                        (if (and (seq? form)
                                                 (some #{'this} form))
                                          (eval (replace {'this (quote+ (-> loc z/root make-safe))}
                                                         form))
                                          form))
                                      list-form)))
              loc)))))))

(defn str-to-literal-str
  "Given a string s, returns s as a literal string, so that when printed, the quotes are included (i.e. \"this\" as opposed to this)."
  [s]
  (str "\"" s "\""))

(defn unbound?
  "Tests whether the given argument is unbound, which would happen if it had been declared but not defined. Note, this will throw an exception when passed anything that has been niether declared nor defined (i.e. it doesn't exist)."
  [v]
  (instance? clojure.lang.Var$Unbound v))

(defn unless
  "If (pred then) returns true, returns else, else returns then."
  ([pred then]
    (unless pred then nil))
  ([pred then else]
    (if (pred then)
      else
      then)))

(defn unless-then-fn
  "If (pred then) returns true, then returns (f then), else returns then."
  [pred then f]
  (unless pred then (f then)))

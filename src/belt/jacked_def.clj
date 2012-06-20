(ns jacked-def
  (:use [clojure.walk]
        [belt.general-utils :only [count=?
                              merge-meta>
                              ref?
                              reflexive
                              str-to-literal-str
                              unless
                              unless-then-fn]]
        [belt.collections :only [all-into
                            postwalk-with-meta]]
        [belt.hash-maps :only [nested-merge]]))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Additional private definitions

(defmacro def-
  "Same as def, yielding private def."
  [name value]
  (list `def (with-meta name (assoc (meta name) :private true)) value))

(defmacro defmulti-
  "Same as defmulti, yielding private def."
  [name & more]
  (list* `defmulti (with-meta name (assoc (meta name) :private true)) more))

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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Expressions

(comment *THINGS TO CHANGE about def-expression-type*
         -Don't let any unused special terms into any resultant expression definition. It would just waste space to do so.)
(defmacro def-expression-type
  "Makes a new expression type, which generates a wrapper_[name] macro with unique metadata, such as :special-terms, which lists the special terms that are available to be nested in the expression, as well as what these terms represent (i.e. what they evaluate to). It also defines [name] as a hash-map of the :wrapper-name, :resultant-fn-arglists, and :special-terms. The wrapper_[name] macro will take an expression (with the relevant context) and output a function. The :arglists metadata on this resultant function is outputted along with it. For example:
   (def-expression-type example-expr [arg1 arg2] [special-term1 (inc arg1) special-term2 (dec arg2)])
   => [ns]/wrapper_example-expr
   example-expr
   => {:wrapper-name wrapper_example-expr
       :resultant-fn-arglists ([arg1 arg2])
       :special-terms [special-term1 (inc arg1) special-term2 (dec arg2)]}
   (meta #'wrapper_example-expr)
   => {:resultant-fn-arglists ([arg1 arg2]),
       :arglists ([example-expr]),
       :expression-wrapper true,
       :ns #<Namespace [ns]>,
       :name wrapper_example-expr,
       :clooj/src \"(def-expression-type example-expr [arg1 arg2] [special-term1 (inc arg1) special-term2 (dec arg2)])\",
       :macro true,
       :line 1,
       :file \"NO_SOURCE_PATH\",
       :special-terms [special-term1 (inc arg1) special-term2 (dec arg2)]}
   (def example-fn (wrapper_example-expr (* special-term1 special-term2)))
   => [ns]/example-fn
   (example-fn 9 11)
   => 100
   (meta example-fn)
   => {:arglists ([arg1 arg2])}"
  [nm args special-terms]
  `(do
     (def ~nm
       {:wrapper-name '~(symbol (str "wrapper_" nm))
        :special-terms '~special-terms
        :resultant-fn-arglists '(~args)})
     (defmacro ~(symbol (str "wrapper_" nm))
       {:expression-wrapper true
        :arglists '([~nm])
        :special-terms '~special-terms
        :resultant-fn-arglists '(~args)}
       ~'[expression]
       `(with-meta
          (fn ~'~args
            (let ~'~special-terms
              ~~'expression))
            {:arglists '(~'~args)
             :converted-expression '(~~''let ~'~special-terms ~~'expression)}))))

(defmacro wrapper
  "A generalized wrapper that takes an expression type and an expression and then outputs a function representing the expression evaluated in the context specified by its expression-type."
  [expression-type expression]
  (let [args (first ((eval expression-type) :resultant-fn-arglists))
        special-terms ((eval expression-type) :special-terms)]
    `(with-meta
       (fn ~args
         (let ~special-terms
           ~expression))
       {:arglists '(~args)
        :converted-expression '(~'let ~special-terms ~expression)})))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;From the old clojure.contrib.def

(defn name-with-attributes
  "*Taken from old clojure.contrib.def*
   To be used in macro definitions. Handles optional docstrings and attribute maps for a name to be defined in a list of macro arguments. If the first macro argument is a string, it is added as a docstring to name and removed from the macro argument list. If afterwards the first macro argument is a map, its entries are added to the name's metadata map and the map is removed from the macro argument list. The return value is a vector containing the name with its extended metadata map and the list of unprocessed macro arguments."
  [name macro-args]
  (let [[docstring macro-args] (if (string? (first macro-args))
                                 [(first macro-args) (next macro-args)]
                                 [nil macro-args])
        [attr macro-args]          (if (map? (first macro-args))
                                     [(first macro-args) (next macro-args)]
                                     [{} macro-args])
        attr                       (if docstring
                                     (assoc attr :doc docstring)
                                     attr)
        attr                       (if (meta name)
                                     (conj (meta name) attr)
                                     attr)]
    [(with-meta name attr) macro-args]))

(defmacro defnk
  "*Taken from old clojure.contrib.def*
   Define a function accepting keyword arguments. Symbols up to the first keyword in the parameter list are taken as positional arguments.  Then an alternating sequence of keywords and defaults values is expected. The values of the keyword arguments are available in the function body by virtue of the symbol corresponding to the keyword (cf. :keys destructuring). defnk accepts an optional docstring as well as an optional metadata map."
  [fn-name & fn-tail]
  (let [[fn-name [args & body]] (name-with-attributes fn-name fn-tail)
        [pos kw-vals]           (split-with symbol? args)
        syms                    (map #(-> % name symbol) (take-nth 2 kw-vals))
        values                  (take-nth 2 (rest kw-vals))
        sym-vals                (apply hash-map (interleave syms values))
        de-map                  {:keys (vec syms)
                                 :or   sym-vals}]
    `(defn ~fn-name
       [~@pos & options#]
       (let [~de-map (apply hash-map options#)]
         ~@body))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;def-new stuff. Replacement for defnk, which allows not only keyword and default arguments, but non-default keyword arguments--and all nested. It also outputs a nice arg-list denoting what keyword arguments can be passed, as well as default values, or REQ (for required) if no default. Additionally, special metadata :hidden on any argument value pair allows for hiding of this argument from the arg-list, and special metadata :transient (which must have a ref as its value) saves initial values (derived when the object is first evaluated) that can be reset to at any later point.

(def- vec-of-count-2?
      (every-pred vector? (count=? 2)))

(defn- vec-of-only-vecs-of-count-2?
  [obj]
  (and (vector? obj)
       (not (empty? obj))
       (every? vec-of-count-2?
               obj)))

(defn- all-toMap-ables-into-maps
  [form] (all-into {} form vec-of-only-vecs-of-count-2?))

(defn- preprocess-bindings
  [bindings]
  (postwalk-with-meta
    (fn [form]
      (letfn [(hide-if-transient 
                [x]
                (if (:transient (meta x))
                  (merge-meta> x
                               {:hidden true})
                  x))
              (hide-if-calls-this
                [x]
                (if (and (vec-of-count-2? x)
                         (not (vec-of-only-vecs-of-count-2?
                                (second x)))
                         (some #{'this}
                               (flatten x)))
                  (merge-meta> x
                               {:hidden true})
                  x))
              (require-if-no-default
                [x]
                (if (and (vector? x)
                         (= 1 (count x)))
                  (merge-meta> x
                               {:required true})
                  x))
              (unhide-if-required
                [x]
                (if (:required (meta x))
                  (merge-meta> x
                               {:hidden false})
                  x))
              (add-default-throw-if-required
                [x]
                (if (:required (meta x))
                  (let [message
                        (str "'"
                             (first x)
                             "' requires an argument.")]
                    (assoc x 1
                           `(throw
                              (Exception. ~message))))
                  x))]
        (-> form
            hide-if-transient
            hide-if-calls-this
            require-if-no-default
            unhide-if-required
            add-default-throw-if-required)))
    (vec bindings)))

(defn- derive-transients
  [preprocessed-bindings]
  (postwalk-with-meta
    (fn [form]
      (letfn [(filter-transients
                [x]
                (filter #(:transient (meta %))
                        x))
              (make-transient-key-seqs
                [x]
                (if (empty? x)
                  x
                  (merge-meta>
                    (vec (mapcat (fn [[k v]]
                                   (if (:transient-key-seq (meta v))
                                     (map (fn [sub-ks]
                                            (vec (cons k sub-ks)))
                                          v)
                                     [[k]]))
                                 x))
                    {:transient-key-seq true})))
              (make-transient
                [x]
                (merge-meta> x
                             {:transient true}))]
        (cond
          (vec-of-only-vecs-of-count-2? form) (-> form
                                                  filter-transients
                                                  make-transient-key-seqs)        
          (and (vec-of-count-2? form)
               (:transient-key-seq
                 (meta (second form)))) (make-transient form)
          :else form)))
    preprocessed-bindings))

(defn- derive-base-values-map
  [return-bindings-map transients-list]
  (into {}
        (map (fn [ks]
               [ks (let [v (get-in return-bindings-map ks)]
                     (if (ref? v)
                       (deref v)
                       (throw (Exception.
                                (str "<def-new transient exception> Value passed to a transient must be a ref."
                                     "\nFailed for value: " (unless-then-fn string? v str-to-literal-str)
                                     "\nAt: " (list 'get-in 'this ks))))))])
             transients-list)))

(defn- derive-arg-map
  [preprocessed-bindings]
  (all-toMap-ables-into-maps
    (postwalk-with-meta (fn [form]
                          (letfn [(insert-REQ-if-required
                                    [x]
                                    (if (:required (meta x))
                                      (assoc x 1 ''REQ)
                                      x))
                                  (remove-hidden
                                    [x]
                                    (if (vec-of-only-vecs-of-count-2? x)
                                      (vec (remove #(:hidden (meta %))
                                                   x))
                                      x))]
                            (-> form
                                insert-REQ-if-required
                                remove-hidden)))
                        preprocessed-bindings)))

(comment *THINGS TO CHANGE ABOUT def-new*
         -Bug: (def-new test-thing [:arg])
         -Bug: (def-new test-thing [:arg (fn [x] x)])
         -Assertions!
         -Transients should not have to be refs, but rather should consider any first-level refs within them to be transient (maybe).
         -Metadata :arglists should sort to put required args first, including args that have nested requires within them
         -Fix it so that throws (for :required issues) tell you more than local information (i.e. if it says that :x is required, also say what :x is a sub-argument of--if anything))
(defmacro def-new
  "Defines a new type of hash-map that takes :key value argument style, with some arguments required and others optional--in which case they must have a default value. Additionally, transient arguments (tagged with ^:transient metadata), must be refs, and can be reset to their base values by calling 'transients-to-base-values'."
  {:arglists '([name doc-string? attr-map? [key value?]*])}
  [nm & args]
  (let [opt-doc-string (unless (complement string?) (first args))
        opt-attr-map (unless (complement map?) ((if opt-doc-string
                                                  second
                                                  first) args))
        bindings (drop (+ (if opt-doc-string 1 0)
                          (if opt-attr-map 1 0))
                       args)
        preprocessed-bindings (preprocess-bindings
                                bindings)
        transients (derive-transients
                     preprocessed-bindings)
        argmap (derive-arg-map
                 preprocessed-bindings)
        preprocessed-bindings-m (all-toMap-ables-into-maps
                                  preprocessed-bindings)]
    `(defmacro ~nm
       ~(unless nil? opt-doc-string
                (str "Creates a new '" nm "' type of hash-map, which takes :key value argumetns, some of which are required (in the :arglists metadata, denoted by REQ) and others of which are optional, with the default value given in the :arglists metadata."))
       ~(merge {:transients transients
                :arglists argmap}
               opt-attr-map)
       ~'[& args]
       `(let [~'~'processed-bindings-m (reflexive ~(nested-merge '~preprocessed-bindings-m
                                                                 ~'(apply hash-map args)))]
          (merge-meta> ~'~'processed-bindings-m
                       {:type (keyword '~'~nm)
                        :base-values ~'(~derive-base-values-map ~'processed-bindings-m
                                                                ~transients)})))))

(defn transients-to-base-values
  "Given a object made using 'def-new', resets the :transient values (which are refs) to the base values when the object was first instantiated."
  [obj]
  (dosync
    (doseq [[kseq v] (:base-values (meta obj))]
      (ref-set (get-in obj kseq) v))))

#_(def-new test-thing
         ^:required [:example-forced-req]
         [:example_expr '()]
         ^:hidden [:example-forced-hid #()]
         ^:hidden [:example-ind-req]
         [:example-default 5]
         ^:transient [:example-transient (ref (str "bloo" (this :example-ind-req)))]
         [:example-ind-hid (inc (this :example-default))]
         ^{:assertions '(integer? (< (this :example-default)))} [:example-assert-int 4]
         [:example-nested [^:required [:thing1]
                           [:thing2 '()]
                           ^:hidden [:thing3 #()]
                           [:thing4]
                           [:thing5 20]
                           ^:transient [:thing-trans (ref "GOGGLES")]
                           ^:transient [:thing-trans2 (ref "boonga")]
                           [:thing6 (inc (-> this :example-nested :thing5))]
                           ^{:assertions '(integer? pos?)} [:thing7 1]
                           ^{:assertions '(string?)} [:thing8 "dubble-da"]]])

#_(def a (test-thing
         :example-forced-req "yay"
         :example-ind-req "abc"
         :example-assert-int 3
         :example-nested {:thing1 "yoyo"
                          :thing4 "me"
                          :thing7 18}))

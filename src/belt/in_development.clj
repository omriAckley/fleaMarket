(ns in-development)

(comment *THINGS TO CHANGE ABOUT def-new*
         -Transients should not have to be refs, but rather should consider any first-level refs within them to be transient (maybe).
         -Assertions should be able to handle 'this' syntax (in a more elegant manner)
         -Assertion testing sometimes still fails with "object embedded in code" problems
         -Metadata :arglists should sort to put required args first, including args that have nested requires within them
         -Fix it so that throws (for :required issues) tell you more than local information (i.e. if it says that :x is required, also say what :x is a sub-argument of--if anything))
(defmacro def-new
  "Defines a new type of hash-map that takes :key value argument style, with some arguments required and others optional--in which case they must have a default value. Additionally, transient arguments (tagged with ^:transient metadata), must be refs, and can be reset to their base values by calling 'transients-to-base-values'. Finally, :assertions metadata (a list of criteria) is used to throw exceptions if any arguments are passed outside of what should be allowed."
  {:arglists '([name doc-string? attr-map? [key value?]*])}
  [nm & args]
  (let [opt-doc-string (unless (complement string?) (first args))
        opt-attr-map (unless (complement map?) ((if opt-doc-string
                                                  second
                                                  first) args))
        bindings (drop (+ (if opt-doc-string 1 0)
                          (if opt-attr-map 1 0))
                       args)
        vec-of-count-2? (every-pred vector? (count=? 2))
        vec-of-only-vecs-of-count-2? (fn [obj]
                                       (and (vector? obj)
                                            (not (empty? obj))
                                            (every? vec-of-count-2?
                                                    obj)))
        modified-bindings (postwalk-with-meta (fn [form]
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
                                              (vec bindings))
        assertions (into {}
                         (postwalk-with-meta (fn [form]
                                               (letfn [(filter-assertions
                                                         [x]
                                                         (filter #(:assertions (meta %))
                                                                 x))
                                                       (make-assertions-key-seqs
                                                         [x]
                                                         (if (empty? x)
                                                           x
                                                           (merge-meta>
                                                             (vec
                                                               (mapcat (fn [[k v :as kv]]
                                                                         (if (:assertions-key-seq (meta v))
                                                                           (vec
                                                                             (map
                                                                               (fn [[sub-ks assertions]]
                                                                                 [(vec (cons k sub-ks))
                                                                                  assertions])
                                                                               v))
                                                                           [[[k] (:assertions (meta kv))]]))
                                                                       x))
                                                             {:assertions-key-seq true})))
                                                       (make-assertions
                                                         [x]
                                                         (merge-meta> x
                                                                      {:assertions true}))]
                                                 (cond
                                                   (vec-of-only-vecs-of-count-2?
                                                     form) (-> form
                                                               filter-assertions
                                                               make-assertions-key-seqs)
                                                   (and (vec-of-count-2? form)
                                                        (:assertions-key-seq
                                                          (meta (second form)))) (make-assertions form)
                                                   :else form)))
                                             modified-bindings))
        test-assertions (fn [return-bindings-map assertions-map]
                          (doseq [[ks assertions-list] assertions-map]
                            (let [test-value (if (= '@% (first assertions-list))
                                               (deref (get-in return-bindings-map ks))
                                               (get-in return-bindings-map ks))]
                              (doseq [assertion (unless-then-fn (fn [coll] (= '@% (first coll)))
                                                                assertions-list
                                                                rest)]
                                (let [message (str "<def-new assertion exception>"
                                                   "\nFailed assertion: " (unless-then-fn string? assertion
                                                                                          str-to-literal-str)
                                                   "\nFor value: " (unless-then-fn string? test-value
                                                                                   str-to-literal-str)
                                                   "\nAt: " (list 'get-in 
                                                                  'this
                                                                  ks))]
                                  (if (seq? assertion)
                                    (when-not ((eval `(fn ~'[x] (def-new-assertion-seq-mac
                                                                  ~'x
                                                                  ~assertion
                                                                  ~test-value)))
                                                     return-bindings-map)
                                      (throw (Exception. message)))
                                    (when-not (eval (list assertion test-value))
                                      (throw (Exception. message)))))))))
        transients (postwalk-with-meta (fn [form]
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
                                       modified-bindings)
        make-base-values-map (fn [return-bindings-map transients-list]
                               (into {}
                                     (map (fn [ks]
                                            [ks (let [v (get-in return-bindings-map ks)]
                                                  (if (ref? v)
                                                    (deref v)
                                                    (throw (Exception.
                                                             (str "<def-new transient exception> Value passed to a transient must be a ref."
                                                                  "\nFailed for value: " (unless-then-fn string? v
                                                                                                         str-to-literal-str)
                                                                  "\nAt: " (list 'get-in 'this ks))))))])
                                          transients-list)))
        toMap-able? vec-of-only-vecs-of-count-2?
        all-toMap-ables-into-maps (fn [form] (all-into {} form toMap-able?))
        argmap (all-toMap-ables-into-maps
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
                                     modified-bindings))
        resultant-bindings-m (all-toMap-ables-into-maps
                               modified-bindings)]
    `(defmacro ~nm
       ~(unless nil? opt-doc-string
                (str "Creates a new '" nm "' type of hash-map, which takes :key value argumetns, some of which are required (in the :arglists metadata, denoted by REQ) and others of which are optional, with the default value given in the :arglists metadata."))
       ~(merge {:assertions assertions
                :transients transients
                :arglists (sort-hash-map-nested-by
                            (fn [a b] (> (count-all-by #{'REQ} a)
                                         (count-all-by #{'REQ} b)))
                            argmap)}
               opt-attr-map)
       ~'[& args]
       `(let [~'~'return-bindings-m (reflexive ~(nested-merge '~resultant-bindings-m
                                                              ~'(apply hash-map args)))]
          ~'(~test-assertions ~'return-bindings-m
                              ~assertions)
          (merge-meta> ~'~'return-bindings-m
                       {:type (keyword '~'~nm)
                        :base-values ~'(~make-base-values-map ~'return-bindings-m
                                                              ~transients)})))))

(defn transients-to-base-values
  "Given a object made using 'def-new', resets the :transient values (which are refs) to the base values when the object was first instantiated."
  [obj]
  (dosync
    (doseq [[kseq v] (:base-values (meta obj))]
      (ref-set (get-in obj kseq) v))))

;Macro that deals with 'this' syntax
(defmacro deal-with-this
  [real-this expr]
  `(postwalk-replace {~''this ~real-this} ~expr))

(defmacro with-this
  [real-this & exprs]
  `(let [~'this ~real-this]
     ~@exprs))

(defmacro with-eval-args
  [macro-name & exprs]
  `(~macro-name ~@exprs))

(defmacro def-new-assertion-seq-mac
  "Used in def-new to determine whether a given assertion sequence evaluates to true or false."
  [return-bindings-map assertion test-value]
  (let [assertion-seq (postwalk-replace {'this return-bindings-map}
                                        assertion)]
    (list* (first assertion-seq)
           test-value
           (rest assertion-seq))))

(comment *ALTERNATIVE TO CURRENT test-assertions in def-new* WHY DOESN'T THIS WORK?!!
         (defn test-assertions
           [return-bindings-map assertions-map]
           (doseq [[ks assertions-list] assertions-map]
             (let [test-value (get-in return-bindings-map ks)]
               (doseq [assertion assertions-list]
                 (let [message (str "<def-new assertion exception>"
                                    "\nFailed assertion: " (unless-then-fn string? assertion
                                                                           str-to-literal-str)
                                    "\nFor value: " (unless-then-fn string? test-value
                                                                    str-to-literal-str)
                                    "\nAt: " (list 'get-in 
                                                   'this
                                                   ks))]
                   (if (seq? assertion)
                     (let [assertion-seq (postwalk-replace {'this return-bindings-map}
                                                           assertion)]
                       (when-not (eval `(~(first assertion-seq)
                                                 ~test-value
                                                 ~@(rest assertion-seq))))
                       (throw (Exception. message)))
                     (when-not (eval (list assertion test-value))
                       (throw (Exception. message))))))))))

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

(comment *EXAMPLES for def-new*
         (def-new test-thing
                  ^:required [:example-forced-req]
                  [:example_expr '()]
                  ^:hidden [:example-forced-hid #()]
                  ^:hidden [:example-ind-req]
                  [:example-default 5]
                  ^:transient [:example-transient (ref (str "bloo" (this :example-ind-req)))]
                  [:example-ind-hid (inc (this :example-default))]
                  ^{:assertions '(integer? (< 200))} [:example-assert-int 100]
                  [:example-nested [^:required [:thing1]
                                    [:thing2 '()]
                                    ^:hidden [:thing3 #()]
                                    [:thing4]
                                    [:thing5 20]
                                    ^:transient [:thing-trans (ref "GOGGLES")]
                                    ^:transient [:thing-trans2 (ref "boonga")]
                                    [:thing6 (inc (-> this :example-nested :thing5))]
                                    ^{:assertions '(pos? integer?)} [:thing7 1]
                                    ^{:assertions '(string?)} [:thing8 "dubble-da"]]])
         (def a (test-thing
                  :example-forced-req "yay"
                  :example-ind-req "abc"
                  :example-assert-int 100
                  :example-nested {:thing1 "yoyo"
                                   :thing4 "me"
                                   :thing7 18})))
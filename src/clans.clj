(ns clans
  (:use utils
        fleas
        [belt.general-utils :only [reflexive]]
        belt.jacked-def
        [belt.hash-maps :only [update-all]]
        [belt.combinations :only [all-unique-pairs]]))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Clan stuff

(def-expression-type
  generate-DVE_expr
  [accessible-fleas]
  [*accessible-fleas* accessible-fleas
   *all-determine-value-exprs* (map :determine-value_expr
                                    *accessible-fleas*)])

(def-new clan
         [:size]
         [:max-give-rate 0.5]
         [:generate-DVE_expr]
         [:generate-DVE (eval
                          `(wrapper_generate-DVE_expr
                             ~(this :generate-DVE_expr)))]
         ^:hidden [:age (ref 0)]
         ^:hidden [:fleas-name-counter (ref 0)]
         ^:hidden [:fleas (ref {})]
         ^:transient [:accessible-fleas (ref {})]
         ^:transient [:death-count (ref 0)])

(defn generate-DVE
  [clan]
  ((clan :generate-DVE)
       (map get-flea @(clan :accessible-fleas))))

(defn new-flea
  [clan]
  [(-> clan :fleas-name-counter (alter inc))
   (reflexive
     {:determine-value_expr (generate-DVE clan)
      :determine-value (eval `(wrapper_determine-value_expr ~(this :determine-value_expr)))
      :age (ref 0)
      :holdings (merge (update-all (select-keys (*market* :flea-holdings)
                                                (*market* :essential-goods))
                                   #(ref (:starting %)))
                       (if (zero? @(clan :age))
                         (update-all (select-keys (*market* :flea-holdings)
                                                  (*market* :non-essential-goods))
                                     #(ref (:starting %)))
                         (zipmap (*market* :non-essential-goods)
                                 (repeatedly #(ref 0)))))
      :information (zipmap (all-unique-pairs (*market* :all-goods))
                           (repeatedly #(ref {})))
      :relative-values (zipmap (*market* :all-goods)
                               (repeatedly #(ref 1)))})])

(defn repopulate
  [clan]
  (dotimes [_ (- (clan :size)
                 (count @(clan :fleas)))]
    (dosync
      (alter (clan :fleas)
             merge
             (apply hash-map
                    (new-flea clan))))))

(defn consume-all
  [clan]
  (dosync
    (doseq [[flea-name flea] @(clan :fleas)]
      (consume flea))))

(defn remove-and-repossess
  [clan-name]
  (let [clan (get-clan clan-name)]
    (doseq [[flea-name flea] @(clan :fleas)]
      (when (dead? flea)
        (dosync
          (doseq [good (*market* :non-essential-goods)]
            (alter (*market* :repossessed)
                   update-in [clan-name good]
                   + @(-> flea :holdings good)))
          (alter (clan :death-count)
                 inc)
          (alter (clan :fleas)
                 dissoc flea-name))))))

(defn equity
  "Given a population size and an amount to be split without fractions, returns a shuffled list of integer amounts to distribute to each member. Namely, it returns a list of dimension pop-size, in which each element is either (quot amount pop-size) or (inc (quot amount pop-size)), and the sum of all the elements is equal to amount."
  [pop-size amount]
  (let [[remainder
         quotient] ((juxt rem quot) amount pop-size)]
    (shuffle
      (concat
        (repeat remainder (inc quotient))
        (repeat (- pop-size remainder) quotient)))))

(defn distribute-repossessed
  [clan-name]
  (let [clan (get-clan clan-name)
        repossessed (-> *market* :repossessed deref clan-name)]
    (if-not (every? zero? (vals repossessed))
      (let [equity-map (update-all repossessed
                                   (fn [good-amount]
                                     (equity (count @(clan :fleas))
                                             good-amount)))]
        (doseq [good (*market* :all-goods)
                [flea
                 amount] (map vector
                              (vals @(clan :fleas))
                              (equity-map good))]
          (dosync
            (alter (-> flea :holdings good)
                   + amount)
            (alter (*market* :repossessed)
                   update-in [clan-name good]
                   - amount)))))))

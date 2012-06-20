(ns fleas
  (:use utils
        belt.jacked-def
        [belt.hash-maps :only [update-all]]))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Flea stuff

(def-expression-type
  determine-value_expr
  [self good]
  [*good* good
   *information* (self :information)
   *holdings* (self :holdings)
   *relative-values* (self :relative-values)
   *good-info-fn* (fn [g2]
                    (get-info *information*
                              [*good* g2]))
   *good-holding* @(*holdings* *good*)
   *good-rel-val-fn* (fn [g2]
                       (apply pd
                              (map
                                #(deref (*relative-values* %))
                                [*good* g2])))])

(def demo-DVE '(pd 100 *good-holding*))

(defn determine-relative-values
  [flea]
  (let [all-goods (*market* :all-goods)
        determine-value (flea :determine-value)]
    (doseq [good all-goods]
      (ref-set (-> flea :relative-values good)
              (determine-value flea good)))))

(defn consume
  [flea]
  (doseq [[good
           use-rate] (update-all (select-keys (*market* :flea-holdings)
                                              (*market* :essential-goods))
                                 :use-rate)]
    (alter (-> flea :holdings good)
           + use-rate)))

(defn dead?
  [flea]
  (let [death-below-map
        (update-all (select-keys (*market* :flea-holdings)
                                 (*market* :essential-goods))
                    :death-below)]
    (boolean
      (some true?
            (map (fn [[good death-below]]
                   (< @(good (flea :holdings))
                      death-below))
                 death-below-map)))))

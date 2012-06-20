(ns markets
  (:use utils
        clans
        trading
        belt.jacked-def
        [belt.collections :only [most]]
        [belt.hash-maps :only [remove-hash-map]]))


(memfn remove-hash-map)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Market stuff

(def-new market
         [:clans]
         [:clan-names (map (fn [c-name num]
                             (if (symbol? c-name)
                               c-name
                               num))
                           (quote (this :clans))
                           (iterate inc 1))]
         [:clan-map (zipmap (this :clan-names)
                            (this :clans))]
         [:reproduction-manager standard-reproduction-manager]
         [:trade-group-manager standard-trade-group-manager]
         [:redistribution-manager standard-redistribution-manager]
         [:flea-holdings [[:food [[:starting 30]
                                  [:use-rate -2]
                                  [:death-below 1]]]
                          [:water [[:starting 30]
                                   [:use-rate -1]
                                   [:death-below 1]]]
                          [:currency [[:starting 100]
                                      [:use-rate 0]]]
                          [:novelty-clocks [[:starting 5]
                                            [:use-rate 0]]]
                          [:silverware [[:starting 20]
                                        [:use-rate 0]]]
                          [:pogs [[:starting 200]
                                  [:use-rate 0]]]]]
         [:all-goods (-> this :flea-holdings keys set)]
         [:essential-goods (->> this :flea-holdings
                                (remove-hash-map (complement :death-below))
                                keys
                                set)]
         [:non-essential-goods (->> this :all-goods
                                    (remove (this :essential-goods))
                                    set)]
         ^:hidden [:age (ref 0)]
         ^:transient [:trade-groups (ref ())]
         ^:transient [:repossessed (ref (zipmap (this :clan-names)
                                                (repeat (zipmap (this :non-essential-goods)
                                                                (repeat 0)))))])

(defn determine-all-trade-groups
  []
  (dosync
    (ref-set (*market* :trade-groups)
             ((*market* :trade-group-manager)))))

(defn do-all-trades
  []
  (doseq [trade-group @(*market* :trade-groups)]
    (trade trade-group)))

(defn do-all-consumptions
  []
  (doseq [clan (*market* :clans)]
    (consume-all clan)))

(defn do-all-deaths-and-repossessions
  []
  (doseq [clan-name (*market* :clan-names)]
    (remove-and-repossess clan-name)))

(defn inc-all-ages
  []
  (dosync
    (alter (*market* :age) inc)
    (doseq [clan (*market* :clans)]
      (alter (clan :age) inc)
      (doseq [[flea-name flea] @(clan :fleas)]
        (alter (flea :age) inc)))))

(defn determine-all-accessible-fleas
  []
  (dosync
    (doseq [clan-name (*market* :clan-names)]
      (ref-set ((get-clan clan-name) :accessible-fleas)
               ((*market* :reproduction-manager) clan-name)))))

(defn do-all-repopulations
  []
  (doseq [clan (*market* :clans)]
    (repopulate clan)))

(defn do-all-redistributions
  []
  (dosync
    (alter (*market* :repossessed)
           (*market* :redistribution-manager)))
  (dosync
    (doseq [clan-name (*market* :clan-names)]
      (distribute-repossessed clan-name))))

(defn reset-all-transients
  []
  (dosync
    (transients-to-base-values *market*)
    (doseq [clan (*market* :clans)]
      (transients-to-base-values clan))))

(ns utils
  (:use [belt.hash-maps :only [update
                               update-all]]
        [belt.math :only [pd]]))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Dynamic declarations

(declare ^:dynamic *market*)





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;General utils

(defn get-clan
  [clan-name]
  (-> *market* :clan-map (get clan-name)))

(defn get-flea
  [[clan-name flea-name]]
  (get @(-> clan-name get-clan :fleas) flea-name))

(defn get-info
  ([flea two-goods]
    (let [info-m (flea :information)]
      (if (contains? info-m two-goods)
        @(get info-m two-goods)
        (update-all @(get info-m (reverse two-goods))
                    (fn [value]
                      (map #(pd 1 %) value))))))
  ([flea two-goods time]
    (get time (get-info flea two-goods))))

(defn set-info
  [flea two-goods trade-rate]
  (let [info-m (flea :information)
        age @(flea :age)]
    (if (contains? info-m two-goods)
      (alter (get info-m two-goods)
             update age #(cons trade-rate %))
      (alter (get info-m (reverse two-goods))
             update age #(cons (pd 1 trade-rate) %)))))

(defn get-holding
  [flea good]
  (-> flea
      :holdings
      good
      deref))

(defn get-proposal
  [flea good-1 good-2]
  (->> flea
       :relative-values
       ((juxt good-1 good-2))
       (map deref)
       (apply pd)))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Presets

(defn standard-redistribution-manager
  [current-repossessed]
  current-repossessed)

(defn standard-trade-group-manager
  []
  (if (-> @(*market* :age)
          (mod 10)
          zero?)
    (partition 2
               (shuffle (mapcat (fn [clan-name]
                                  (->> (get-clan clan-name)
                                       :fleas
                                       deref
                                       keys
                                       (map list (repeat clan-name))))
                                (*market* :clan-names))))
    (mapcat (fn [clan-name]
              (->> (get-clan clan-name)
                   :fleas
                   deref
                   keys
                   shuffle
                   (map list (repeat clan-name))
                   (partition 2)))
            (*market* :clan-names))))

(defn standard-reproduction-manager
  [clan-name]
  (map list
       (repeat clan-name)
       (keys @((get-clan clan-name) :fleas))))

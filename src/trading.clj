(ns trading
  (:use utils
        [fleas :only [determine-relative-values]]
        [belt.general-utils :only [constrain
                                   unless]]
        [belt.combinations :only [all-unique-pairs]]
        [belt.collections :only [most]]
        [belt.math :only [pd
                          geometric-mean]]))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Trading

(defn get-max-give-rate
  [flea-id]
  (-> (first flea-id)
      get-clan
      :max-give-rate))

(defn trade-utility
  [[flea-A-id flea-B-id
    good-1 good-2]]
  ;Originally, goods are unordered, do not correspond to either flea
  (determine-relative-values (get-flea flea-A-id))
  (determine-relative-values (get-flea flea-B-id))
  (let [flea-A (get-flea flea-A-id)
        flea-B (get-flea flea-B-id)
        [good-A
         good-B] (if (< (get-proposal flea-A
                                      good-2
                                      good-1)
                        (get-proposal flea-B
                                      good-2
                                      good-1))
                   [good-1 good-2]
                   [good-2 good-1])
        ;Now, goods are ordered, good-A being what flea-A will give, and good-B being what flea-B will give
        flea-A-proposed-ratio (get-proposal flea-A
                                            good-B
                                            good-A)
        flea-B-proposed-ratio (get-proposal flea-B
                                            good-B
                                            good-A)
        ideal-ratio (geometric-mean [flea-A-proposed-ratio
                                     flea-B-proposed-ratio])
        actual-ratio (constrain ideal-ratio
                                (* (pd 1 (get-holding flea-B good-B))
                                   (get-max-give-rate flea-A-id))
                                (* (get-holding flea-A good-A)
                                   (get-max-give-rate flea-B-id)))
        flea-A-utility (pd actual-ratio
                           flea-A-proposed-ratio)
        flea-B-utility (pd flea-B-proposed-ratio
                           actual-ratio)]
    (min flea-A-utility flea-B-utility)))

(defn choose-trade-group-and-goods
  [flea-ids]
  (let [flea-id-pairs (all-unique-pairs flea-ids)
        good-pairs (all-unique-pairs (*market* :all-goods))
        trade-possibilities (mapcat (fn [flea-id-pair]
                                      (map (fn [good-pair]
                                             (concat flea-id-pair good-pair))
                                           good-pairs))
                                    flea-id-pairs)]
    (most trade-utility
          trade-possibilities)))

(defn transaction
  [flea-A-id flea-B-id
   good-A good-B
   trade-rate]
  (let [flea-A (get-flea flea-A-id)
        flea-B (get-flea flea-B-id)
        flea-A-amount (min (* (get-max-give-rate flea-A-id)
                              @(-> flea-A-id get-flea :holdings good-A))
                           (* (get-max-give-rate flea-B-id)
                              @(-> flea-B-id get-flea :holdings good-B)
                              trade-rate))
        flea-B-amount (pd flea-A-amount trade-rate)
        [flea-A-amount_int
         flea-B-amount_int] (unless #(some zero? %)
                                    [(Math/round (float flea-A-amount))
                                     (Math/round (float flea-B-amount))]
                                    [0 0])]
    (alter (-> flea-A :holdings good-A)
           - flea-A-amount_int)
    (alter (-> flea-B :holdings good-A)
           + flea-A-amount_int)
    (alter (-> flea-A :holdings good-B)
           + flea-B-amount_int)
    (alter (-> flea-B :holdings good-B)
           - flea-B-amount_int)
    (set-info flea-A
              [good-A good-B]
              trade-rate)
    (set-info flea-B
              [good-A good-B]
              trade-rate)))

(defn trade
  [trade-group]
  (dosync
    (let [flea-ids trade-group
          [flea-A-id flea-B-id
           good-A good-B] (choose-trade-group-and-goods
                            flea-ids)
          flea-A (get-flea flea-A-id)
          flea-B (get-flea flea-B-id)
          flea-A-proposed-ratio (get-proposal flea-A
                                              good-B
                                              good-A)
          flea-B-proposed-ratio (get-proposal flea-B
                                              good-B
                                              good-A)
          ideal-ratio (geometric-mean [flea-A-proposed-ratio
                                       flea-B-proposed-ratio])
          actual-ratio (constrain ideal-ratio
                                  (pd 1 (get-holding flea-B good-B))
                                  (get-holding flea-A good-A))]
      (transaction flea-A-id flea-B-id
                   good-A good-B
                   actual-ratio))))

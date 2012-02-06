(ns simulator
  (:use
    [utils :only (Map
                   Function
                   any?
                   abs
                   geometric-mean
                   inverse
                   pd
                   map-corresponding
                   update
                   nested-alter
                   nested-update
                   update-all
                   remove-hash-map
                   shuffle-hash-map
                   my-round
                   %->
                   delivered?)]))





;     *Notes*
; This is a market simulation in which the agents are called fleas.
;   It is constructed to be run from another namespace. As such, it 
;   would not work to simply evaluate the file and call (run). The 
;   bare minimum--to run a demo--you would have to first call 
;   (setup). Then you could call (run) and it would go through a 
;   demo run.
; The market goes through discrete time steps in which fleas trade,
;   consume essential goods, die off, are replaced; then the goods 
;   are taken from those that died off and distributed equitably 
;   amongst the living. Each of these time steps I'll call a round.
; Every round, trades occur between randomly chosen pairs of fleas. 
;   Every flea participates in exactly one trade with one other 
;   flea. Therefore, the population size must always be even. The 
;   goods and amounts are determined by the fleas in the following 
;   way...
; Each flea has a 'relative value' for each of the goods. Each 
;   relative value is determined by the flea's 'determine-relative-
;   value' expression. These relative values are numbers whose 
;   absolute value is arbitrary--the relative values are given 
;   meaning by comparing them to other relative values within the 
;   same flea. So based on these relative values, each flea will 
;   have a trading ratio for any two goods (which can be understood 
;   as "relative value of a divided by relative value of b").
; The goods to be traded are selected based on the "most different" 
;   ratio between the two fleas. Here, "most different" is division 
;   based. If flea A would trade pogs for currency at a rate of 4 
;   to 1, and flea B would trade currency for pogs at a rate of 1 
;   to 4, then they value pogs and currency equally. If, however, 
;   flea B would trade currency for pogs at a rate of 1 to 8, then 
;   flea B values currency twice as much (relatively) as flea A, 
;   and flea A values pogs twice as much (relatively) as flea B. 
;   This number (2 for "twice as much") is critical. The program 
;   does some math to maximize this number, and the pair of goods 
;   for which this number is maximized are the goods that will be 
;   traded.
; Once the goods are determined, then the ratio is determined. 
;   Using both fleas relative valuations for the goods, two ratios 
;   are derived from each flea--essentially, this is a "proposed 
;   ratio". Then, the program takes the geometric mean of these 
;   proposed ratios in order to determine the best "middle ground". 
;   In other words, both fleas will get an "n times better deal" 
;   than their respective proposed ratios. For example, if flea A 
;   is willing to trade 1 pog for 2 currency and flea B is willing 
;   to trade 8 currency for 1 pog, the trading ratio will be 1:4  
;   for pogs:currency. In this way, both fleas are getting "twice 
;   as good a deal"--i.e. flea A is receiving 4 currency for each 
;   pog it trades (twice as many as its own proposed ratio), and 
;   flea B is only giving 4 currency for each pog it trades (half 
;   as much as it would give based on its own proposed ratio).
; So the transaction takes place, and now both fleas have different 
;   holdings. Also, every fleas has an information hash-maps which 
;   keeps track of the trade ratios for every trade that flea has 
;   been a part of. The thought is that this information will be 
;   useful for the fleas to use in their strategy. Anyways, after 
;   every trade, this information hash-map is updated with a) the 
;   goods traded, and b) the ratio of the trade.
; After this, all fleas consume a specified amount of each 
;   essential good, and any flea that drops below a certain minimum 
;   "required for life" is removed from the market (i.e. it dies). 
;   Every dead flea is replaced by a new flea, so the total 
;   population size never changes. New fleas have a default starting 
;   amount of essential goods, but have no other goods (luxury or 
;   currency)--this is so as not to add non-essential goods to the 
;   market as a whole. The non-essential holdings of the dead fleas 
;   are distributed equitably amongst the current living, including 
;   the newly hatched. As such, the total amount of each non-
;   essential good in the market does not change.
; At the end of each round, "stats" are printed, then the simulation 
;   recurs upon itself with the new market that has been created
;   from the old one by what happened during the round. This will 
;   continue until the time step reaches the specified duration of 
;   the run. At the end of a run, the atom *final-market* is set to 
;   the last market state.










;;----------------------------------------------------------------
;;SETTINGS AND SUCH
;;----------------------------------------------------------------

(def settings (promise))

(def demo-GDVE_expr
  ''(pd 100 (*holdings* good)))

(def demo?
  (future (-> @settings
              :generate-DVE_expr
              (= demo-GDVE_expr))))

(def all-goods
  (future (->> @settings
               :flea
               :holdings
               vals
               (mapcat keys))))

(def essential-goods
  (future (-> @settings
              :flea
              :holdings
              :essential
              keys)))

(def luxury-goods
  (future (-> @settings
              :flea
              :holdings
              :luxury
              keys)))

(defn setup
  "Setup will deliver the promise to settings. It uses a default to do this, or, if passed key value pairs, will assoc into the default the new settings."
  [& settings-ksvs]
  (let [new-settings (apply hash-map settings-ksvs)
        default-settings {:generate-DVE_expr demo-GDVE_expr
                          :market {:starting {:world-time 0
                                              :name 0
                                              :fleas {}}
                                   :size 10
                                   :duration 50}
                          :flea {:holdings
                                 {:currency {:currency {:starting 100}}
                                  :essential {:food {:starting 80
                                                     :use-rate -2
                                                     :death-below 1}
                                              :water {:starting 80
                                                      :use-rate -1
                                                      :death-below 1}}
                                  :luxury {:silverware {:starting 20}
                                           :pogs {:starting 200}
                                           :novelty-clocks {:starting 5}}}}
                          :transaction {:good-max 0.5}
                          :output {:begin "Here it goes..."
                                   :end "...Done"
                                   :break "-----------------------"}}
        resultant-settings (nested-alter default-settings new-settings)]
    (deliver settings resultant-settings)))





;;----------------------------------------------------------------
;;EVOLUTION FUNCTIONS
;;----------------------------------------------------------------

(defn wrapper_generate-DVE
  "This is a wrapper used to convert an expression into a function that takes a market and returns the evaluated expression (i.e. it will return a determine value expression)."
  [generate-DVE_expr]
  (eval
    (list 'fn '[market]
          (list 'let '[*all-goods* '@all-goods
                       *world-time* (market :world-time)
                       *fleas-map* (dissoc (market :fleas)
                                           :information
                                           :relative-values)
                       *fleas-list* (vals *fleas-map*)
                       *all-determine-value-exprs* (map :determine-value_expr
                                                        *fleas-list*)]
                generate-DVE_expr))))

(def generate-DVE
  (future (-> @settings
              :generate-DVE_expr
              wrapper_generate-DVE)))

(defn wrapper_determine-value
  "This is a wrapper used to convert the expression stored in :determine-value_expr (for a flea) into a function that takes a self and a good. This function will return the evaluated expression (should return a number)."
  [determine-value_expr]
  (eval
    (list 'fn '[self good]
          (list 'let '[*good* good
                       *all-goods* @all-goods
                       *information* (self :information)
                       *holdings* (self :holdings)
                       *relative-values* (self :relative-values)
                       *good-info-fn* (fn [g2]
                                        (get-in *information*
                                                [*good* g2]))
                       *good-holding* (*holdings* *good*)
                       *good-rel-val-fn* (fn [g2]
                                           (apply pd
                                                  (map
                                                    *relative-values*
                                                    [*good* g2])))]
                determine-value_expr))))





;;----------------------------------------------------------------
;;SIMULATION FUNCTIONS
;;----------------------------------------------------------------

(defn new-flea
  "Given a market, returns a new market, with a new flea in it. The specifications for this flea are taken from 'settings'. Note that starting holdings are 0 (except for the first generation)."
  [market]
  (let [flea {:date-of-birth (market :world-time)
              :information (into {}
                                 (map (fn [good]
                                        [good (zipmap (remove #{good} @all-goods)
                                                      (repeat nil))])
                                      @all-goods))
              :holdings (apply assoc (zipmap @all-goods (repeat 0))
                               (->> @settings
                                    :flea
                                    :holdings
                                    :essential
                                    (mapcat (juxt key
                                                  #(:starting (val %))))))
              :determine-value_expr (@generate-DVE market)
              :relative-values (zipmap @all-goods (repeat nil))}]
    (%-> market
         (update % :name inc)
         (assoc-in % [:fleas (% :name)] flea))))

(defn determine-relative-values
  "Given a flea, returns a new flea, with :relative-values updated by calling that flea's determine-value_expr on each good."
  [flea]
  (let [determine-value (wrapper_determine-value
                          (flea :determine-value_expr))]
    (assoc flea :relative-values
           (reduce (fn [rel-vals good]
                     (assoc rel-vals
                            good
                            (abs (determine-value
                                   flea
                                   good))))
                   (flea :relative-values)
                   @all-goods))))

(defn transaction
  "Given the fleas, two flea names, and a world-time, enacts a transaction between the pair of fleas. The goods for the transaction are determined by comparing relative values between the two fleas, and selecting those goods for which the two fleas have the most dissimilar relative values. The trading ratio is then determined by taking the geometric mean of both fleas' proposed ratios (each proposed ratio is simply a ratio of the flea's relative values of p-good to r-good). Geometric mean is used here so that each flea gets a deal that is 'n times better' than its proposed ratio. For example, if one flea A's proposed ratio is 2:1 and flea B's proposed ratio is 8:1, then if the trading ratio is 4:1, both fleas are getting 'twice as good of a deal'. So once the goods and ratio have been established, the actual trading amounts are determined. In the 'setting' there is a percent max on how much of a good a flea is allowed to trade. The trade follows on the prerogative of whichever flea has a 'more limiting' absolute maximum. Both fleas' holdings are then updated (the trade 'takes place'). Finally, both fleas' information maps is updated."
  [fleas
   [proposer-name replier-name]
   world-time]
  (let [proposer (-> fleas 
                     (get proposer-name)
                     determine-relative-values)
        replier (-> fleas
                    (get replier-name)
                    determine-relative-values)
        p-rel-vals (proposer :relative-values)
        r-rel-vals (replier :relative-values)
        [p-good
         r-good](->> (map-corresponding pd r-rel-vals p-rel-vals)
                     (sort-by val)
                     ((juxt last first))
                     keys)
        p-ratio (pd (p-rel-vals r-good)
                    (p-rel-vals p-good))
        r-ratio (pd (r-rel-vals r-good)
                    (r-rel-vals p-good))
        ratio (geometric-mean [p-ratio
                               r-ratio])
        p-amount (* (-> @settings :transaction :good-max)
                    (min (-> proposer :holdings p-good)
                         (-> replier :holdings r-good (* ratio))))
        r-amount (pd p-amount ratio)
        p-amount_int (my-round p-amount)
        r-amount_int (my-round r-amount)]
    (nested-alter
      fleas
      {proposer-name {:holdings {p-good #(- % p-amount_int)
                                 r-good #(+ % r-amount_int)}
                      :information {p-good {r-good ratio}
                                    r-good {p-good (pd 1 ratio)}}}
       replier-name {:holdings {r-good #(- % r-amount_int)
                                p-good #(+ % p-amount_int)}
                     :information {p-good {r-good ratio}
                                   r-good {p-good (pd 1 ratio)}}}})))

(defn consume
  "Given a flea, returns a new flea with new values for essential goods. Essential goods are consumed by an amount specified in 'settings'."
  [flea]
  (let [consumption-list (->> @settings :flea :holdings :essential
                              (mapcat (juxt key #(->> % val :use-rate
                                                      (partial +)))))]
    (update flea
            :holdings (fn [holdings-map]
                        (apply update holdings-map
                               consumption-list)))))

(defn dead?
  "Given a flea, returns true if any of the flea's essential goods have gone below the minimum specified in 'settings'--otherwise returns false."
  [flea]
  (let [death-belows (->> @settings :flea :holdings :essential vals
                          (map :death-below))]
    (any? true?
          (map
            (fn [good death-below]
              (< (-> flea :holdings good) death-below))
            @essential-goods
            death-belows))))

(defn repossess-holdings
  "Repossesses the holdings of dead fleas by lumping the holdings together and assoc-ing them into :repossessed in the given market."
  [market]
  (let [fleas (:fleas market)
        the-dead (filter dead? (vals fleas))]
    (if (empty? the-dead) market
      (assoc market
             :repossessed (apply map-corresponding +
                                 (map #(apply dissoc
                                              (:holdings %)
                                              @essential-goods)
                                      the-dead))))))

(defn refill
  "Injects new fleas into a market until that market reaches a given size."
  [market size]
  (let [diff (- size
                (count (market :fleas)))]
    (if (zero? diff)
      market
      (nth
        (iterate 
          new-flea
          market)
        diff))))

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
  "Distributes to the current living fleas any goods that have been reposseseed from the dead."
  [market]
  (if-let [repossessed (market :repossessed)]
    (update market
            :fleas #(into {}
                          (reduce (fn [flea-map good]
                                    (map
                                      (fn [[_ flea] amount]
                                        [_ (update-in flea
                                                      [:holdings good]
                                                      + amount)])
                                      flea-map
                                      (equity (count (:fleas market))
                                              (repossessed good))))
                                  (vec %)
                                  (keys repossessed)))
            :repossessed (constantly nil))
    market))

(defn fitness-sort
  "Sorts a hash-map of fleas by :date-of-birth (the lower the dob, the better)."
  [fleas]
  (sort-by
    #(:date-of-birth (val %))
    fleas))

(defn print-stats
  "Given a market, prints the current time, the live rate (how many fleas survived the round), the age of the oldest flea, and the average age of all the fleas."
  [market size live-count time-remaining]
  (let [_world-time (:world-time market)
        time-print (str _world-time '/
                        (+ _world-time time-remaining))
        live-rate (str live-count '/ size)
        _fleas (:fleas market)
        _all-dobs (map #(:date-of-birth (val %)) _fleas)
        ave-age (float (- _world-time
                          (/ (reduce + _all-dobs)
                             (count _fleas))))
        _oldest-flea (-> _fleas fitness-sort first)
        age-of-oldest-flea (- _world-time
                              (-> _oldest-flea second :date-of-birth))
        oldest-flea-DVE (-> _oldest-flea second :determine-value_expr)]
    (println "MARKET...")
    (println "  World time: " time-print)
    (println "  Live rate:  " live-rate)
    (println "  Average age:" ave-age)
    (println "OLDEST FLEA...")
    (println "  Age:" age-of-oldest-flea)
    (println "  DVE:" oldest-flea-DVE)
    (println (-> @settings :output :break))))

(defn to-starting-holdings
  "Given a holdings map, returns a new holdings map with the starting holdings within it. Only called from 'holdings-setup'."
  [holdings-map]
  (reduce (fn [m [k v]] (assoc m k v))
          holdings-map
          (let [goods (-> @settings :flea :holdings
                          (select-keys [:currency :luxury])
                          vals)]
            (for [good (mapcat keys goods)]
              [good (->> goods
                         (keep good)
                         (apply :starting))]))))

(defn holdings-setup
  "Given a market, returns a new market in which all fleas have the default starting holdings. Only called once, by 'run' upon start-up."
  [market]
  (update market
          :fleas
          (fn [flea-map]
            (update-all flea-map
                        (fn [flea]
                          (update flea
                                  :holdings
                                  to-starting-holdings))))))

(defn error-fn
  "Given an error message, returns a fleaMarket-specific error."
  [error-message]
  (do
    (println "Flea Market Error:")
    error-message))

(defmacro error-cond
  "This macro is speifically for errors relevant to this flea market simulation. Expands to a (cond ... ) in which error-messages are thrown if any of the criteria are met. Given a :no-errors crieterion, will convert this into the :default of the (cond ... )--essentially, will return the result of the corresponding expression if there are no errors."
  [& args]
  (list*
    'cond
    (mapcat 
      (fn [[criterion return]]
        (if (= :no-errors criterion)
          [:default return]
          [criterion `(error-fn ~return)]))
      (partition-all 2 args))))

(defn simulation
  "The meat of the whole program. Given a market, a size, and a duration, recurs upon itself with a new market, the same size, and (dec duration)--until duration reaches 0. This new market is one which a round of trades have occured, fleas have consumed essential goods, dead fleas have been removed, the world time has incremented, and if any fleas have died, new fleas have been brought into the population."
  [market size duration]
  (if (zero? duration)
    market
    (let [fleas (market :fleas)
          random-pairs (->> (keys fleas)
                            shuffle
                            (partition 2))
          new-fleas (%-> fleas
                         (reduce (fn [fls pair]
                                   (transaction fls
                                                pair
                                                (market :world-time)))
                                 %
                                 random-pairs)
                         (update-all % consume))
          live-count (count (remove-hash-map dead? new-fleas))
          new-market (%-> market
                          (assoc % :fleas new-fleas)
                          (repossess-holdings %)
                          (update % :fleas
                                  (fn [fls]
                                    (remove-hash-map dead? fls)))
                          (update % :world-time inc)
                          (refill % size)
                          (distribute-repossessed %))]
      (print-stats new-market
                   size
                   live-count
                   (dec duration))
      (recur new-market
             size
             (dec duration)))))

(def starting-market (atom nil))

(def final-market (atom nil))

(defn _run
  "This is wrapper for the simulation. It takes keyword arguments (for :duration, :size, and :market), with defaults for each (taken from 'settings'). It also uses error-cond to only allow proper arguments to result in a simulation. Finally, it prints starting and ending messages for a simulation."
  [& settings-ksvs]
  (let [new-settings (apply hash-map settings-ksvs)
        default-settings {:duration (-> @settings :market :duration)
                          :size (-> @settings :market :size)
                          :market (-> @settings :market :starting)}
        resultant-settings (nested-alter default-settings new-settings)
        duration (:duration resultant-settings)
        size (:size resultant-settings)
        market (:market resultant-settings)
        start-market (-> market 
                         (refill (int size))
                         holdings-setup)]
    (error-cond
      (< duration 0) "Duration must be greater than or equal to 0."
      (< size 1) "Size must be greater than 0."
      (odd? size) "Size must be even."
      (not (start-market :world-time)) "Market must have :world-time entry."
      (not (start-market :name)) "Market must have :name entry."
      :no-errors (do
                   (println)
                   (reset! starting-market start-market)
                   (when @demo?
                     (println (-> @settings :output :break))
                     (println "**DEMO**")
                     (Thread/sleep 1000))
                   (println (-> @settings :output :break))
                   (println (-> @settings :output :begin))
                   (println (-> @settings :output :break))
                   (reset! final-market
                           (simulation start-market (int size) (int duration)))
                   (println (-> @settings :output :end))
                   (println (-> @settings :output :break))))))

(defn run
  "This is the wrapper for '_run'. It checks to make sure that all promises required for a simulation have been delivered. Otherwise throws an exception."
  [& args]
  (error-cond
    (not (delivered? settings)) "Settings not set. Deliver to settings either directly, or (suggested) by calling 'setup' with relevant arguments."
    :no-errors (apply _run args)))

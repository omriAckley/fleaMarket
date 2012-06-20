(ns core
  (:use utils
        [clans :only [clan]]
        markets
        [belt.math :only [pd]]
        [belt.hash-maps :only [remove-hash-map]]
        [belt.collections :only [most]]))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Example run. Evaluate the whole file, then uncomment and evaluate the following lines.






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Simulation stuff

(def output
  {:begin "Here it goes..."
   :end "...Done"
   :break "-----------------------"})

(defn print-stats
  [time-step duration]
  (println "Time step:" time-step "/" duration "...")
  (println "Market age:" @(*market* :age))
  (doseq [[clan-name clan] (*market* :clan-map)]
    (println "  Clan:" clan-name)
    (let [oldest-flea (most #(deref (:age %))
                                 (vals @(clan :fleas)))]
      (println "    Death count:" @(clan :death-count) "/" (clan :size))
      (println "    Oldest flea:" @(oldest-flea :age))
      (println "    Strategy:" (oldest-flea :determine-value_expr))))
  (println (output :break)))

(def initial-market (atom nil))
(def final-market (atom nil))

(defn run-simulation
  [duration market]
  (println)
  (println (output :break))
  (println (output :begin))
  (println (output :break))
  (binding [*market* market]
    (reset! initial-market *market*)
    (do-all-repopulations)
    (doseq [time-step (range duration)]
      (determine-all-trade-groups)
      (do-all-trades)
      (do-all-consumptions)
      (do-all-deaths-and-repossessions)
      (inc-all-ages)
      (determine-all-accessible-fleas)
      (do-all-repopulations)
      (do-all-redistributions)
      (print-stats (inc time-step) duration)
      (reset-all-transients))
    (reset! final-market *market*))
  (println (output :end))
  (println (output :break)))

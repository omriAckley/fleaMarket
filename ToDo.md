*CHECKLIST*
  get-clan : YES
  get-flea : YES
  get-info : YES
  set-info : YES
  get-holding : YES
  get-proposal : YES
  determine-relative-values : NOT FOR ALL SPECIAL TERMS
  consume : YES
  dead? : YES
  trade-utility : YES (SEEMINGLY)
  choose-trade-groups : YES (SEEMINGLY)
  trade : YES (SEEMINGLY)
  transaction : YES (SEEMINGLY)
  new-clan : YES
  generate-DVE : NOT FOR ANY SPECIAL TERMS
  new-flea : YES (by association)
  repopulate : YES
  consume-all : YES
  remove-and-repossess : YES
  equity : YES
  distribute-repossessed : YES
  clan? : YES
  new-market : YES
  determine-all-trade-groups : YES (SEEMINGLY)
  do-all-trades : YES (SEEMINGLY)
  do-all-consumptions : YES
  do-all-deaths-and-repossessions : YES
  inc-all-ages : YES
  determine-all-accessible-fleas : YES
  do-all-repopulations : YES
  do-all-redistributions : YES
  reset-all-transients : YES
  print-stats : YES
  run-simulation : YES

*TO-DO*
  choose-trade-groups : choose trade groups based on actual proposed trade rates, not hypothetical
  new-market : instead of awkwardly requiring names with clans, require :clans to be a vector of symbols in which each evaluates to a clan
  reproduction-manager : use defn-new to create canned reproduction managers
  trade-group-manager : use defn-new to create canned trade group managers
  redistribution-manager : use defn-new to create canned redistribution managers
  DATA VISUALIZATION!
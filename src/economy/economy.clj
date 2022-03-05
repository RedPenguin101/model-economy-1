(ns economy.economy)

(defn produce [state]
  state)

(defn trade [state]
  state)

(defn consume [state]
  state)

(defn day [state]
  (-> state
      produce
      trade
      consume))

(day "state")
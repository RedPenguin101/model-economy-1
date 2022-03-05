(ns economy.economy)

(def resources {:mammoth {:id :mammoth :ease  4}
                :ketchup {:id :ketchup :ease  8}
                :lettuce {:id :lettuce :ease  6}
                :bread   {:id :bread   :ease  6}})

(def number-of-agents 10)

;; agent
{:id 0
 :inventory {:money 100 :mammoth 0 :ketchup 0 :lettuce 0 :bread 0}
 :production-efficiency {:mammoth 4 :ketchup 3 :lettuce 2 :bread 3}}

(defn roll-efficiency [resources]
  (into {} (for [r (vals resources)]
             [(:id r) (rand-int (:ease r))])))

(roll-efficiency resources)

(defn create-agent [id]
  [id {:id id
       :inventory (into {:money 1000} (map #(vector % 0) (keys resources)))
       :production-efficiency (roll-efficiency resources)}])

(defn init-state []
  {:agents (into {} (map create-agent (range number-of-agents)))
   :marketplace {:sell-orders []
                 :prices (into {} (for [r (keys resources)] [r 150]))}})

(defn agent-decide-production [agent prices]
  (let [values (merge-with * (:production-efficiency agent) prices)]
    (ffirst (sort-by val > values))))

(defn produce-resource [agent resource]
  (let [production (get-in agent [:production-efficiency resource])]
    (update-in agent [:inventory resource] + production)))

(defn produce [state]
  (reduce (fn produce-inner [state agent-id]
            (let [agent (get-in state [:agents agent-id])
                  prices (get-in state [:marketplace :prices])
                  resource (agent-decide-production agent prices)]
              (update-in state [:agents agent-id] produce-resource resource)))
          state
          (keys (:agents state))))

(defn trade [state]
  state)

(defn consume [state]
  state)

(defn day [state]
  (-> state
      produce
      trade
      consume))

(day (init-state))
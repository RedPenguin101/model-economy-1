(ns economy.economy
  (:require [economy.order-matching :as om]
            [economy.order-generate :as og]))

(def resources {:mammoth {:id :mammoth :ease [1 5] :initial-price 700}
                :ketchup {:id :ketchup :ease [3 10] :initial-price 100}
                :lettuce {:id :lettuce :ease [3 9] :initial-price 200}
                :bread   {:id :bread   :ease [2 8] :initial-price 200}})

(def empty-resources (zipmap (keys resources) (repeat 0)))

(defn roll-efficiency! [resources]
  (into {} (for [r (vals resources)]
             [(:id r) (+ (first (:ease r)) (rand-int (inc (abs (apply - (:ease r))))))])))

(defn create-agent [id]
  [id {:id id
       :inventory (into {:money 1000} (map #(vector % 0) (keys resources)))
       :production-efficiency (roll-efficiency! resources)}])

(defn init-state [num-agents]
  {:turn 0
   :agents (into {} (map create-agent (range num-agents)))
   :marketplace {:prices (into {} (for [r (keys resources)] [r (get-in resources [r :initial-price])]))}})

;; Production of resources
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn agent-decide-production
  "When given an agent object and the prices in the marketplace, returns the good
   which the agent decides to produce"
  [agent prices]
  (let [values (merge-with * (:production-efficiency agent) prices)]
    (ffirst (sort-by val > values))))

(defn produce-resource
  "Updates an agent's inventory with a days production of the resource passed in"
  [agent resource]
  (let [production (get-in agent [:production-efficiency resource])]
    (update-in agent [:inventory resource] + production)))

(defn produce-phase
  [state]
  (reduce (fn produce-inner [state agent-id]
            (let [agent (get-in state [:agents agent-id])
                  prices (get-in state [:marketplace :prices])
                  resource (agent-decide-production agent prices)]
              (update-in state [:agents agent-id] produce-resource resource)))
          state
          (keys (:agents state))))

;; Price adjustment
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn surplus
  "Given the orders for a single resource, will return by how much demand outstrips
   supply. A positive number means there is a surplus, a negative means there is a shortfall"
  [orders]
  (reduce (fn [s [buy-sell _ _ qty]]
            ((if (= :buy buy-sell) - +) s qty))
          0 orders))

(defn price-adjust
  "Adjust prices based on unmatched trades. If there is a surplus, the price will be reduced,
   if there is a shortfall it will increase"
  [prices unmatched-orders]
  (merge prices (into {} (for [[resource orders] (group-by second unmatched-orders)]
                           [resource (* (if (pos? (surplus orders)) 0.9 1.1) (resource prices))]))))

;; Trading
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; trade is 4 tuple of buyer, seller, material, quantity
[0 1 :lettuce 2]



(defn settle-trade
  "Given a trade and the gamestate, will update the inventories of the two participants in the
   trade, including the cash settlement"
  [state [buyer-id seller-id resource quantity]]
  (let [cash (* quantity (get-in state [:marketplace :prices resource]))]
    (-> state
        (update-in [:agents buyer-id :inventory resource] + quantity)
        (update-in [:agents buyer-id :inventory :money] - cash)
        (update-in [:agents seller-id :inventory resource] - quantity)
        (update-in [:agents seller-id :inventory :money] + cash))))

(defn trade-phase
  "The trade phase will generate a list of orders that each of the agents wants to make.
   These orders will be matched off against eachother, and the agent's inventories and cash
   changed accordingly."
  [state]
  (let [orders (mapcat #(og/gen-ord state %) (keys (:agents state)))
        {:keys [trades unmatched]} (om/find-trades orders)
        new-prices (price-adjust (get-in state [:marketplace :prices]) unmatched)]
    (-> (reduce settle-trade state trades)
        (assoc-in [:marketplace :trades] trades)
        (assoc-in [:marketplace :prices] new-prices))))


;; Making and consuming burgers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn make-burgers
  "The Agent will figure out how many burgers they can makes based on their inventories.
   Making the burgers will use up the inventory. The number of burgers are kept track of in
   a list."
  [agent]
  (let [able-to-make (apply min (vals (dissoc (:inventory agent) :money)))]
    (-> agent
        (assoc :burgers able-to-make)
        (update :inventory merge empty-resources))))

(defn consume-phase [state]
  (update state :agents update-vals make-burgers))

;; Logging
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn total-burgers [state]
  (apply + (map :burgers (vals (:agents state)))))

(defn trade-summary [state]
  (reduce (fn [A [_ _ rsrs qty]]
            (update A rsrs (fnil + 0) qty))
          {} (get-in state [:marketplace :trades])))

(defn log-state [state]
  (-> state
      (update-in [:log :prices] conj (get-in state [:marketplace :prices]))
      (update-in [:log :burgers] conj (total-burgers state))
      (update-in [:log :trades] conj (trade-summary state))))


;; Orchestration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn day [state]
  (-> state
      produce-phase
      trade-phase
      consume-phase
      (update :turn inc)
      log-state))

;; Summarization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn mean [xs] (int (/ (apply + xs) (count xs))))

(defn summary-stats [xs]
  {:total (apply + xs)
   :max (apply max xs)
   :min (apply min xs)
   :mean (mean xs)})

(def demo (init-state 10))
(def data (:log (last (take 10 (iterate day demo)))))

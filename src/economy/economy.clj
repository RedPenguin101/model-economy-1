(ns economy.economy
  (:require [economy.order-matching :as om]))

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
  {:agents (into {} (map create-agent (range num-agents)))
   :marketplace {:orders []
                 :prices (into {} (for [r (keys resources)] [r (get-in resources [r :initial-price])]))}
   :trade-log {:mammoth []
               :ketchup []
               :lettuce []
               :bread   []}})

(init-state 1)

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

(def demo-state (produce-phase (init-state 10)))

demo-state

(defn target-burgers
  "Calculates the number of burgers the agent will aim to make
   Returns a 2tuple of number of burgers, surplus resources (i.e. 'sell orders')
   and the buy orders to get the necessary materials"
  [state agent-id]
  (let [inv (dissoc (get-in state [:agents agent-id :inventory]) :money)
        money (get-in state [:agents agent-id :inventory :money])
        prices (get-in state [:marketplace :prices])
        total-val (+ money (apply + (vals (merge-with * prices inv))))
        burger-price (apply + (vals prices))
        burgers (int (/ total-val burger-price))]
    (max 0 burgers)))

;; order is a 4-tuple of type, material, order-placer id and amount
[:sell :lettuce 0 2]

(defn generate-orders
  "Generates the orders an agent will place based on their target number of burgers and
   current resources"
  [agent target]
  (let [orders
        (keep (fn [[k v]] (when (not (zero? (- v target))) [(if (pos? (- v target)) :sell :buy) k (:id agent) (Math/abs (- v target))])) (dissoc (:inventory agent) :money))]

    orders))

(target-burgers demo-state 0)

(defn- gen-ord [state agent-id]
  (let [target (target-burgers state agent-id)]
    (generate-orders (get-in state [:agents agent-id]) target)))


(defn settle-trade [state [buyer-id seller-id resource quantity]]
  (let [cash (* quantity (get-in state [:marketplace :prices resource]))]
    (-> state
        (update-in [:agents buyer-id :inventory resource] + quantity)
        (update-in [:agents buyer-id :inventory :money] - cash)
        (update-in [:agents seller-id :inventory resource] - quantity)
        (update-in [:agents seller-id :inventory :money] + cash))))

(defn shortfall-surplus [orders]
  (reduce (fn [s [buy-sell _ _ qty]]
            ((if (= :buy buy-sell) + -) s qty))
          0 orders))

(defn price-adjust [prices unmatched-orders]
  (merge prices (into {} (for [[resource orders] (group-by second unmatched-orders)]
                           [resource (* (if (pos? (shortfall-surplus orders)) 1.1 0.9) (resource prices))]))))



;; trade is 4 tuple of buyer, seller, material, quantity
[0 1 :lettuce 2]
(defn log-trades [trade-log trades]
  (def debug trades)
  (merge-with conj
              trade-log
              (merge empty-resources (into {} (for [[k tds] (group-by #(nth % 2) trades)]
                                                [k (apply + (map last tds))])))))

debug

(defn trade-phase [state]
  (let [orders (mapcat #(gen-ord state %) (keys (:agents state)))
        {:keys [trades unmatched]} (om/find-trades orders)
        new-prices (price-adjust (get-in state [:marketplace :prices]) unmatched)]
    (-> (reduce settle-trade state trades)
        (assoc-in [:marketplace :prices] new-prices)
        (assoc :log {:orders orders :unmatched unmatched :trades trades})
        (update :trade-log log-trades trades))))

(defn make-burgers [agent]
  (let [able-to-make (apply min (vals (dissoc (:inventory agent) :money)))
        new-rec (update-vals (dissoc (:inventory agent) :money) #(- % able-to-make))]
    (-> agent
        (update :burgers conj able-to-make)
        (update :inventory merge new-rec))))

(make-burgers {:id 0,
               :inventory {:money 1300, :mammoth 3, :ketchup 2, :lettuce 2, :bread 2},
               :production-efficiency {:mammoth 3, :ketchup 7, :lettuce 2, :bread 4}})

(defn consume-phase [state]
  (update state :agents update-vals make-burgers))

(consume-phase {:agents {0 {:id 0,
                            :inventory {:money 1300, :mammoth 2, :ketchup 2, :lettuce 2, :bread 2},
                            :production-efficiency {:mammoth 3, :ketchup 7, :lettuce 2, :bread 4}}}})

(defn day [state]
  (-> state
      produce-phase
      trade-phase
      consume-phase))

(def demo2 (init-state 2))
(def demo4 (init-state 4))
(def demo (init-state 10))
(def demo100 (init-state 100))

(comment)
(do
  nil)

(def x (take 1000 (iterate day demo100)))
(dissoc (last x) :log)

(defn mean [xs] (int (/ (apply + xs) (count xs))))

(defn summary-stats [xs]
  {:total (apply + xs)
   :max (apply max xs)
   :min (apply min xs)
   :mean (mean xs)})

(map (comp summary-stats :burgers) (vals (:agents (last x))))
(get-in (last x) [:marketplace :prices])
(apply + (vals (get-in (last x) [:marketplace :prices])))
(update-vals (get-in (last x) [:trade-log]) summary-stats)
(take 5 (:agents (last x)))
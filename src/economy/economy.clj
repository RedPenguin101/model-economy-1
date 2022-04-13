(ns economy.economy
  (:require [economy.order-matching :as om]
            [economy.order-generate :as og]))

(def resources {:mammoth {:id :mammoth :ease 4 :initial-price 200}
                :ketchup {:id :ketchup :ease 8 :initial-price 200}
                :lettuce {:id :lettuce :ease 6 :initial-price 200}
                :bread   {:id :bread   :ease 6 :initial-price 200}})

(def empty-resources (zipmap (keys resources) (repeat 0)))

(defn roll-efficiency! [resources]
  (into {} (for [r (vals resources)]
             [(:id r) (rand-int (* 2 (:ease r)))])))

(defn create-agent [id]
  [id {:id id
       :money 1000
       :inventory (into {} (map #(vector % 0) (keys resources)))
       :production-efficiency (roll-efficiency! resources)}])

(roll-efficiency! resources)
(create-agent 0)

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

;; order is a 4-tuple of type, material, order-placer id and amount
[:sell :lettuce 0 2]

(defn surplus
  "Given the orders, will return a map of how much demand outstrips supply for each resource.
   A positive number means there is a surplus, a negative means there is a shortfall"
  [orders]
  (reduce (fn [s [buy-sell rsrs _ qty]]
            (update s rsrs (if (= buy-sell :buy) - +) qty))
          empty-resources
          orders))

(surplus [[:sell :lettuce 0 2] [:buy :mammoth 0 6]])


;; trade is 4 tuple of buyer, seller, material, quantity
[0 1 :lettuce 2]

(defn trade-volumes
  [trades]
  (reduce (fn [s [_ _ rsrs qty]]
            (update s rsrs + qty))
          empty-resources
          trades))

(defn price-adjust
  "Adjust prices based on unmatched trades. If there is a surplus, the price will be reduced,
   if there is a shortfall it will increase"
  [prices unmatched-orders]
  (merge-with * prices
              (update-vals (surplus unmatched-orders) #(if (pos? %) 0.99 1.01))))


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
        (update-in [:agents buyer-id :money] - cash)
        (update-in [:agents seller-id :inventory resource] - quantity)
        (update-in [:agents seller-id :money] + cash))))

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
        (assoc-in [:marketplace :surplus] (into {} (map (fn [[r ordrs]] [r (surplus ordrs)]) (group-by second unmatched))))
        (assoc-in [:marketplace :prices] new-prices))))


;; Making and consuming burgers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn make-burgers
  "The Agent will figure out how many burgers they can makes based on their inventories.
   Making the burgers will use up the inventory. The number of burgers are kept track of in
   a list."
  [agent]
  (let [burgers-to-make (apply min (vals (:inventory agent)))]
    (-> agent
        (assoc :burgers burgers-to-make)
        #_(update :inventory #(merge-with + %1 %2) (zipmap (keys resources) (repeat (- burgers-to-make))))
        (update :inventory merge empty-resources))))

(defn consume-phase [state]
  (update state :agents update-vals make-burgers))

;; Price fixing / modification
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn fix-prices [state]
  (if (< 250 (:turn state) 500)
    (assoc-in state [:marketplace :prices :mammoth] 200)
    state))

(defn elon-tusk-adjust [state]
  (if (= (:turn state) 750)
    (assoc-in state [:agents 0 :production-efficiency :ketchup] 100)
    state))

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
      (update-in [:log :trades] conj (trade-volumes (get-in state [:marketplace :trades])))
      (update-in [:log :surplus] conj (get-in state [:marketplace :surplus]))))


;; Orchestration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn day [state]
  (-> state
      produce-phase
      trade-phase
      consume-phase
      (update :turn inc)
      fix-prices
      elon-tusk-adjust
      log-state))

;; Summarization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn sum [xs] (apply + xs))
(defn mean [xs] (int (/ (sum xs) (count xs))))

(defn summary-stats [xs]
  {:total (apply + xs)
   :max (apply max xs)
   :min (apply min xs)
   :mean (mean xs)})

(comment
  "Oz"
  (def demo (init-state 40))
  (def data (:log (last (take 1000 (iterate day demo)))))

  (require '[oz.core :as oz])

  (oz/start-server!)

  (defn sliding-average [period]
    (fn [xs]
      (map mean (partition period 1 xs))))


  (defn maps->datapoints [to-take maps]
    (mapcat (fn [price-map]
              (for [[k v] (dissoc price-map :turn)]
                {:turn (:turn price-map)
                 :resource k
                 :value v}))
            (map #(assoc %1 :turn %2) (reverse (take to-take maps)) (range))))

  (def prices-stacked-bar
    {:data {:values (maps->datapoints 100 (:prices data))}
     :mark "bar"
     :encoding {:x {:field "turn"
                    :type "ordinal"}
                :y {:aggregate "sum"
                    :field "price"
                    :type "quantitative"}
                :color {:field "resource"
                        :type "nominal"}}})

  (let [data (:log (last (take 1000 (iterate day (init-state 40)))))


        production-line-plot
        (let [burgers (reverse (:burgers data))
              avg ((sliding-average 10) burgers)]
          {:data {:values (mapcat #(vector {:turn %3 :name :burgers :value %1}
                                           {:turn %3 :name :average :value %2}) burgers avg (range))}
           :encoding {:x {:field "turn" :type "quantitative"}
                      :y {:field "value" :type "quantitative"}
                      :color {:field "name" :type "nominal"}}
           :mark {:type "line" :strokeWidth 1}
           :width 1000
           :height 300})

        prices-line
        {:data {:values (maps->datapoints 1000 (:prices data))}
         :mark {:type "line" :strokeWidth 1}
         :encoding {:x {:field "turn"
                        :type "quantitative"}
                    :y {:field "value"
                        :type "quantitative"}
                    :color {:field "resource"
                            :type "nominal"}}
         :width 1000
         :height 300}

        surplus-line
        {:data {:values (maps->datapoints 1000 (:surplus data))}
         :mark {:type "line" :strokeWidth 1}
         :encoding {:x {:field "turn"
                        :type "quantitative"}
                    :y {:field "value"
                        :type "quantitative"}
                    :color {:field "resource"
                            :type "nominal"}}
         :width 1000
         :height 300}

        trades-line
        {:data {:values (maps->datapoints 1000 (:trades data))}
         :mark {:type "line" :strokeWidth 1}
         :encoding {:x {:field "turn"
                        :type "quantitative"}
                    :y {:field "value"
                        :type "quantitative"}
                    :color {:field "resource"
                            :type "nominal"}}
         :width 1000
         :height 300}]

    (oz/view! [:div
               [:h1 "Econ model summary"]
               [:h2 "Burgers Made"]
               [:div {:style {:display "flex" :flex-direction "row"}}
                [:vega-lite production-line-plot]]
               [:h2 "Prices"]
               [:div {:style {:display "flex" :flex-direction "row"}}
                [:vega-lite prices-line]]
               [:h2 "Market surplus/shortfall"]
               [:div {:style {:display "flex" :flex-direction "row"}}
                [:vega-lite surplus-line]]
               [:h2 "Trades"]
               [:div {:style {:display "flex" :flex-direction "row"}}
                [:vega-lite trades-line]]])))
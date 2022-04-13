(ns economy.order-generate)

(defn- target-burgers
  "Calculates the number of burgers the agent will aim to make"
  [state agent-id]
  (let [inv (get-in state [:agents agent-id :inventory])
        money (get-in state [:agents agent-id :money])
        prices (get-in state [:marketplace :prices])
        total-val (+ money (apply + (vals (merge-with * prices inv))))
        burger-price (apply + (vals prices))
        burgers (int (/ total-val burger-price))]
    (max 0 burgers)))

;; order is a 4-tuple of type, material, order-placer id and amount
[:sell :lettuce 0 2]

(defn- generate-orders
  "Generates the orders an agent will place based on their target number of burgers and
   current resources"
  [agent target]
  (keep (fn [[k v]] (when (not (zero? (- v target))) [(if (pos? (- v target)) :sell :buy) k (:id agent) (Math/abs (- v target))])) (dissoc (:inventory agent) :money)))

(defn gen-ord [state agent-id]
  (let [target (target-burgers state agent-id)]
    (generate-orders (get-in state [:agents agent-id]) target)))
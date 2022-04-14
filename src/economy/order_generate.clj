(ns economy.order-generate
  "The Order generate NS contains methods by which Agents choose what they want to buy and
   sell on the marketplace. This typically involves the agent setting some sort of goal and
   determining how much of each resource they will need to buy to accomplish that goal, and
   how much of their inventory is unecessary to that goal and can be sold")

;; An order is a 4-tuple of type, material, order-placer-id and quantity
[:sell :lettuce 0 2]

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


(defn- generate-orders-agent
  "Generates the orders an agent will place based on their target number of burgers and
   current resources"
  [agent target]
  (keep (fn [[k v]] (when (not (zero? (- v target))) [(if (pos? (- v target)) :sell :buy) k (:id agent) (Math/abs (- v target))])) (dissoc (:inventory agent) :money)))

(defn generate-orders
  "Generates a list of buy and sell orders an agent will take to the marketplace."
  [state agent-id]
  (let [target (target-burgers state agent-id)]
    (generate-orders-agent (get-in state [:agents agent-id]) target)))

(ns economy.order-matching)

;; order is a 4-tuple of type, material, order-placer id and amount

(def orders [[:sell :lettuce 0 2] [:buy :lettuce 1 3]
             [:sell :mammoth 2 3] [:buy :mammoth 3 1]])

;; trade is 4 tuple of buyer, seller, material, quantity
[:sell 0 1 :lettuce 2]

(defn single-match [sell-order buy-order]
  (when (and sell-order buy-order)
    (let [[_ good seller offer] sell-order
          [_ _ buyer desire] buy-order
          bag-holder (cond (> desire offer) :seller
                           (< desire offer) :buyer)
          resid-amount (Math/abs (- desire offer))]
      (cond-> {:trade [buyer seller good desire]}
        bag-holder (assoc :residual (case bag-holder
                                      :seller [:sell good seller resid-amount]
                                      :buyer  [:buy  good buyer  resid-amount]))))))

(single-match [:sell :lettuce 0 2] [:buy :lettuce 1 3])
(single-match [:sell :lettuce 0 3] [:buy :lettuce 1 2])
(single-match [:sell :lettuce 0 2] [:buy :lettuce 1 2])

;; if there is no buy or sell order (or neither), nil is retuned
(single-match [:sell :lettuce 0 2] nil)
(single-match nil [:buy :lettuce 0 2])
(single-match nil nil)


; if there's no possible trade, 
(defn find-trades-single-good
  ([orders] (find-trades-single-good orders []))
  ([orders trades]
   (let [{:keys [buy sell]} (group-by first orders)
         trade+residual (single-match (first sell) (first buy))]
     (if trade+residual
       (recur (cond-> (concat (rest buy) (rest sell))
                (:residual trade+residual) (conj (:residual trade+residual)))
              (conj trades (:trade trade+residual)))
       {:trade trades :residual orders}))))

(find-trades-single-good (:lettuce (group-by second orders)))
(find-trades-single-good (:lettuce (group-by second [[:sell :lettuce 0 3] [:buy :lettuce 1 2]])))
(find-trades-single-good (:lettuce (group-by second [[:sell :lettuce 0 2] [:buy :lettuce 1 3]])))


(defn find-trades [orders]
  (let [goods-orders (vals (group-by second orders))]
    (apply merge-with concat (for [good goods-orders]
                               (find-trades-single-good good)))))

(find-trades orders)
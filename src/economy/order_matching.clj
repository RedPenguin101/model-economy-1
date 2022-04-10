(ns economy.order-matching)

;; order is a 4-tuple of type, material, order-placer id and amount
[:sell :lettuce 0 2]

;; trade is 4 tuple of buyer, seller, material, quantity
[0 1 :lettuce 2]

(defn- single-match [sell-order buy-order]
  (when (and sell-order buy-order)
    (let [[_ good seller offer] sell-order
          [_ _ buyer desire] buy-order]
      (cond-> {:trade [buyer seller good desire]}
        (not= desire offer)
        (assoc :residual
               (cond
                 (> desire offer) [:sell good seller (Math/abs (- desire offer))]
                 (< desire offer) [:buy  good buyer  (Math/abs (- desire offer))]))))))

(defn- find-trades-single-good
  ([orders] (find-trades-single-good orders []))
  ([orders trades]
   (let [{:keys [buy sell]} (group-by first orders)
         trade+residual (single-match (first sell) (first buy))]
     (if trade+residual
       (recur (cond-> (concat (rest buy) (rest sell))
                (:residual trade+residual) (conj (:residual trade+residual)))
              (conj trades (:trade trade+residual)))
       {:trade trades :residual orders}))))

(defn find-trades [orders]
  (let [goods-orders (vals (group-by second orders))]
    (apply merge-with concat (for [good goods-orders]
                               (find-trades-single-good good)))))

(comment
  (require '[clojure.spec.alpha :as s]
           '[clojure.spec.gen.alpha :as gen])

  (s/def ::order (s/cat :order-type #{:sell :buy}
                        :good       #{:lettuce :mammoth :ketchup :bread}
                        :agent      (s/and nat-int? #(< % 10))
                        :quantity   (s/and pos-int? #(< % 10))))

  (find-trades (gen/sample (s/gen ::order) 100)))
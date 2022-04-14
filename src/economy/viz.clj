(ns economy.viz
  (:require [oz.core :as oz]))

(defonce server (atom nil))

(defn stop-server []
  (when @server (@server) (reset! server nil)))

(defn- sum  [xs] (apply + xs))
(defn- mean [xs] (int (/ (sum xs) (count xs))))
(defn- sliding-average [period] (fn [xs] (map mean (partition period 1 xs))))
(def ten-period-average (sliding-average 10))

(defn- summary-stats [xs]
  {:total (apply + xs)
   :max (apply max xs)
   :min (apply min xs)
   :mean (mean xs)})

(defn- maps->datapoints [to-take maps]
  (mapcat (fn [price-map]
            (for [[k v] (dissoc price-map :turn)]
              {:turn (:turn price-map)
               :resource k
               :value v}))
          (map #(assoc %1 :turn %2) (reverse (take to-take maps)) (range))))

(defn visualize-simulation [data]
  (let [data (:log (last data))

        production-line-plot
        (let [burgers (reverse (:burgers data))
              avg (ten-period-average burgers)]
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
         :height 300}

        produced-line
        {:data {:values (maps->datapoints 1000 (:produced data))}
         :mark {:type "line" :strokeWidth 1}
         :encoding {:x {:field "turn"
                        :type "quantitative"}
                    :y {:field "value"
                        :type "quantitative"}
                    :color {:field "resource"
                            :type "nominal"}}
         :width 1000
         :height 300}]
    (when (nil? @server) (reset! server (oz/start-server!)))
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
                [:vega-lite trades-line]]
               [:h2 "Produced"]
               [:div {:style {:display "flex" :flex-direction "row"}}
                [:vega-lite produced-line]]])))

(ns economy.main
  (:require [economy.economy :refer [run price-adjust-flat price-adjust-dynamic]]
            [economy.viz :refer [visualize-simulation stop-server]]))

(comment
  (-> {:price-adjustment price-adjust-flat}
      run visualize-simulation)

  (-> {:price-adjustment price-adjust-dynamic}
      run visualize-simulation)

  (-> {:price-adjustment price-adjust-flat :fix-prices true :elon-tusk true}
      run visualize-simulation)

  (-> {:price-adjustment price-adjust-dynamic :fix-prices true :elon-tusk true}
      run visualize-simulation)

  (stop-server))

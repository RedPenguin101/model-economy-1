(ns economy.main
  (:require [economy.economy :refer [run-simulation price-adjust-dynamic price-adjust-flat stop-server]]))

(comment
  (run-simulation {:price-adjustment price-adjust-flat})
  (stop-server))

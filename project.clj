(defproject snake "0.0.1"
  :dependencies [[org.clojure/clojure "1.6.0"]]
  :main ^:skip-aot snake.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})

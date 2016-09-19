(defproject five-weekends "0.1.0-SNAPSHOT"
  :description "Print all years between 2000 -> 2100 that do not have any month with 5 weekends."
  :url ""
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"], [clj-time "0.12.0"]]
  :main ^:skip-aot five-weekends.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})

(ns five-weekends.core
  (:gen-class))

;; Import clojure's time librarys.
(require '[clj-time.core :as t])
(require '[clj-time.predicates :as pr])

;; Iterate over all years, and their months that contain 31 days.
(defn -main
  [& args]
  (doseq [y (range 2000 2100)
    :let [d1 (t/date-time y 1 1) d2 (t/date-time y 3 1) d3 (t/date-time y 5 1) d4 (t/date-time y 7 1) d5 (t/date-time y 8 1) d6 (t/date-time y 10 1) d7 (t/date-time y 12 1) 
          q1 (pr/friday? d1)     q2 (pr/friday? d2)     q3 (pr/friday? d3)     q4 (pr/friday? d4)     q5 (pr/friday? d5)     q6 (pr/friday? d6)      q7 (pr/friday? d7)]
    :when (= (or q1 q2 q3 q4 q5 q6 q7) false)]
    (println y)
  )
)
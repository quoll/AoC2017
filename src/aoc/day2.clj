(ns aoc.day2
  (:require [clojure.java.io :refer [resource]]
            [clojure.string :as str]))

(def read-int #(Integer/parseInt %))

(defn as-arrays [text]
 (->> (str/split text #"\n")
      (map #(str/split % #"\s+"))
      (map (partial map read-int))))

(defn day2 [data]
  (->> data
       (map #(- (apply max %) (apply min %)))
       (apply +)))

(defn -main [& args]
  (let [data (-> (resource "day2.dat")
                 slurp
                 as-arrays)]
    (println (day2 data))))

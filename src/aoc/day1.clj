(ns aoc.day1
  (:require [clojure.java.io :refer [resource]]
            [clojure.string :refer [trim]]))

(defn day1 [data]
  (->> (concat (drop 1 data) [(first data)])
       (map vector data)
       (filter (partial apply =))
       (map first)
       (map #(- (int %) 48))
       (apply +)))

(defn day1* [data]
  (let [half (/ (count data) 2)]
    (->> (map vector data (drop half (concat data data)))
         (filter (partial apply =))
         (map first)
         (map #(- (int %) 48))
         (apply +))))

(defn -main [& args]
  (let [data (-> (resource "day1.dat")
                 slurp
                 trim)]
    (println (day1 data) "; " (day1* data))))

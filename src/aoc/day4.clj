(ns aoc.day4
  (:require [clojure.java.io :refer [resource]]
            [clojure.string :as str]))

(defn valid? [phrase]
  (let [words (str/split phrase #" ")]
    (= (count words) (count (set words)))))

(defn day4 [phrases]
  (count (filter valid? phrases)))

(defn -main [& args]
  (let [data (-> (resource "day4.dat")
                 slurp
                 (str/split #"\n"))]
    (println (day4 data))))

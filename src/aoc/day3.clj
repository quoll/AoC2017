(ns aoc.day3
  (:require [clojure.java.io :refer [resource]]
            [clojure.string :as str]))

(defn day3 [i]
  (let [rt (int (Math/ceil (Math/sqrt i)))
        sq (if (odd? rt) rt (inc rt))
        hsq (int (/ sq 2))
        base (* (- sq 2) (- sq 2))
        side (mod (- i base) (dec sq))]
  (+ (Math/abs (- hsq side)) hsq)))

(defn -main [& args]
  (println (day3 312051)))


(ns aoc.day6
  (:require [clojure.java.io :refer [resource]]
            [clojure.string :as str]))

(defn redistribute
  [offset array]
  (let [size (count array)
        v (array offset) ]
    (loop [i (inc offset)
           arr (update array offset (constantly 0))]
      (if (> i (+ offset v))
        arr
        (recur (inc i) (update arr (mod i size) inc))))))

(defn offset-max
  [s]
  (first
    (reduce (fn [[maxn maxv] [n v]] (if (> v maxv) [n v] [maxn maxv]))
            [0 0]
            (map-indexed vector s))))

(defn day6 [data]
  (loop [c 0 seen #{} banks (vec data)]
    (if (seen banks)
      c
      (recur (inc c) (conj seen banks) (redistribute (offset-max banks) banks)))) )

(defn day6* [data]
  (loop [c 0 seen {} banks (vec data)]
    (if-let [last-count (seen banks)]
      (- c last-count)
      (recur (inc c) (assoc seen banks c) (redistribute (offset-max banks) banks)))))

(defn -main [& args]
  (let [line (-> (resource "day6.dat")
                 slurp
                 (str/split #"\s+"))
        data (map #(Integer/parseInt %) line)]
    (println (day6 data) "; " (day6* data))))

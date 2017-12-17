(ns aoc.day17
  (:require [clojure.string :as str]))

(defn rnd
  [step]
  (fn [[arr p] v]
    (let [np (inc (mod (+ p step) (count arr)))]
      [(concat (take np arr) (cons v (drop np arr))) np])))

(defn day17
  [i]
  (let [r (rnd i)
        [buffer pos] (reduce r [[0] 0] (range 1 2018))]
    (nth buffer (inc pos))))

(defn day17*
  []
  )


(defn -main [& args]
  (println (day17 371) "; ")
    )

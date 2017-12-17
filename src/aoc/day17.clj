(ns aoc.day17
  (:require [clojure.string :as str]))

(defn rnd
  [step]
  (fn [[arr p] v]
    (let [np (inc (mod (+ p step) v))]
      [(doall (concat (take np arr) (cons v (drop np arr)))) np])))

(defn day17
  [step]
  (let [r (rnd step)
        [buffer pos] (reduce r [[0] 0] (range 1 2018))]
    (nth buffer (inc pos))))

(defn day17*
  [step]
  (letfn [(r [[p one] v]
            (let [np (mod (+ p step) v)]
              [(inc np) (if (zero? np) v one)]))]
    (second (reduce r [0 0] (range 1 50000001)))))


(defn -main [& args]
  (println (day17 371) "; " (day17* 371)))

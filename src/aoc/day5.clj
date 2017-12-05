(ns aoc.day5
  (:require [clojure.java.io :refer [resource]]
            [clojure.string :as str]))

(defn print-state
  [ip data]
  (dotimes [x (count data)]
    (if (= x ip)
      (print (str "(" (get data x) ") "))
      (print (str (get data x) " "))))
  (println))

(defn day5 [data]
  (loop [c 0 ip 0 instr data]
    ; (print-state ip instr)
    (if (or (< ip 0) (>= ip (count instr)))
      c
      (recur (inc c) (+ ip (instr ip)) (update instr ip inc)))))

(defn day5* [data]
  (loop [c 0 ip 0 instr data]
    ; (print-state ip instr)
    (if (or (< ip 0) (>= ip (count instr)))
      c
      (let [offset (instr ip)
            upd (if (>= offset 3) dec inc)]
        (recur (inc c) (+ ip offset) (update instr ip upd))))))

(defn -main [& args]
  (let [lines (-> (resource "day5.dat")
                  slurp
                  (str/split #"\n"))
        data (vec (map #(Integer/parseInt %) lines))]
    (println (day5 data) "; " (day5* data))))

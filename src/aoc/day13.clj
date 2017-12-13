(ns aoc.day13
  (:require [clojure.java.io :refer [resource]]
            [clojure.string :as str]))

(defn atoi [x]
  (try
    (Integer/parseInt x)
    (catch NumberFormatException e
      (throw (ex-info (str "Not an int: '" x "'") {:text x})))))

(defn parse-line
  [line]
  (let [[[_ layer depth]] (re-seq #"(\d+): (\d+)" line)]
    [(atoi layer) (atoi depth)]))

(defn day13
  [lines]
  (let [layers (map parse-line lines)
        add-severity (fn [s [l d]]
                       (if (zero? (mod l (* 2 (dec d))))
                         (+ s (* l d))
                         s))]
    (reduce add-severity 0 layers)))

(defn caught?
  [layers start]
  (let [tst-caught? (fn [[l d]]
                      (when (zero? (mod (+ l start) (* 2 (dec d)))) l))]
    (some tst-caught? layers)))

(defn day13*
  [lines]
  (let [layers (map parse-line lines)]
    (->> (range 1000000000)
         (remove (partial caught? layers))
         first)))

(defn -main [& args]
  (let [lines (-> (resource "day13.dat")
                  slurp
                  (str/split #"\n"))]
    (print (day13 lines) "; ")
    (flush)
    (println (day13* lines))))

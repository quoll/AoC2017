(ns aoc.day11
  (:require [clojure.java.io :refer [resource]]
            [clojure.string :as str]))

(def offsets
  {"n"  [1 0 0]
   "ne" [0 1 0]
   "se" [0 0 1]
   "s"  [-1 0 0]
   "sw" [0 -1 0]
   "nw" [0 0 -1]})

(defn dist [[a b c]]
  (let [x (Math/abs (+ b c))
        y (Math/abs (float (- (+ a (/ b 2)) (/ c 2))))]
    (int (+ x (max 0 (- y (/ x 2)))))))

(defn day11
  [dirs]
  (let [vects (map offsets dirs)
        totalv (reduce (fn [[at bt ct] [a b c]] [(+ at a) (+ bt b) (+ ct c)]) vects)]
    (dist totalv)))

(defn day11*
  [dirs]
  (let [vects (map offsets dirs)
        [_ msteps] (reduce (fn [[[at bt ct] m] [a b c]]
                             (let [nstep [(+ at a) (+ bt b) (+ ct c)]
                                   nm (max m (dist nstep))]
                               [nstep nm]))
                           [[0 0 0] 0]
                           vects)]
    msteps))

(defn -main [& args]
  (let [data (-> (resource "day11.dat")
                 slurp
                 (str/trim)
                 (str/split #","))]
    (println (day11 data) "; " (day11* data))))

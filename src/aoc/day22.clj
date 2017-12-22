(ns aoc.day22
  (:require [clojure.java.io :refer [resource]]
            [clojure.string :as str]))

(def dirs {:up 0, :right 1, :down 2, :left 3})

(def deltas [[0 -1] [1 0] [0 1] [-1 0]])

(defn burst
  [{:keys [x y dir inf grid]}]
  (let [n ((grid y) x)
        infected? (= \# n)
        d (mod (if infected? (inc dir) (dec dir)) 4)
        g (update-in grid [y x] {\# \. \. \#})
        [x' y'] (map + [x y] (deltas d))]
    {:x x' :y y' :dir d :inf (if infected? inf (inc inf)) :grid g}))

(defn burst*
  [{:keys [x y dir inf grid]}]
  (let [n ((grid y) x)
        [d' nv] (case n
                 \. [(dec dir) \W]
                 \W [dir \#]
                 \# [(inc dir) \F]
                 \F [(+ 2 dir) \.])
        d (mod d' 4)
        g (assoc-in grid [y x] nv)
        [x' y'] (map + [x y] (deltas d))]
    {:x x' :y y' :dir d :inf (if (= \# nv) (inc inf) inf) :grid g}))

(defn expand
  [brd g]
  (let [gw (count (first g))
        gh (count g)
        ew (+ gw (* 2 brd))
        brd-rows (repeat brd (vec (repeat ew \.)))
        brd-side (repeat brd \.)
        exp-row (fn [r] (vec (concat brd-side r brd-side)))]
    (vec (concat brd-rows (map exp-row g) brd-rows))))

(defn count-hash
  [image]
  (apply + (map #(count (filter #{\#} %)) image)))

(defn mid [r] (int (/ r 2)))

(defn run
  [lines step-count brd evolved?]
  (let [init (map seq lines)
        grid (expand brd init)
        y (mid (count grid))
        x (mid (count (first grid)))
        burst-step (if evolved? burst* burst)]
    (loop [state {:x x :y y :dir 0 :inf 0 :grid grid} c 0]
      (if (= c step-count)
        (:inf state)
        (recur (burst-step state) (inc c))))))

(def border 100)

(defn day22
  [lines]
  (run lines 10000 border false))

(def border* 190)

(defn day22*
  [lines]
  (run lines 10000000 border* true))

(defn -main [& args]
  (let [lines (-> (resource "day22.dat")
                  slurp
                  str/trim
                  (str/split #"\n"))]
    (print (day22 lines) "; ")
    (flush)
    (println (day22* lines))))

(ns aoc.day20
  (:require [clojure.java.io :refer [resource]]
            [clojure.string :as str]
            [clojure.set :as set]))

(defn intv
  [& v]
  (vec (map #(Long/parseLong %) v)))

(defn parse-line
  [line]
  (let [[[_ px py pz vx vy vz ax ay az]] (re-seq #"p=<(-?\d+),(-?\d+),(-?\d+)>, v=<(-?\d+),(-?\d+),(-?\d+)>, a=<(-?\d+),(-?\d+),(-?\d+)>" line)]
    {:p (intv px py pz)
     :v (intv vx vy vz)
     :a (intv ax ay az)}))

(defn add-abs [s] (apply + (map #(Math/abs %) s)))

(defn day20
  [lines]
  (let [data (map-indexed (fn [n v] [n (parse-line v)]) lines)
        mina (apply min (map (comp add-abs :a second) data))
        x (filter #(= mina (add-abs (:a (second %)))) data)]
    (ffirst x)))

(defn remove-collisions
  [particles]
  (let [poss (map :p particles)
        all-poss (into #{} poss)]
    (if (= (count poss) (count all-poss))
      particles
      (let [collisions (set (filter (fn [{p :p}]
                                      (< 1 (count (filter #(= p (:p %)) particles))))
                                    particles))]
        (remove collisions particles)))))

(defn update-vel
  [particles]
  (map (fn [{[vx vy vz] :v [ax ay az] :a :as particle}]
         (assoc particle :v [(+ vx ax) (+ vy ay) (+ vz az)]))
       particles))

(defn update-pos
  [particles]
  (map (fn [{[px py pz] :p [vx vy vz] :v :as particle}]
         (assoc particle :p [(+ px vx) (+ py vy) (+ pz vz)]))
       particles))

(defn increasing-distances?
  [last-distances distances]
  (when last-distances
    (letfn [(closer? [ds lds]
              (some #(< % 0) (map - ds lds)))]
      (not (some identity (map closer? distances last-distances))))))

(defn calc-distances
  [particles]
  (letfn [(distances [{p1 :p}]
            (map (fn [{p2 :p}] (apply + (map #(Math/abs (- %1 %2)) p1 p2))) particles))]
    (map distances particles)))

(def samples 20)

(defn day20*
  [lines]
  (let [sz (count lines)]
    (loop [particles (map parse-line lines) c 0 last-distances nil]
      (let [distances (if (zero? (mod c samples)) (calc-distances particles))]
        (if (and distances (increasing-distances? last-distances distances))
          (count particles)
          (let [particles* (remove-collisions particles)
                pv (update-vel particles*)]
            (recur (update-pos pv) (inc c) (or distances last-distances))))))))

(defn -main [& args]
  (let [lines (-> (resource "day20.dat")
                  slurp
                  str/trim
                  (str/split #"\n"))]
    (print (day20 lines) "; ")
    (flush)
    (println (day20* lines))))

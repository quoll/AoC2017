(ns aoc.day3
  (:require [clojure.java.io :refer [resource]]
            [clojure.string :as str]))

(comment
  (defn day3 [i]
    (let [rt (int (Math/ceil (Math/sqrt i)))
          sq (if (odd? rt) rt (inc rt))
          hsq (int (/ sq 2))
          base (* (- sq 2) (- sq 2))
          side (mod (- i base) (dec sq))]
    (+ (Math/abs (- hsq side)) hsq))))

(defn coords [i]
  (if (<= i 1)
    [0 0]
    (let [rt (int (Math/ceil (Math/sqrt i)))
          sq (if (odd? rt) rt (inc rt))
          hsq (int (/ sq 2))
          base (* (- sq 2) (- sq 2))
          side-offset (mod (- i base) (dec sq))
          side-nr (int (/ (- i base) (dec sq)))
          disp (- hsq side-offset)]
      (case side-nr
        (0 4) [hsq disp]
        1 [disp (- hsq)]
        2 [(- hsq) (- disp)]
        3 [(- disp) hsq]))))

(defn day3 [i]
  (let [[x y] (coords i)]
    (+ (Math/abs x) (Math/abs y))))

(def fieldr 50)

(defn init [half-size]
  (let [data (vec (repeat (* 2 half-size) (vec (repeat (* 2 half-size) 0))))]
    (update-in data [half-size half-size] (constantly 1))))

(defn day3* [input]
  (loop [i 2 field (init fieldr)]
    (let [[x y] (coords i)
          x' (+ fieldr x)
          y' (+ fieldr y)
          v (apply + (for [i [-1 0 1] j [-1 0 1] :when (not (and (zero? i) (zero? j)))]
                       (get-in field [(+ y' j) (+ x' i)])))]
      (if (> v input)
        v
        (recur (inc i) (update-in field [y' x'] + v))))))

(defn -main [& args]
  (println (day3 312051) "; " (day3* 312051)))


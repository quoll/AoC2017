(ns aoc.day15
  (:require [clojure.java.io :refer [resource]]
            [clojure.string :as str]))

(defn word=
  [a b]
  (= (bit-and 0xffff a) (bit-and 0xffff b)))

(defn day15
  [seeda seedb]
  (loop [c 0 m 0 a seeda b seedb]
    (if (= c 40000000)
      m
      (recur (inc c)
             (if (word= a b) (inc m) m)
             (mod (* 16807 a) 0x7fffffff)
             (mod (* 48271 b) 0x7fffffff)))))

(defn series
  [factor fltr]
  (fn n [x]
    (let [nxt (mod (* factor x) 0x7fffffff)]
      (if (zero? (bit-and fltr nxt))
        (cons nxt (lazy-seq (n nxt)))
        (recur nxt)))))

(defn day15*
  [seeda seedb]
  (let [aseries ((series 16807 2r11) seeda) 
        bseries ((series 48271 2r111) seedb)]
    (->> (map word= aseries bseries)
         (take 5000000)
         (filter identity)
         count)))

(defn -main [& args]
  (print (day15 722 354) "; ")
  (flush)
  (println (day15* 722 354)))

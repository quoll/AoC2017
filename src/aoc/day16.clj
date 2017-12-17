(ns aoc.day16
  (:require [clojure.java.io :refer [resource]]
            [clojure.string :as str]))

(def init (vec (map #(char (+ (int \a) %)) (range 16))))

(defn spin
  [arr p]
  (let [sp (- (count arr) (Long/parseLong p))]
    (vec (concat (drop sp arr) (take sp arr)))))

(defn exchange
  [arr p]
  (let [[[_ a b]] (re-seq #"(\d+)/(\d+)" p)
        a' (Long/parseLong a)
        b' (Long/parseLong b)
        av (nth arr a')
        bv (nth arr b')]
    (-> arr
        (assoc a' bv)
        (assoc b' av))))

(defn index-of
  [v arr]
  (first (keep-indexed #(when (#{v} %2) %1) arr)))

(defn partner
  [arr p]
  (let [[[_ [a] [b]]] (re-seq #"([a-p])/([a-p])" p)
        a' (index-of a arr)
        b' (index-of b arr)
        av (nth arr a')
        bv (nth arr b')]
    (-> arr
        (assoc a' bv)
        (assoc b' av))))

(defn move
  [arr mv]
  (let [params (subs mv 1)]
    (case (first mv)
      \s (spin arr params)
      \x (exchange arr params)
      \p (partner arr params)
      (throw (ex-info "Unknown move." {:mv mv})))))

(defn lastv
  [v]
  (let [c (count v)]
    (when-not (zero? c)
      (nth v (dec c)))))

(defn collect-cycle
  [moves]
  (loop [t 0 history []]
    (let [prev (lastv history)]
      (if (= prev init)
        history
        (recur (inc t) (conj history (reduce move (or prev init) moves)))))))

(defn day16
  [moves]
  (apply str (reduce move init moves)))

(defn day16*
  [moves]
  (let [history (collect-cycle moves)
        cycle (count history)]
    (apply str (nth history (dec (mod 1000000000 cycle))))))

(defn -main [& args]
  (let [data (-> (resource "day16.dat")
                 slurp
                 str/trim
                 (str/split #","))]
    (println (day16 data) "; " (day16* data))))

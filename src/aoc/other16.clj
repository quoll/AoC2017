(ns aoc.other16
  (:require [clojure.java.io :refer [resource]]
            [clojure.string :as str]))

(def init (vec (map #(char (+ (int \a) %)) (range 16))))

(defn run
  [arr moves times]
  (let [sz (count arr)
        a (char-array arr)
        spare (char-array sz)]
    (loop [t 0 history []]
      (if (or (and (zero? times) (not= 0 t) (= (vec a) arr))
              (and (not= 0 times) (>= t times)))
        (if (zero? times)
          [t history]
          (apply str a))
        (do
          (loop [[[mv op1 :as m] & rm] moves]
            (if mv
              (do
                (case mv
                  :s (let [sp (- sz op1)]
                       (doseq [s (range op1)]
                         (aset-char spare s (aget a (+ sp s))))
                       (doseq [s (range (dec sz) (dec op1) -1)]
                         (aset-char a s (aget a (- s op1))))
                       (doseq [s (range op1)]
                         (aset-char a s (aget spare s))))
                  :x (let [op2 (nth m 2)
                           x (aget a op2)]
                       (aset-char a op2 (aget a op1))
                       (aset-char a op1 x))
                  :p (let [op2 (nth m 2)]
                       (loop [n 0 q -1 r -1]
                         (if-not (neg? r)
                           (let [x (aget a r)]
                             (aset-char a r (aget a q))
                             (aset-char a q x))
                           (let [cm (aget a n)]
                             (if (or (= cm op1) (= cm op2))
                               (if (neg? q)
                                 (recur (inc n) n -1)
                                 (recur (inc n) q n))
                               (recur (inc n) q -1)))))))
                (recur rm))))
          (recur (inc t) (conj history (apply str a))))))))

(defn process-move
  [move]
  (let [params (subs move 1)]
    (case (first move)
      \s [:s (Long/parseLong params)]
      \x (let [[[_ a b]] (re-seq #"(\d+)/(\d+)" params)]
           [:x (Long/parseLong a) (Long/parseLong b)])
      \p (let [[[_ [a] [b]]] (re-seq #"([a-p])/([a-p])" params)]
           (when (= a b) (throw (ex-info "Bad data" {:op1 a :op2 b})))
           [:p a b])
      (throw (ex-info "Unknown move." {:move move})))))

(defn day16
  [arr moves]
  (let [mvs (map process-move moves)]
    (run arr mvs 1)))

(defn day16*
  [arr moves]
  (let [mvs (map process-move moves)
        [cycles history] (run arr mvs 0)]
    (nth history (dec (mod 1000000000 cycles)))))


(defn -main [& args]
  (let [data (-> (resource "day16.dat")
                 slurp
                 str/trim
                 (str/split #","))]
    (print (day16 init data) "; ")
    (flush)
    (println (day16* init data))))

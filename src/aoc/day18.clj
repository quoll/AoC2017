(ns aoc.day18
  (:refer-clojure :exclude [set mod])
  (:require [clojure.java.io :refer [resource]]
            [clojure.string :as str]))

(defn val-for [state x] (if (keyword? x) (state x 0) x))

(defn snd
  [state x y]
  (assoc state :snd (val-for state x)))

(defn set
  [state x y]
  (assoc state x y))

(defn +n [a b] (if a (+ a b) b))

(defn add
  [state x y]
  (update state x +n y))

(defn *n [a b] (if a (* a b) 0))

(defn mul
  [state x y]
  (update state x *n y))

(defn modn [a b] (if a (clojure.core/mod a b) 0))

(defn mod
  [state x y]
  (update state x modn y))

(defn rcv
  [state x _]
  (if-not (zero? (val-for state x))
    (assoc state :rcv (:snd state))
    state))

(defn jgz
  [{ip :ip :as state} x y]
  (if (pos? (val-for state x))
    (update state :ip + y -1)
    state))

(defn parse
  [line]
  (let [[[head op a1 :as parsed]] (re-seq #"(\w\w\w) (\w)" line)
        a1k (keyword a1)
        opfn (ns-resolve *ns* (symbol op))]
    (if (#{"snd" "rcv"} op)
      [opfn a1k]
      (let [a2 (subs line (inc (count head)))
            a2v (if (and (= 1 (count a2)) (Character/isLetter (first a2)))
                  (keyword a2)
                  (Long/parseLong a2))]
        [opfn a1k a2v]))))

(def init-state {:ip 0})

(defn day18
  [program]
  (let [instrs (into [] (map parse program))
        program-length (count instrs)]
    (loop [{ip :ip :as state} init-state]
      (if (or (neg? ip) (>= ip program-length) (if-let [rcv (:rcv state)] (not= 0 rcv)))
        (:rcv state)
        (let [[op a1 a2] (instrs ip)
              a2' (if (keyword? a2) (state a2 0) a2)]
          (recur (update (op state a1 a2') :ip inc)))))))


(defn -main [& args]
  (let [lines (-> (resource "day18.dat")
                  slurp
                  str/trim
                  (str/split #"\n"))]
    (println (day18 lines) "; " )))

(ns aoc.day23
  (:refer-clojure :exclude [set mod])
  (:require [clojure.java.io :refer [resource]]
            [clojure.string :as str]))

(defn val-for [state x] (if (keyword? x) (state x 0) x))

(defn setn
  [state x y]
  (assoc state x y))

(defn -n [a b] (if a (- a b) (- b)))

(defn sub
  [state x y]
  (update state x -n y))

(defn *n [a b] (if a (* a b) 0))

(defn mul
  [state x y]
  (-> state
      (update x *n y)
      (update :muls inc)))

(defn jnz
  [{ip :ip :as state} x y]
  (if (not= (val-for state x) 0)
    (update state :ip + y -1)
    state))

(def fns
  {"set" setn
   "sub" sub
   "mul" mul
   "jnz" jnz})

(defn as-param
  [x]
  (if (and (= 1 (count x)) (Character/isLetter (first x)))
    (keyword x)
    (try
      (Long/parseLong x)
      (catch Exception _ (throw (ex-info (str "Parse: '" x "'") {:x x}))))
    ))

(defn parse
  [line]
  (let [[[head op a1 a2 :as parsed]] (re-seq #"(\w\w\w) (\w) ([-\w]+)" line)
        a1k (as-param a1)
        a2k (as-param a2)
        opfn (fns op)]
    [opfn a1k a2k]))

(defn calc-step
  [instrs {ip :ip :as this-state}]
  (let [[op a1 a2] (instrs ip)
        a2' (if (keyword? a2) (this-state a2 0) a2)
        nxt-state (op this-state a1 a2')]
    (update nxt-state :ip inc)))

(def init-state {:ip 0 :muls 0})

(defn oor? [ip len] (or (neg? ip) (>= ip len)))

(defn run
  [program]
  (let [instrs (into [] (map parse program))
        program-length (count instrs)]
    (loop [{ip :ip :as state} init-state]
      (if (oor? ip program-length)
        state
        (recur (calc-step instrs state))))))

;; run this from a repl to determine ongoing state
(defn run*
  [program init]
  (let [instrs (into [] (map parse program))
        program-length (count instrs)]
    (loop [{ip :ip :as state} (merge init-state init) c 0]
      (println state ":-" (instrs ip))
      (if (or (and (>= c 30) (= ip 11)) (oor? ip program-length))
        state
        (recur (calc-step instrs state) (inc c))))))

(defn day23
  [program]
  (:muls (run program)))

(defn day23*
  "Steps b from 16500 to 123500 (inclusive) by 17. For each b, loop e and d from 2 to b,
   and check if they divide into b. If they do, then set f to 0, and increment h.
   Prime numbers of b do not have divisors, so h will not be incremented."
  []
  (let [ptext (slurp (resource "primes.txt"))
        t (str/split ptext #"\s+")
        prime? (clojure.core/set (map #(Long/parseLong %) t))]
   (count (remove prime? (range 106500 123517 17)))))

(defn -main [& args]
  (let [lines (-> (resource "day23.dat")
                  slurp
                  str/trim
                  (str/split #"\n"))]
    (println (day23 lines) ";" (day23*))))

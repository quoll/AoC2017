(ns aoc.day23
  (:refer-clojure :exclude [set mod])
  (:require [clojure.java.io :refer [resource]]
            [clojure.string :as str]))

(defn val-for [state x] (if (keyword? x) (state x 0) x))

(defn setn
  [state s2 x y]
  [(assoc state x y) s2])

(defn -n [a b] (if a (- a b) b))

(defn sub
  [state s2 x y]
  [(update state x -n y) s2])

(defn *n [a b] (if a (* a b) 0))

(defn mul
  [state s2 x y]
  [(-> state
       (update x *n y)
       (update :muls inc)) s2])

(defn jnz
  [{ip :ip :as state} s2 x y]
  [(if (not= (val-for state x) 0)
      (update state :ip + y -1)
      state) s2])

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
  ([instrs ip this-state]
   (first (calc-step instrs ip this-state nil)))
  ([instrs ip this-state other-state]
   (let [[op a1 a2] (instrs ip)
         a2' (if (keyword? a2) (this-state a2 0) a2)
         [nxt-state next-ostate] (op this-state other-state a1 a2')]
     [(update nxt-state :ip inc) next-ostate])))

(def init-state {:ip 0 :muls 0})

(defn oor? [ip len] (or (neg? ip) (>= ip len)))

(defn run
  ([program] (run program nil))
  ([program init]
   (let [instrs (into [] (map parse program))
         program-length (count instrs)]
     (loop [{ip :ip :as state} (merge init-state init) c 0]
       (if (oor? ip program-length)
         
         (:muls state)
         (recur (calc-step instrs ip state) (inc c)))))))

(defn day23
  [program]
  (run program))

(defn day23*
  [program]
  (run program {:a 1}))

(defn -main [& args]
  (let [lines (-> (resource "day23.dat")
                  slurp
                  str/trim
                  (str/split #"\n"))]
    (println (day23 lines) ";" )))

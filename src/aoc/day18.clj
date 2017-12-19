(ns aoc.day18
  (:refer-clojure :exclude [set mod])
  (:require [clojure.java.io :refer [resource]]
            [clojure.string :as str]))

(defn val-for [state x] (if (keyword? x) (state x 0) x))

(defn snd
  [state s2 x _]
  [(assoc state :snd (val-for state x)) s2])

(defn setn
  [state s2 x y]
  [(assoc state x y) s2])

(defn +n [a b] (if a (+ a b) b))

(defn add
  [state s2 x y]
  [(update state x +n y) s2])

(defn *n [a b] (if a (* a b) 0))

(defn mul
  [state s2 x y]
  [(update state x *n y) s2])

(defn modn* [a b] (if a (clojure.core/mod a b) 0))

(defn modn
  [state s2 x y]
  [(update state x modn* y) s2])

(defn rcv
  [state s2 x _]
  [(if-not (zero? (val-for state x))
      (assoc state :rcv (:snd state))
      state) s2])

(defn jgz
  [{ip :ip :as state} s2 x y]
  [(if (> (val-for state x) 0)
      (update state :ip + y -1)
      state) s2])

(defn snd2
  [state s2 x _]
  [(update state :sends inc) (update s2 :queue conj (val-for state x))])

(defn rcv2
  [{q :queue :as state} s2 x _]
  (if-let [nextq (first q)]
    [(-> (update state :queue (comp vec rest))
         (assoc x nextq)) s2]
    [(update state :ip dec) s2]))

(def fns
  {"snd" snd
   "set" setn
   "add" add
   "mul" mul
   "mod" modn
   "rcv" rcv
   "jgz" jgz
   "snd2" snd2
   "rcv2" rcv2})

(defn find-fn
  [dbl nm]
  (let [n (if (and dbl (#{"snd" "rcv"} nm)) (str nm "2") nm)]
    (fns n)))

(defn as-param
  [x]
  (if (and (= 1 (count x)) (Character/isLetter (first x)))
    (keyword x)
    (Long/parseLong x)))

(defn parse
  [dbl line]
  (let [[[head op a1 :as parsed]] (re-seq #"(\w\w\w) (\w)" line)
        a1k (as-param a1)
        opfn (find-fn dbl op)]
    (if (#{"snd" "rcv"} op)
      [opfn a1k]
      (let [a2 (subs line (inc (count head)))
            a2v (as-param a2)]
        [opfn a1k a2v]))))

(defn calc-step
  ([instrs ip this-state]
   (first (calc-step instrs ip this-state nil)))
  ([instrs ip this-state other-state]
   (let [[op a1 a2] (instrs ip)
         a2' (if (keyword? a2) (this-state a2 0) a2)
         [nxt-state next-ostate] (op this-state other-state a1 a2')]
     [(update nxt-state :ip inc) next-ostate])))

(def init-state {:ip 0})

(defn oor? [ip len] (or (neg? ip) (>= ip len)))

(defn day18
  [program]
  (let [instrs (into [] (map (partial parse false) program))
        program-length (count instrs)]
    (loop [{ip :ip :as state} init-state]
      (if (or (oor? ip program-length) (if-let [rcv (:rcv state)] (not= 0 rcv)))
        (:rcv state)
        (recur (calc-step instrs ip state))))))

(def init-state2 [{:ip 0 :p 0 :queue [] :sends 0} {:ip 0 :p 1 :queue [] :sends 0}])

(defn waiting?
  [{:keys [ip queue]} program]
  (let [[op] (program ip)]
    (and (= op rcv2) (empty? queue))))

(defn day18*
  [program]
  (let [instrs (into [] (map (partial parse true) program))
        program-length (count instrs)]
    (loop [[{ip0 :ip :as state0} {ip1 :ip :as state1}] init-state2 c 0]
      (if (or (oor? ip0 program-length) (oor? ip1 program-length)
              (and (waiting? state0 instrs) (waiting? state1 instrs))
              #_(= c 200))
        (:sends state1)
        (let [[step0 step1] (calc-step instrs ip0 state0 state1)
              [step1' step0'] (calc-step instrs ip1 step1 step0)]
          (recur [step0' step1'] (inc c)))))))


(defn -main [& args]
  (let [lines (-> (resource "day18.dat")
                  slurp
                  str/trim
                  (str/split #"\n"))]
    (println (day18 lines) ";" (day18* lines))))

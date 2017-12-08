(ns aoc.day8
  (:require [clojure.java.io :refer [resource]]
            [clojure.string :as str]))

(def line-re #"(\w+)\s+(inc|dec)\s+([-\d]+)\s+if\s+(\w+)\s+([><=!]=?)\s+([-\d]+)")

(def diff-ops {'== =, '!= not=})

(defn parse-line
  [line]
  (let [[_ reg op v treg tst tv :as r] (re-find line-re line)
        symtst (symbol tst)]
    {:reg reg
     :op (if (= "inc" op) + -)
     :opval (Integer/parseInt v)
     :treg treg
     :tst (or (diff-ops symtst) (eval symtst))
     :tval (Integer/parseInt tv)}))

(defn exec-line
  [regs {:keys [reg op opval treg tst tval]}]
  (let [rv (regs treg 0)
        mx (regs "m x")]
    (if (tst rv tval)
      (let [new-val (op (regs reg 0) opval)
            regs' (assoc regs reg new-val)]
        (if (> new-val mx) (assoc regs' "m x" new-val) regs'))
      regs)))

(defn day8
  [lines]
  (let [program (map parse-line lines)
        regs (reduce exec-line {"m x" 0} program)]
    (->> (dissoc regs "m x")
         (map second)
         (apply max))))

(defn day8*
  [lines]
  (let [program (map parse-line lines)
        regs (reduce exec-line {"m x" 0} program)]
    (regs "m x")))

(defn -main [& args]
  (let [lines (-> (resource "day8.dat")
                 slurp
                 (str/split #"\n"))]
    (println (day8 lines) "; " (day8* lines))))

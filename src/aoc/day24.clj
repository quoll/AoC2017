(ns aoc.day24
  (:require [clojure.java.io :refer [resource]]
            [clojure.string :as str]))

(defn read-file
  [file]
  (-> (resource file)
      slurp
      str/trim
      (str/split #"\n")))

(defn parse
  [line]
  (let [[a b] (str/split line #"/")]
    [(Long/parseLong a) (Long/parseLong b)]))

(defn conj*
  [s v]
  (if s (conj s v) #{v}))

(defn divide
  [p s]
  (reduce (fn [[f r] x] (if (p x) [(conj f x) r] [f (conj r x)])) [[] []] s))

(defn opp-end [e [a b]] (if (= e a) b a))

(defn remaining-paths
  [s pipes]
  (let [fpipes (filter (fn [[a b]] (or (= s a) (= s b))) pipes)
        ext (apply concat (keep (fn [p]
                                  (let [r (remaining-paths (opp-end s p) (remove #{p} pipes))]
                                    (when (seq r)
                                      (map #(cons p %) r))))
                                fpipes))]
    (when (seq fpipes)
      (concat (map list fpipes) ext))))

(defn sum-path
  [[[a b :as p] & rp]]
  (if p (+ a b (sum-path rp)) 0))

(defn day24
  [lines]
  (let [pipes (map parse lines)
        paths (remaining-paths 0 pipes)]
    (reduce (fn [m p] (let [ps (sum-path p)] (if (> ps m) ps m))) 0 paths)))

(defn -main [& args]
  (let [lines (read-file "day24.dat")]
    (println (day24 lines) ";" )))

(ns aoc.day12
  (:require [clojure.java.io :refer [resource]]
            [clojure.string :as str]
            [clojure.set :as set]))

(defn atoi [x]
  (try
    (Integer/parseInt x)
    (catch NumberFormatException e
      (throw (ex-info (str "Not an int: '" x "'") {:text x})))))

(defn parse-line
  [line]
  (let [[[hdr id]] (re-seq #"(\d+) <-> " line)
        cns (str/split (subs line (count hdr)) #", ")]
    [(atoi id) (set (map atoi cns))]))

(defn group-for
  [connections root]
  (loop [seen #{root} grp [root]]
    (if (empty? grp)
      seen
      (let [nxt (apply set/union (map connections grp))
            new-cns (set/difference nxt seen)]
        (recur (set/union seen new-cns) new-cns)))))

(defn day12
  [lines]
  (let [index (into {} (map parse-line lines))]
    (count (group-for index 0))))

(defn add-group
  [connections [roots grouped] id]
  (if (grouped id)
    [roots grouped]
    [(conj roots id) (set/union grouped (group-for connections id))]))

(defn day12*
  [lines]
  (let [index (into {} (map parse-line lines))
        [roots _] (reduce (partial add-group index) [[] #{}] (keys index))]
    (count roots)))

(defn -main [& args]
  (let [lines (-> (resource "day12.dat")
                  slurp
                  (str/split #"\n"))]
    (println (day12 lines) "; " (day12* lines))))

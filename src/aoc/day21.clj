(ns aoc.day21
  (:require [clojure.java.io :refer [resource]]
            [clojure.string :as str]
            [clojure.set :as set]))

(defn var1 [r1 r2]
  (fn [ss]
    (for [x r1 y r2] ((ss y) x))))

(defn var2 [r1 r2]
  (fn [ss]
    (for [x r1 y r2] ((ss x) y))))

(defn parse
  [line]
  (let [[[_ f t]] (re-seq #"([#./]+) => ([#./]+)" line)
        from (vec (map vec (str/split f #"/")))
        to (vec (map vec (str/split t #"/")))
        sz (count from)
        r1 (range sz)
        r2 (range (dec sz) -1 -1)
        variations (juxt (var1 r1 r1) (var1 r1 r2) (var1 r2 r1) (var1 r2 r2)
                         (var2 r1 r1) (var2 r1 r2) (var2 r2 r1) (var2 r2 r2))
        froms (->> (variations from)
                   (map (partial partition sz))
                   (map #(vec (map vec %))))]
    (for [v froms] [v to])))

(defn tx-by
  [tx image]
  (let [sz (count image)
        w (if (even? sz) 2 3)]
    (apply
     concat
     (map
      (fn [row]
        (let [images-row (map
                          (fn [col]
                            (let [subimage (map #(take w (drop col %)) (take w (drop row image)))]
                              (if-let [wider (tx subimage)]
                                wider
                                (throw (ex-info (str "Did not find: " subimage) {:sub subimage})))))
                          (range 0 sz w))]
          (map (fn [r] (apply concat (map #(nth % r) images-row))) (range (inc w)))))
      (range 0 sz w)))))

(defn count-hash
  [image]
  (apply + (map #(count (filter #{\#} %)) image)))

(def init [[\. \# \.] [\. \. \#] [\# \# \#]])

(defn run
  [lines it]
  (let [txs (->> (map parse lines)
                 (apply concat)
                 (into {}))]
    (loop [image init c 0]
      (if (= c it)
        (count-hash image)
        (recur (tx-by txs image) (inc c))))))

(defn day21
  [lines]
  (run lines 5))

(defn day21*
  [lines]
  (run lines 18))

(defn -main [& args]
  (let [lines (-> (resource "day21.dat")
                  slurp
                  str/trim
                  (str/split #"\n"))]
    (print (day21 lines) "; ")
    (flush)
    (println (day21* lines))))

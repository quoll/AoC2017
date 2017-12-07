(ns aoc.day7
  (:require [clojure.java.io :refer [resource]]
            [clojure.string :as str]))

(defn parse-line
  [line]
  (let [[p nm wt plate?] (re-find #"(\w+)\s+\((\d+)\)(\s+->\s+)?" line)
        w (Integer/parseInt wt)]
    (if plate?
      (let [children (->> (count p)
                          (subs line)
                          (re-seq #"(\w+)(,\s+)?")
                          (map second))]
        [nm w children])
      [nm w])))

(defn build-tree
  [lines]
  (letfn [(add-line [tree line]
            (let [[nm wt children] (parse-line line)
                  t (update tree nm #(assoc %1 :weight wt))]
              (if children
                (-> (reduce (fn [m c] (update m c #(assoc % :parent nm))) t children)
                    (update nm #(assoc % :children children)))
                t)))]
    (reduce add-line {} lines)))

(defn day7
  [tree]
  (ffirst (remove (comp :parent second) tree)))

(defn diffn [[f & r]]
  "Find the single different value in a sequence and return it"
  (let [a (seq (keep #(if (not= f %) %) r))]
    (if (= 1 (count a)) (first a) f)))

(def tree-weight
  (memoize (fn [tree prg]
             (let [{:keys [weight children]} (tree prg)]
               (apply + weight (map (partial tree-weight tree) children))))))

(def balance
  (memoize (fn [tree w root]
             (let [{kids :children root-weight :weight} (tree root)
                   weights (map (partial tree-weight tree) kids)]
               (if (apply = weights)
                 (- root-weight w)
                 (let [bad-weight (diffn weights)
                       good-weight (first (remove #{bad-weight} weights))
                       n (first (keep-indexed #(when (= bad-weight %2) %1) weights))]
                   (balance tree (- bad-weight good-weight) (nth kids n))))))))

(defn day7*
  [tree]
  (let [root (day7 tree)]
    (balance tree 0 root)))

(defn -main [& args]
  (let [lines (-> (resource "day7.dat")
                 slurp
                 (str/split #"\n"))
        data (build-tree lines)]
    (println (day7 data) "; " (day7* data))))

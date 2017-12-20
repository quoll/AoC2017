(ns aoc.day19
  (:require [clojure.java.io :refer [resource]]
            [clojure.string :as str]))

(def dir-index {:up 0 :right 1 :down 2 :left 3})

(defn dir-for
  [[u r d l]]
  (cond
    (not= \space u) :up
    (not= \space r) :right
    (not= \space d) :down
    (not= \space l) :left))

(defn get-surrounds
  [f x y]
  (letfn [(vfor [a b]
            (get (get f (+ y b) []) (+ x a) \space))]
    [(vfor 0 -1) (vfor 1 0) (vfor 0 1) (vfor -1 0)]))

(defn new-direction
  [field {:keys [x y dir] :as r}]
  (let [surrounds (get-surrounds field x y)
        s' (assoc surrounds (mod (+ 2 (dir-index dir)) 4) \space)
        dir-count (count (remove #{\space} s'))]
    (cond
      (> dir-count 1) (throw (ex-info "Unexpected multiple directions" {:r r}))
      (zero? dir-count) (throw (ex-info (str "Unexpected Termination:" r) {:r r}))
      :default (assoc r :dir (dir-for s')) )))

(defn step
  [field {:keys [x y] :as r}]
  (let [spot ((field y) x)]
    (if (= \space spot)
      (assoc r :end true)
      (let [r (cond
                (Character/isLetter spot) (update r :history conj spot)
                (= \+ spot) (new-direction field r)
                (#{\- \|} spot) r
                :default (throw (ex-info (str "Unexpected character: " spot) {:x x :y y :r r})))]
        (case (:dir r)
          :up (update r :y dec)
          :down (update r :y inc)
          :left (update r :x dec)
          :right (update r :x inc))))))

(defn day19
  [[start-line :as lines]]
  (let [startx (str/index-of start-line \|)
        field (vec (map vec lines))
        width (count start-line)
        height (count lines)]
    (loop [{:keys [x y end history] :as r} {:x startx :y 0 :dir :down :history []} c 0]
      (if end
        (str (apply str history) "; " (dec c))
        (do
          (when (or (< x 0) (< y 0) (>= x width) (>= y height))
            (throw (ex-info "Out of bounds" {:x x :y y :r r})))
          (recur (step field r) (inc c)))))))


(defn -main [& args]
  (let [lines (-> (resource "day19.dat")
                  slurp
                  (str/split #"\n"))]
    (println (day19 lines))))

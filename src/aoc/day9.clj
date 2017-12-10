(ns aoc.day9
  (:require [clojure.java.io :refer [resource]]
            [clojure.string :as str]))

(defn us [s ns] (assoc s :state ns))

(defmulti process (fn [s _] (:state s)))

(defmethod process :garbage [s c]
  (case c
    \! (us s :ignore)
    \> (us s :default)
    (update s :gc inc)))

(defmethod process :ignore [s c]
  (us s :garbage))

(defmethod process :default [{:keys [current total] :as s} c]
  (case c
    \< (us s :garbage)
    \{ (assoc s :current (inc current) :total (+ (inc current) total))
    \} (assoc s :current (dec current))
    s))

(defn process-stream
  [stream]
  (loop [ms {:state :default :current 0 :total 0 :gc 0} [ch & r] stream]
    (if ch
      (recur (process ms ch) r)
      ms)))

(defn day9
  [stream]
  (:total (process-stream stream)))

(defn day9*
  [stream]
  (:gc (process-stream stream)))

(defn -main [& args]
  (let [data (-> (resource "day9.dat")
                  slurp)]
    (println (day9 data) "; " (day9* data))))

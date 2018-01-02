(ns aoc.day25
  (:require [clojure.java.io :refer [resource]]
            [clojure.string :as str]))

(def blocksize 32)

(def block (repeat blocksize 0))

(defn move
  "Returns [tape position]"
  [dir tape pos]
  (let [dfn (case dir :right inc :left dec)
        pos' (dfn pos)]
    (cond
      (< pos' 0) [(vec (concat block tape)) (+ pos' blocksize)]
      (>= pos' (count tape)) [(vec (concat tape block)) pos']
      :default [tape pos'])))

(defn state-op
  [tape pos zw zd zs ow od os]
  (let [[w d s] (if (zero? (get tape pos))
                  [zw zd zs]
                  [ow od os])
        t (assoc tape pos w)
        [ret-tape ret-pos] (move d t pos)]
    {:tape ret-tape
     :pos ret-pos
     :state s}))

(defmulti step :state)

(defmethod step :a
  [{:keys [tape pos]}]
  (state-op tape pos 1 :right :b 0 :left :e))

(defmethod step :b
  [{:keys [tape pos]}]
  (state-op tape pos 1 :left :c 0 :right :a))

(defmethod step :c
  [{:keys [tape pos]}]
  (state-op tape pos 1 :left :d 0 :right :c))

(defmethod step :d
  [{:keys [tape pos]}]
  (state-op tape pos 1 :left :e 0 :left :f))

(defmethod step :e
  [{:keys [tape pos]}]
  (state-op tape pos 1 :left :a 1 :left :c))

(defmethod step :f
  [{:keys [tape pos]}]
  (state-op tape pos 1 :left :e 1 :right :a))

(def init {:tape (vec block) :pos (/ blocksize 2) :state :a})

(defn checksum [s] (apply + s))

(defn day25
  [end]
  (loop [steps 0 {tape :tape :as machine} init]
    (if (>= steps end)
      (checksum tape)
      (recur (inc steps) (step machine)))
    )
  )

(defn -main [& args]
  (print (day25 12208951) "; " )
  (flush)
  (println ))

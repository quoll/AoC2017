(ns aoc.day10
  (:require [clojure.java.io :refer [resource]]
            [clojure.string :as str]))

(defn knot
  [[array current skip] n]
  (let [sz (count array)
        end (+ n current)
        overlap (- end sz)
        arr (if (> overlap 0)
              (let [sa (reverse (concat (drop current array) (take (- end sz) array)))]
                (concat (take overlap (drop (- sz current) sa))
                        (drop overlap (take current array))
                        (take (- sz current) sa)))
              (concat (take current array)
                      (reverse (take n (drop current array)))
                      (drop end array)))]
    [arr (mod (+ end skip) sz) (inc skip)]))

(defn day10
  [nums]
  (let [[[f s]] (reduce knot [(range 256) 0 0] nums)]
    (* f s)))

(def salt [17 31 73 47 23])

(defn sparse-hash
  [text]
  (let [data (concat (map int text) salt)]
    (loop [array (range 256) current 0 skip 0 count 64]
      (if (zero? count)
        array
        (let [[array' current' skip'] (reduce knot [array current skip] data)]
          (recur array' current' skip' (dec count)))))))

(defn day10*
  [text]
  (let [shash (sparse-hash text)
        dhash (map (partial apply bit-xor) (partition 16 shash))]
    (apply format "%02x%02x%02x%02x%02x%02x%02x%02x%02x%02x%02x%02x%02x%02x%02x%02x" dhash)))

(defn -main [& args]
  (let [nums (partial map #(Integer/parseInt %))
        text (-> (resource "day10.dat")
                 slurp
                 str/trim)
        data (-> text
                 (str/split #",")
                 nums)]
    (println (day10 data) "; " (day10* text))))

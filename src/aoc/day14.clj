(ns aoc.day14
  (:require [aoc.day10 :as h]))

(def bits
  (into {}
        (map (fn [[n b]] [(first (format "%x" n)) b])
             {2r0000 "...."
              2r0001 "...#"
              2r0010 "..#."
              2r0011 "..##"
              2r0100 ".#.."
              2r0101 ".#.#"
              2r0110 ".##."
              2r0111 ".###"
              2r1000 "#..."
              2r1001 "#..#"
              2r1010 "#.#."
              2r1011 "#.##"
              2r1100 "##.."
              2r1101 "##.#"
              2r1110 "###."
              2r1111 "####"})))

(def bin
  {2r0000 0
   2r0001 1
   2r0010 1
   2r0011 2
   2r0100 1
   2r0101 2
   2r0110 2
   2r0111 3
   2r1000 1
   2r1001 2
   2r1010 2
   2r1011 3
   2r1100 2
   2r1101 3
   2r1110 3
   2r1111 4})

(def bitcount
  (into {} (map (fn [c] [(first (format "%x" c)) (bin c)]) (range 0x10))))

(defn count-bits
  [text]
  (reduce #(+ %1 (bitcount %2 :err)) 0 text))

(defn day14
  [text]
  (->> (range 128)
       (map (fn [n] (format "%s-%d" text n)))
       (map h/day10*)
       (map count-bits)
       (apply +)))

(defn add-region
  [[disk region] [x y :as coords]]
  (letfn [(surrounding [a b]
            (remove
             (fn [[i j]] (or (> 0 i) (> 0 j)
                             (>= i (count (first disk))) (>= j (count disk))))
             [[a (dec b)] [a (inc b)] [(dec a) b] [(inc a) b]]))
          (set-region [d [i j]]
            (if (= \# ((d j) i))
              (let [d' (assoc-in d [j i] region)]
                (reduce set-region d' (surrounding i j)))
              d))]
    (if (= \# ((disk y) x))
      [(set-region disk coords) (inc region)]
      [disk region])))

(defn day14*
  [text]
  (let [disk (->> (range 128)
                  (map (fn [n] (format "%s-%d" text n)))
                  (map h/day10*)
                  (map (partial mapcat bits))
                  (map vec)
                  vec)
        coords (for [y (range 128) x (range 128)] [x y])]
    (second (reduce add-region [disk 0] coords))))

(defn -main [& args]
  (println (day14 "xlqgujun") "; " (day14* "xlqgujun")))

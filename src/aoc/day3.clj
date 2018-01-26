(ns aoc.day3)

(defn square [x] (* x x))

(defn coords [i]
  (if (<= i 1)
    [0 0]
    (let [rt (int (Math/ceil (Math/sqrt i)))
          sq (if (odd? rt) rt (inc rt))
          hsq (int (/ sq 2))
          base (square (- sq 2))
          side-offset (mod (- i base) (dec sq))
          side-nr (int (/ (- i base) (dec sq)))
          disp (- hsq side-offset)]
      (case side-nr
        (0 4) [hsq disp]
        1 [disp (- hsq)]
        2 [(- hsq) (- disp)]
        3 [(- disp) hsq]))))

(defn offset [x y]
  (let [ms (max (Math/abs x) (Math/abs y))
        sq (dec (* 2 ms))
        base (square sq)
        side (inc sq)
        hside (/ side 2)]
    (cond
      (and (= x ms) (< y ms)) (- (+ base hside) y)
      (= y (- ms)) (- (+ base side hside) x)
      (= x (- ms)) (+ base (* 2 side) hside y)
      (= y ms) (+ base (* 3 side) hside x))))

(defn day3 [i]
  (let [[x y] (coords i)]
    (+ (Math/abs x) (Math/abs y))))

(defn day3* [input]
  (loop [i 2 mem [1 1]]
    (let [[x y] (coords i)
          v (apply + (for [p [-1 0 1] q [-1 0 1]
                           :let [f (offset (+ x p) (+ y q))]
                           :when (< f i)]
                       (get mem f)))]
      (if (> v input)
        v
        (recur (inc i) (conj mem v))))))

(defn -main [& args]
  (println (day3 312051) "; " (day3* 312051)))


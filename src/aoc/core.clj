(ns aoc.core)

(def max-day 4)

(defn -main [& args]
  (doseq [n (range 1 (inc max-day))]
    (print (str "Day" n ": "))
    (let [ns-symbol (symbol (str "aoc.day" n))]
      (require ns-symbol)
      (let [day-ns (the-ns ns-symbol)]
        (if-let [main (ns-resolve day-ns '-main)]
          (apply main args)
          (println "Can't find -main for namespace " (ns-name day-ns)))))))


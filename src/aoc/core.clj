(ns aoc.core)

(defn namespace-load
  "Load a namespace, returning true on success, and false otherwise."
  [ns-symbol]
  (not
    (try
      (require ns-symbol)
      (catch Exception _ :error))))

(defn -main
  "Execute a -main function in every namespace of the form aoc.day###
   where the ### is a series of numbers starting at 1. Finishes at
   the end of the consecutive namespaces."
  [& args]
  (doseq [n (rest (range))
          :let [ns-symbol (symbol (str "aoc.day" n))]
          :while (namespace-load ns-symbol)]
    (print (str "Day" n ": "))
    (flush)
    (let [day-ns (the-ns ns-symbol)]
      (if-let [main (ns-resolve day-ns '-main)]
        (apply main args)
        (println "Can't find -main for namespace " (ns-name day-ns))))))


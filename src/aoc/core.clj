(ns aoc.core
  (:require [aoc.day1 :as day1]
            [aoc.day2 :as day2]))

(defn -main [& args]
  (print "Day 1: ")
  (apply day1/-main args)
  (print "Day 2: ")
  (apply day2/-main args)
  )

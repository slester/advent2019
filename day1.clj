(ns day1
  (:require [clojure.string :refer [split]]))

(def input (map read-string (split (slurp "input/1.txt") #"\n")))

(defn fuel [mass]
  (let [d (quot mass 3)]
    (- d 2)))

;; part one
(println "Part 1:"
  (reduce + (map fuel input)))

;; part two
(defn additional-fuel [mass]
  (loop [total-fuel          (fuel mass)
         remaining-fuel-mass total-fuel]
    (let [f (fuel remaining-fuel-mass)]
      (if (> 1 f)
        total-fuel
        (recur (+ total-fuel f) f)))))

(println "Part 2:"
  (reduce + (map additional-fuel input)))

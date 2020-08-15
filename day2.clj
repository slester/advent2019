(ns day2
  (:require [clojure.string :refer [split]]))

(def input (into '[] (map read-string (split (slurp "input/2.txt") #","))))

(defn process [nums]
  (loop [pos 0
         is nums]
    (let [opcode (is pos)
          first-val (is (nums (inc pos)))
          second-val (is (nums (+ pos 2)))
          storage-pos (is (+ pos 3))]

      (case opcode
        1 (recur (+ pos 4) (assoc is storage-pos (+ first-val second-val)))
        2 (recur (+ pos 4) (assoc is storage-pos (* first-val second-val)))
        99 (is 0)))))

(println "Part 1: "
         (process (assoc input 1 12 2 2)))
(println "Part 2: "
         (for [noun (range 0 100)
               verb (range 0 100)
               :when (= 19690720 (process (assoc input 1 noun 2 verb)))]
           (+ verb (* 100 noun))))

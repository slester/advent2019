(ns day4
  (:require [clojure.string :refer [split]]))

(def input (map read-string (split (slurp "input/4.txt") #"-")))
(def min-password (first input))
(def max-password (second input))

(defn has-double? [s]
  (let [groups (partition-by identity s)]
    (some #(<= 2 (count %)) groups)))

(defn has-true-double? [s]
  (let [groups (partition-by identity s)]
    (some #(= 2 (count %)) groups)))

(defn only-increase? [s]
  (every? true? (for [i (range 1 (count s))]
                  (<= (read-string (str (nth s (dec i))))
                      (read-string (str (nth s i)))))))

(defn good-password? [s]
  (and (= 6 (count s))
       (< min-password (read-string s))
       (< (read-string s) max-password)
       (has-double? s)
       (only-increase? s)))

(defn better-password? [s]
  (and (good-password? s)
       (has-true-double? s)))

(def examples {"111111" false ; because of range
               "555555" true
               "456788" true
               "543211" false
               "567890" false
               "223450" false
               "123789" false})
(filter #(not= (val %) (good-password? (key %))) examples)

; part 1
(count (filter #(good-password? (str %)) (range min-password max-password)))
; part 2
(count (filter #(better-password? (str %)) (range min-password max-password)))
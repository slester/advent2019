(ns day3
  (:require [clojure.string :refer [split]]))

(def input-lines (split (slurp "input/3.txt") #"\n"))
(def wire1-directions (into [] (split (first input-lines) #",")))
(def wire2-directions (into [] (split (second input-lines) #",")))
(def origin {:x 0 :y 0})

(defn manhattan-distance [a b]
  (+ (Math/abs (- (:x a) (:x b)))
     (Math/abs (- (:y a) (:y b)))))

(defn parse-direction [s]
  (let [pieces (split s #"")
        dir (first pieces)
        count (read-string (apply str (rest pieces)))]
    [dir count]))

(defn points-cross? [a b]
  (and (= (:x a) (:x b))
       (= (:y a) (:y b))))

(defn generate-points [point directions]
  (into #{} (flatten (for [d directions]
                       ;; 'point' isn't updating between directions
                       (let [[dir count] (parse-direction d)
                             [f axis] (case dir "R" [+ :x]
                                                "L" [- :x]
                                                "U" [+ :y]
                                                "D" [- :y])]
                         (for [i (range 1 (inc count))]
                           (update-in point [axis] (partial f i))))))))

(def wire1 (generate-points origin wire1-directions))
(def wire2 (generate-points origin wire2-directions))
(def crosspoints (for [a wire1
                       b wire2
                       :when (points-cross? a b)] a))

(second (sort-by #(manhattan-distance % origin) < crosspoints))
(ns day3
  (:require [clojure.set :refer [union intersection]]
            [clojure.string :refer [split]]))

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

(defn generate-points [point command]
  (into #{}
    (let [[dir count] (parse-direction command)
          [f axis] (case dir "R" [+ :x]
                             "L" [- :x]
                             "U" [+ :y]
                             "D" [- :y])]
      (for [i (range 1 (inc count))]
        (update-in point [axis] #(f % i))))))

(defn run-commands [commands]
  (loop [p origin
         cs commands
         ps #{}]
    (if (empty? cs)
      ps
      (let [c (first cs)
            new-points (generate-points p c)
            new-origin (last new-points)]
        (recur new-origin (rest cs) (union ps new-points))))))


(def wire1 (run-commands wire1-directions))
(def wire2 (run-commands wire2-directions))
(def crosspoints (intersection wire1 wire2))

(manhattan-distance origin
                    (first (sort-by #(manhattan-distance % origin) crosspoints)))
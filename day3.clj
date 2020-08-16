(ns day3
  (:require [clojure.set :refer [union intersection]]
            [clojure.string :refer [split]]))

(def input-lines (split (slurp "input/3.txt") #"\n"))
(def wire1-commands (into [] (split (first input-lines) #",")))
(def wire2-commands (into [] (split (second input-lines) #",")))

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
  (let [[dir count] (parse-direction command)
        [f axis] (case dir "R" [+ :x]
                           "L" [- :x]
                           "U" [+ :y]
                           "D" [- :y])]
    (for [i (range 1 (inc count))]
      (update-in point [axis] #(f % i)))))

(defn run-commands [commands]
  (loop [p origin
         cs commands
         ps []]
    (if (empty? cs)
      ps
      (let [c (first cs)
            new-points (generate-points p c)
            new-origin (last new-points)]
        (recur new-origin (rest cs) (concat ps new-points))))))

(def wire1 (run-commands wire1-commands))
(def wire2 (run-commands wire2-commands))
(defn crosspoints [a b] (intersection (into #{} a) (into #{} b)))

;; part 1
(defn shortest-distance [a b]
  (manhattan-distance origin
                      (first (sort-by #(manhattan-distance % origin)
                                      (crosspoints a b)))))
(shortest-distance wire1 wire2)

;; part 2
(defn shortest-steps [a b]
  (first
    (sort
      (for [i (crosspoints a b)
            :let [a-steps (.indexOf a i)
                  b-steps (.indexOf b i)]]
        (+ 2 a-steps b-steps))))) ;; must add 2 to account for the origin->first point
(shortest-steps wire1 wire2)

;; examples
(= {:x -30 :y 0} (last (generate-points origin "L30")))
(= {:x 30 :y 0} (last (generate-points origin "R30")))
(= {:x 0 :y -30} (last (generate-points origin "D30")))
(= {:x 0 :y 30} (last (generate-points origin "U30")))
(= {:x 10 :y 10} (last (run-commands ["R10" "U10"])))
(= {:x 10 :y 10} (last (run-commands ["U10" "R10"])))
(= {:x -10 :y -10} (last (run-commands ["D10" "L10"])))
(= 20 (manhattan-distance origin {:x 10 :y 10}))

(def ex1a-commands (split "R8,U5,L5,D3" #","))
(def ex1b-commands (split "U7,R6,D4,L4" #","))
(= 6 (shortest-distance (run-commands ex1a-commands) (run-commands ex1b-commands)))
(def ex2a-commands (split "R75,D30,R83,U83,L12,D49,R71,U7,L72" #","))
(def ex2b-commands (split "U62,R66,U55,R34,D71,R55,D58,R83" #","))
(= 159 (shortest-distance (run-commands ex2a-commands) (run-commands ex2b-commands)))
(def ex3a-commands (split "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51" #","))
(def ex3b-commands (split "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7" #","))
(= 135 (shortest-distance (run-commands ex3a-commands) (run-commands ex3b-commands)))


(def ex4a-commands (split "R75,D30,R83,U83,L12,D49,R71,U7,L72" #","))
(def ex4b-commands (split "U62,R66,U55,R34,D71,R55,D58,R83" #","))
(= 610 (shortest-steps (run-commands ex4a-commands) (run-commands ex4b-commands)))
(def ex5a-commands (split "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51" #","))
(def ex5b-commands (split "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7" #","))
(= 410 (shortest-steps (run-commands ex5a-commands) (run-commands ex5b-commands)))

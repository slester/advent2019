(ns day5
  (:require [clojure.string :refer [split]]))

(def input (into '[] (map read-string (split (slurp "input/5.txt") #","))))

(defn parse-opcode [s]
  (let [s (str "0000" s) ; pad instruction set for implied 0s
        instruction (clojure.string/replace (subs s (- (count s) 2)) #"^0+" "") ; 0-prefixed numbers are treated as octal
        param1-mode (subs s (- (count s) 3) (- (count s) 2))
        param2-mode (subs s (- (count s) 4) (- (count s) 3))
        param3-mode (subs s (- (count s) 5) (- (count s) 4))]
    (map read-string [instruction param1-mode param2-mode param3-mode])))

(defn get-param [is pos num param-mode]
  (let [val (is (+ pos num))]
    (if (= 1 param-mode) val (is val))))

(defn process [instructions in]
  (loop [pos 0
         is instructions
         out '[]]
    (let [[opcode param1-mode param2-mode _] (parse-opcode (is pos))]
      (case opcode
        1 (let [first-val (get-param is pos 1 param1-mode)
                second-val (get-param is pos 2 param2-mode)
                storage-pos (get-param is pos 3 1)]
              (recur (+ pos 4) (assoc is storage-pos (+ first-val second-val)) out))
        2 (let [first-val (get-param is pos 1 param1-mode)
                second-val (get-param is pos 2 param2-mode)
                storage-pos (get-param is pos 3 1)]
            (recur (+ pos 4) (assoc is storage-pos (* first-val second-val)) out))
        3 (let [val (get-param is pos 1 1)]
            (recur (+ pos 2) (assoc is val in) out))
        4 (let [val (get-param is pos 1 param1-mode)]
            (recur (+ pos 2) is (conj out val)))
        5 (let [first-val (get-param is pos 1 param1-mode)
                second-val (get-param is pos 2 param2-mode)]
            (if (not= 0 first-val)
               (recur second-val is out)
               (recur (+ pos 3) is out)))
        6 (let [first-val (get-param is pos 1 param1-mode)
                second-val (get-param is pos 2 param2-mode)]
            (if (= 0 first-val)
              (recur second-val is out)
              (recur (+ pos 3) is out)))
        7 (let [first-val (get-param is pos 1 param1-mode)
                second-val (get-param is pos 2 param2-mode)
                storage-pos (get-param is pos 3 1)]
            (if (< first-val second-val)
              (recur (+ pos 4) (assoc is storage-pos 1) out)
              (recur (+ pos 4) (assoc is storage-pos 0) out)))
        8 (let [first-val (get-param is pos 1 param1-mode)
                second-val (get-param is pos 2 param2-mode)
                storage-pos (get-param is pos 3 1)]
            (if (= first-val second-val)
              (recur (+ pos 4) (assoc is storage-pos 1) out)
              (recur (+ pos 4) (assoc is storage-pos 0) out)))
        99 out))))

; part 1
(process input 1)

; part 2
(process input 5)
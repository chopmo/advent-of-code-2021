(ns advent.day2
  (:require [clojure.string :as str]))

(defn puzzle2-1 []
  (->> (slurp "resources/input2.txt")
       (str/split-lines)
       (map (fn [s]
              (let [[command arg] (str/split s #" ")
                    arg           (Integer/parseInt arg)]
                (case command
                  "forward" [arg 0]
                  "up"      [0 (* -1 arg)]
                  "down"    [0 arg]))))
       (reduce (fn [[a b] [new-a new-b]]
                 [(+ a new-a)
                  (+ b new-b)])
               [0 0])
       (apply *)))


(defn puzzle2-2 []
  (loop [x        0
         y        0
         aim      0
         commands (->> (slurp "resources/input2.txt")
                       (str/split-lines)
                       (map #(str/split % #" ")))]
    (let [cmd (first commands)]
      (if cmd
        (let [kw  (first cmd)
              arg (Integer/parseInt (second cmd))]
          (case kw
            "forward" (recur (+ x arg) (+ y (* aim arg)) aim (rest commands))
            "up"      (recur x y (- aim arg) (rest commands))
            "down"    (recur x y (+ aim arg) (rest commands))))
        (* x y)))))

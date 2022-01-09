(ns advent.day6
  (:require [clojure.string :as str]))

(defn read-numbers [filename]
  (map #(Integer/parseInt %) (str/split (str/trim (slurp filename)) #",")))

(defn evolve [p]
  {0 (get p 1 0)
   1 (get p 2 0)
   2 (get p 3 0)
   3 (get p 4 0)
   4 (get p 5 0)
   5 (get p 6 0)
   6 (+ (get p 7 0) (get p 0 0))
   7 (get p 8 0)
   8 (get p 0 0)})

(defn count-population [filename generation]
  (let [population (frequencies (read-numbers filename))]
    (->> (iterate evolve population)
         (drop generation)
         first
         vals
         (apply +))))

(defn puzzle5-1 []
  (count-population "resources/input6.txt" 80))

(defn puzzle5-2 []
  (count-population "resources/input6.txt" 256))

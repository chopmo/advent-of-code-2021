(ns advent.day1
  (:require [clojure.string :as str]))

(defn puzzle1-1 []
  (->> (slurp "resources/input1.txt")
       str/split-lines
       (map #(Integer/parseInt %))
       (partition 2 1)
       (filter (fn [[i1 i2]] (> i2 i1)))
       count))


(defn puzzle1-2 []
  (->> (slurp "resources/input1.txt")
       str/split-lines
       (map #(Integer/parseInt %))
       (partition 3 1)
       (map (partial apply +))
       (partition 2 1)
       (filter (fn [[i1 i2]] (> i2 i1)))
       count))

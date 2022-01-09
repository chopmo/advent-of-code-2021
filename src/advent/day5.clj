(ns advent.day5
  (:require [clojure.string :as str]))

(defn read-coord [s]
  (let [parts (str/split s #",")]
    {:x (Integer/parseInt (first parts))
     :y (Integer/parseInt (last parts))}))

(defn read-lines [filename]
  (->> (slurp filename)
       str/split-lines
       (map (fn [s] (str/split s #" -> ")))
       (map (fn [[a b]] {:from (read-coord a)
                        :to   (read-coord b)}))))

(defn endpoints [line axis]
  (map (comp axis second) line))

(defn horizontal? [line]
  (apply = (endpoints line :y)))

(defn vertical? [line]
  (apply = (endpoints line :x)))

(defn inclusive-range [a b]
  (if (< a b)
    (range a (inc b))
    (range a (dec b) -1)))

(defn line-points [line]
  (cond
    (horizontal? line) (map vector
                            (apply inclusive-range (endpoints line :x))
                            (repeat (first (endpoints line :y))))
    (vertical? line)   (map vector
                            (repeat (first (endpoints line :x)))
                            (apply inclusive-range (endpoints line :y)))
    true               (map vector
                            (apply inclusive-range (endpoints line :x))
                            (apply inclusive-range (endpoints line :y)))))

(defn puzzle5-1 []
  (->> (read-lines "resources/input5.txt")
       (filter #(or (horizontal? %) (vertical? %)))
       (mapcat line-points)
       (frequencies)
       (map second)
       (filter (partial < 1))
       count))

(defn puzzle5-2 []
  (->> (read-lines "resources/input5.txt")
       (mapcat line-points)
       (frequencies)
       (map second)
       (filter (partial < 1))
       count))

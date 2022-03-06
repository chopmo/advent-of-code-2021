(ns advent.day11
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn read-octopuses []
  (->> (str/split-lines (slurp "resources/input11.txt"))
       (map (fn [s] (map #(Integer/parseInt %) (str/split s #""))))
       (map vec)
       vec))

(defn num-cols [os]
  (count (first os)))

(defn num-rows [os]
  (count os))

(defn inc-all [os]
  (vec (map #(vec (map inc %)) os)))

(defn inc-value [os x y]
  (update-in os [y x] clojure.core/inc))

(defn reset-value [os x y]
  (update-in os [y x] (constantly 0)))

(defn diff [new old]
  (vec (set/difference (set new) (set old))))

(defn level [os x y]
  (-> os
      (nth y)
      (nth x)))

(defn outside-board [os [x y]]
  (or (< x 0)
      (< y 0)
      (>= x (num-cols os))
      (>= y (num-rows os))))

(defn adjacent [os x y]
  (remove (partial outside-board os)
          [[(dec x) (dec y)]
           [x       (dec y)]
           [(inc x) (dec y)]

           [(dec x) y]
           [(inc x) y]

           [(dec x) (inc y)]
           [x       (inc y)]
           [(inc x) (inc y)]]))

(defn flash [os x y]
  (reduce (fn [os [x y]] (inc-value os x y))
          os
          (adjacent os x y)))


(defn reset-values [os flashers]
  (reduce (fn [os [x y]] (reset-value os x y))
          os
          flashers))

(defn flash-all [os flashers]
  (reduce (fn [os [x y]] (flash os x y))
          os
          flashers))

(defn find-flashers [os]
  (remove nil?
          (for [x (range (num-cols os))
                y (range (num-rows os))]
            (when (> (level os x y) 9)
              [x y]))))

(defn step [[flashes octopuses]]
  (loop [os              (inc-all octopuses)
         flashed []]
    (let [new-flashers (diff (find-flashers os) flashed)]
      (if (empty? new-flashers)
        [(+ flashes (count flashed))
         (reset-values os flashed)]
        (recur (flash-all os new-flashers)
               (concat flashed new-flashers))))))

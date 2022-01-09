(ns advent.day7
  (:require [clojure.string :as str]))

(defn read-numbers [filename]
  (map #(Integer/parseInt %) (str/split (str/trim (slurp filename)) #",")))

;; 7.1
(defn dist [a b]
  (Math/abs (- b a)))

(defn fuel-cost [positions target]
  (apply + (map (partial dist target) positions)))

(defn puzzle7-1 []
  (let [positions (read-numbers "resources/input7.txt")
        fuel-costs (map (fn [pos] [pos (fuel-cost positions pos)])
                        (range (apply min positions) (inc (apply max positions))))]

    (last (first (sort-by second fuel-costs)))))

;; 7.2
(defn increasing-fuel-cost [target position]
  (let [dist (Math/abs (- position target))]
    (apply + (range (inc dist)))))

(defn fuel-cost-sum [positions target]
  (apply + (map (partial increasing-fuel-cost target) positions)))

(defn puzzle7-2 []
  (let [positions (read-numbers "resources/input7.txt")
        fuel-costs (map (fn [pos] [pos (fuel-cost-sum positions pos)])
                        (range (apply min positions) (inc (apply max positions))))]

    (last (first (sort-by second fuel-costs)))))

(ns advent.day4
  (:require [clojure.string :as str]))

(defn read-numbers [lines]
  (map #(Integer/parseInt %) (str/split (first lines) #",")))

(defn read-board [lines]
  (for [line lines]
    (->> (str/split (str/trim line) #"\s+")
         (map #(vector (Integer/parseInt %) false)))))

(defn read-boards [input-lines]
  (->> (partition 5 6 input-lines)
       (map read-board)))

(defn winning-row? [r]
  (->> r
       (map second)
       (every? true?)))

(defn board-rows [b]
  b)

(defn board-cols [b]
  (apply (partial mapv vector) b))

(defn winning-board? [board]
  (->> (concat (board-cols board) (board-rows board))
       (filter winning-row?)
       first
       boolean))

(defn board-score [b num]
  (* num
     (->> b
          (apply concat)
          (filter (complement second))
          (map first)
          (apply +))))

(defn mark-row [n row]
  (map (fn [[num marked]]
         [num (or marked (= num n))])
       row))

(defn mark-board [n board]
  (map (fn [row] (mark-row n row)) board))

(defn winners []
  (let [input-lines (str/split-lines (slurp "resources/input4.txt"))]
    (loop [numbers (read-numbers input-lines)
           winners []
           boards  (read-boards (drop 2 input-lines))]
      (let [n      (first numbers)
            boards (map (partial mark-board n) boards)]
        (if n
          (let [grouped-boards (group-by winning-board? boards)
                winning-boards (get grouped-boards true)
                remaining-boards (get grouped-boards false)]
            (recur (rest numbers)
                   (concat winners (map #(hash-map :board %
                                                   :score (board-score % n))
                                        winning-boards))
                   remaining-boards))
          winners)))))

(defn puzzle4-1 []
  (:score (first (winners))))

(defn puzzle4-2 []
  (:score (last (winners))))

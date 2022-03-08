(ns advent.day13
  (:require [clojure.string :as str]))

(defn read-input [filename]
  (let [lines (str/split-lines (slurp filename))
        coord-lines (take-while #(not (str/blank? %)) lines)
        fold-lines (filter #(re-matches #"fold.*" %) lines)
        parse-coord (fn [s] (map #(Integer/parseInt %) (str/split s #",")))
        parse-fold (fn [s] (let [parts (re-matches #"fold along (.)=(\d+)" s)]
                            [(nth parts 1) (Integer/parseInt (nth parts 2))]))]
    {:coordinates (map parse-coord coord-lines)
     :folds (map parse-fold fold-lines)}))

(defn fold [coords axis line]
  (->> coords
       (map (fn [[x y]]
              (case axis
                "x" (if (> x line) (list (- (* 2 line) x) y) (list x y))
                "y" (if (> y line) (list x (- (* 2 line) y)) (list x y)))))
       distinct))

(defn puzzle13-1 []
  (let [{:keys [coordinates folds]} (read-input "resources/input13.txt")]
    (count (apply fold coordinates (first folds)))))

(defn puzzle13-1 []
  (let [{:keys [coordinates folds]} (read-input "resources/input13.txt")]
    (count (apply fold coordinates (first folds)))))

(defn- ->strings [cs]
  (let [width  (->> cs (map first) sort last inc)
        height (->> cs (map second) sort last inc)
        cs-set (set cs)]
    (map #(str/join (map (fn [x] (if (get cs-set [x %]) "X" " ")) (range width)))
         (range height))))

(defn puzzle13-2 []
  (let [{:keys [coordinates folds]} (read-input "resources/input13.txt")]
    (clojure.pprint/pprint (->strings (reduce (partial apply fold) coordinates folds)))))

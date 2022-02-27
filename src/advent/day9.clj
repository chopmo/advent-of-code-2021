(ns advent.day9
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn read-height-map []
  (->> (str/split-lines (slurp "resources/input9.txt"))
       (map (fn [s] (map #(Integer/parseInt %) (str/split s #""))))))

(defn row-count [m]
  (count m))

(defn col-count [m]
  (count (first m)))

(defn height-at [m x y]
  (cond
    ;; Simulate border of height 10
    (< x 0) 10
    (>= x (count (first m))) 10
    (< y 0) 10
    (>= y (count m)) 10

    :else
    (-> m (nth y) (nth x))))

(defn adjacent [x y]
  [[(dec x) y]
   [(inc x) y]
   [x (dec y)]
   [x (inc y)]])

(defn adjacent-height [m x y]
  [(height-at m (dec x) y)
   (height-at m (inc x) y)
   (height-at m x (dec y))
   (height-at m x (inc y))])

(defn low-point? [m x y]
  (< (height-at m x y)
     (apply min (adjacent-height m x y))))

(defn risk-level [m x y]
  (if (low-point? m x y)
    (inc (height-at m x y))
    0))

(defn puzzle9-1 []
  (let [m (read-height-map)]
    (reduce +
            (for [x (range 100)
                  y (range 100)]
              (risk-level m x y)))))

(defn adj-in-basin [m coords]
  (->> coords
       (mapcat (fn [[x y]] (adjacent x y)))
       (distinct)
       (filter (fn [[x y]] (< (height-at m x y) 9)))))

(defn new-in-basin [m coords]
  (vec (set/difference (set (adj-in-basin m coords))
                       (set coords))))

(defn basin-at [m x y]
  (loop [basin [[x y]]]
    (let [new-coords (new-in-basin m basin)]
      (if (empty? new-coords)
        basin
        (recur (concat basin new-coords))))))

(defn in-basin? [basins x y]
  (get (set (apply concat basins)) [x y]))

(defn basins [m]
  (let [rows (row-count m)
        cols (col-count m)]
    (loop [basins []
           coords (for [x (range cols)
                        y (range rows)]
                    [x y])]
      (if (empty? coords)
        basins
        (let [[x y] (first coords)
              h     (height-at m x y)]
          (if (or (= 9 h)
                  (in-basin? basins x y))
            (recur basins (rest coords))
            (recur (conj basins (basin-at m x y)) (rest coords))))))))

(defn puzzle9-2 []
  (->> (basins (read-height-map))
       (map count)
       (sort)
       (reverse)
       (take 3)
       (reduce *)))

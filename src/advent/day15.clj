(ns advent.day15
  (:require [clojure.string :as str]
            [clojure.data.priority-map :refer [priority-map]]))

(defn read-input [filename]
  (let [lines (str/split-lines (slurp filename))
        parse-line (fn [l] (map #(Integer/parseInt %) (str/split l #"")))]
    (map parse-line lines)))

(defn get-value [grid [x y]]
  (nth (nth grid y) x))

(defn valid-pos? [grid-size [x y]]
  (and (<= 0 x (dec grid-size))
       (<= 0 y (dec grid-size))))

(defn neighbours [grid [x y]]
  (filter (partial valid-pos? (count grid))
          [[x (dec y)]
           [(inc x) y]
           [x (inc y)]
           [(dec x) y]]))

(defn find-cost [grid]
  (let [grid-size   (count grid)
        start       [0 0]
        goal        [(dec grid-size) (dec grid-size)]
        frontier    (atom (priority-map start 0))
        came-from   (atom {start nil})
        cost-so-far (atom {start 0})]
    (loop []
      (when-let [[current _] (first @frontier)]
        (when-not (= goal current)
          (swap! frontier #(dissoc % current))
          (doseq [next (neighbours grid current)]
            (let [new-cost (+ (get @cost-so-far current)
                              (get-value grid next))]
              (when (< new-cost (get @cost-so-far next Integer/MAX_VALUE))
                (swap! cost-so-far #(assoc % next new-cost))
                (swap! frontier #(assoc % next new-cost))
                (swap! came-from #(assoc % next current)))))
          (recur))))

    (get @cost-so-far goal)))

(defn puzzle15-1 []
  (let [grid (read-input "resources/day15.txt")]
    (find-cost grid)))

(defn inc-line [l]
  (map (fn [i] (if (= 9 i) 1 (inc i))) l))

(defn expand-horiz [iterations grid]
  (map
   (fn [line]
     (->> line
          (iterate inc-line)
          (take iterations)
          (apply concat)))
   grid))

(defn expand-vert [iterations grid]
  (->> grid
       (iterate (partial map inc-line))
       (take iterations)
       (apply concat)))

(defn expand-grid [iterations grid]
  (->> grid
       (expand-horiz iterations)
       (expand-vert iterations)))

(defn puzzle15-2 []
  (let [grid (read-input "resources/day15.txt")
        full-grid (expand-grid 5 grid)]
    (find-cost full-grid)))

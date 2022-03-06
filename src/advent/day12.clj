(ns advent.day12
  (:require [clojure.string :as str]))

(defn read-connections [filename]
  (map #(str/split % #"-")
       (str/split-lines (slurp filename))))

(defn connections-from
  "Connections go both ways."
  [connections node]
  (concat
   (map last (filter #(= node (first %)) connections))
   (map first (filter #(= node (last %)) connections))))

(defn small-cave? [node]
  (= node (str/lower-case node)))

(defn visited-small-cave? [path node]
  (and (small-cave? node)
       (get (set path) node)))

(defn find-paths [connections path]
  (let [last-node (last path)]
    (if (= last-node "end")
      [path]
      (mapcat #(find-paths connections (concat path [%]))
              (remove (partial visited-small-cave? path)
                      (connections-from connections last-node))))))
(defn puzzle12-1 []
  (count
   (find-paths (read-connections "resources/input12.txt")
               ["start"])))

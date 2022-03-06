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

(defn valid-path? [allow-one-double-visit? path]
  (let [pruned-path      (filter small-cave? path)
        freqs            (frequencies pruned-path)
        small-cave-freqs (dissoc freqs "start" "end")]
    (and
     ;; start and end visited at most once
     (<= (get freqs "start" 0) 1)
     (<= (get freqs "end" 0) 1)

     ;; No small cave visited more than twice
     (empty? (filter (fn [[_ v]] (> v 2)) small-cave-freqs))

     ;; Maybe allow at most 1 small cave visited twice
     (if allow-one-double-visit?
       (<= (->> small-cave-freqs
                (filter (fn [[_ v]] (= v 2)))
                count)
           1)
       (zero? (->> small-cave-freqs
                   (filter (fn [[_ v]] (> v 1)))
                   count))))))

(defn find-paths [connections path]
  (let [last-node (last path)]
    (if (= last-node "end")
      [path]
      (let [possible-paths (map #(concat path [%])
                                (connections-from connections last-node))]
        (mapcat #(find-paths connections %)
                (filter (partial valid-path? false) possible-paths))))))

(defn puzzle12-1 []
  (count
   (find-paths (read-connections "resources/input12.txt")
               ["start"])))

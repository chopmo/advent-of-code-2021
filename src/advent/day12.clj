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
        small-cave-freqs (dissoc freqs "start" "end")
        visited-more-than (fn [visits]
                            (->> small-cave-freqs
                                 (filter (fn [[_ v]] (> v visits)))
                                 count))]
    (and
     ;; start and end visited at most once
     (<= (get freqs "start" 0) 1)
     (<= (get freqs "end" 0) 1)

     ;; No small cave visited more than twice
     (zero? (visited-more-than 2))

     ;; Maybe allow at most 1 small cave visited twice
     (<= (visited-more-than 1)
         (if allow-one-double-visit? 1 0)))))

(defn find-paths [allow-one-double-visit? connections path]
  (let [last-node (last path)]
    (if (= last-node "end")
      [path]
      (->> (connections-from connections last-node)
           (map #(concat path [%]))
           (filter (partial valid-path? allow-one-double-visit?))
           (mapcat #(find-paths allow-one-double-visit? connections %))))))

(defn puzzle12-1 []
  (count
   (find-paths false
               (read-connections "resources/input12.txt")
               ["start"])))

(defn puzzle12-2 []
  (count
   (find-paths true
               (read-connections "resources/input12.txt")
               ["start"])))

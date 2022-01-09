(ns advent.day3
  (:require [clojure.string :as str]))

;; This one could still use some refactoring but I'd rather press on
;; to the next day.
(defn read-numbers [filename]
  (->> (slurp filename)
       (str/split-lines)
       (map (fn [s] (->> (str/split s #"")
                        (map #(Integer/parseInt %)))))))
(defn puzzle3-1 []
  (let [numbers (read-numbers "resources/input3.txt")
        freqs
        (for [i (range (count (first numbers)))]
          (->> (frequencies (map #(nth % i) numbers))
               (sort-by second)))

        gamma   (BigInteger. (str/join (map (comp first last) freqs)) 2)
        epsilon (BigInteger. (str/join (map (comp first first) freqs)) 2)]

    (int (* gamma epsilon))
    (str/join (map (comp first last) freqs))))


(defn- most-common-bit [numbers pos]
  (let [freqs (->> numbers
                   (map #(nth % pos))
                   (frequencies))]
    (if (> (get freqs 0 0)
           (get freqs 1 0))
      0
      1)))

(defn- least-common-bit [numbers pos]
  (let [freqs (->> numbers
                   (map #(nth % pos))
                   (frequencies))]
    (if (< (get freqs 1 0)
           (get freqs 0 0))
      1
      0)))

(defn- find-diag-rating [numbers bit-criteria]
  (loop [pos     0
         numbers numbers]
    (if (= 1 (count numbers))
      (BigInteger. (str/join (first numbers)) 2)
      (let [bit (bit-criteria numbers pos)]
        (recur (inc pos)
               (filter #(= bit (nth % pos)) numbers))))))

(defn puzzle3-2 []
  (let [numbers (read-numbers "resources/input3.txt")
        oxygen (find-diag-rating numbers most-common-bit)
        co2    (find-diag-rating numbers least-common-bit)]
    (int (* oxygen co2))))

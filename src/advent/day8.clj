(ns advent.day8
  (:require [clojure.string :as str]
            [clojure.core.logic :as l]))

(defn lines []
  (str/split-lines (slurp "resources/input8.txt")))

(defn inputs []
  (->> (str/split-lines (slurp "resources/input8.txt"))
       (map #(str/split % #"\|"))
       (map (fn [[patterns outputs]]
              {:patterns (str/split (str/trim patterns) #"\s+")
               :outputs  (str/split (str/trim outputs) #"\s+")}))))

(defn puzzle8-1 []
  (->> (inputs)
       (map :outputs)
       flatten
       (map count)
       (filter #{2 3 4 7})
       count))

(defn patterns-sized [patterns n]
  (->> patterns
       (map seq)
       (filter #(= n (count %)))
       (map sort)))

(defn pattern-sized [patterns n]
  (first (patterns-sized patterns n)))


;; I'm sure all the permuteo's could have been written with less
;; duplication, but let's call that out of scope :)
(defn translation-key
  "Find the letters that abcdefg map to given the complete list of 10 patterns."
  [patterns]
  (first
   (l/run* [a b c d e f g]
     ;; 0
     (l/conde [(l/permuteo [a b c e f g] (nth (patterns-sized patterns 6) 0))]
              [(l/permuteo [a b c e f g] (nth (patterns-sized patterns 6) 1))]
              [(l/permuteo [a b c e f g] (nth (patterns-sized patterns 6) 2))])

     ;; 1
     (l/permuteo [c f] (pattern-sized patterns 2))

     ;; 2
     (l/conde [(l/permuteo [a c d e g] (nth (patterns-sized patterns 5) 0))]
              [(l/permuteo [a c d e g] (nth (patterns-sized patterns 5) 1))]
              [(l/permuteo [a c d e g] (nth (patterns-sized patterns 5) 2))])

     ;; 3
     (l/conde [(l/permuteo [a c d f g] (nth (patterns-sized patterns 5) 0))]
              [(l/permuteo [a c d f g] (nth (patterns-sized patterns 5) 1))]
              [(l/permuteo [a c d f g] (nth (patterns-sized patterns 5) 2))])

     ;; 4
     (l/permuteo [b c d f] (pattern-sized patterns 4))

     ;; 7
     (l/permuteo [a c f] (pattern-sized patterns 3)))))

;; Much faster version, based on an algorithm I jotted down on a piece
;; of paper long ago.
(defn faster-translation-key
  "Find the letters that abcdefg map to given the complete list of 10 patterns."
  [patterns]
  (let [freqs (frequencies (str/join patterns))
        letter-with-freq (fn [n]
                           (->> freqs
                                (filter #(= n (second %)))
                                first
                                key))]
    (first
     (l/run* [a b c d e f g]
       ;; f b e
       (l/== f (letter-with-freq 9))
       (l/== b (letter-with-freq 6))
       (l/== e (letter-with-freq 4))

       ;; c
       (l/permuteo [c f] (pattern-sized patterns 2))

       ;; a
       (l/permuteo [a c f] (pattern-sized patterns 3))

       ;; d
       (l/permuteo [b c d f] (pattern-sized patterns 4))

       ;; g
       (l/permuteo [a b c d e f g] (pattern-sized patterns 7))))))

(defn translation-map [patterns]
  (zipmap (faster-translation-key patterns)
          [\a \b \c \d \e \f \g]))

(defn digit [segments]
  (case (str/join (sort segments))
    "abcefg"  0
    "cf"      1
    "acdeg"   2
    "acdfg"   3
    "bcdf"    4
    "abdfg"   5
    "abdefg"  6
    "acf"     7
    "abcdefg" 8
    "abcdfg"  9))

(defn translate [translation-map output]
  (map translation-map output))

(defn find-number [patterns outputs]
  (let [tm (translation-map patterns)]
    (->> outputs
         (map seq)
         (map (partial translate tm))
         (map digit)
         (str/join)
         (Integer/parseInt))))

;; Takes about 4 secs
(defn puzzle8-2 []
  (time
   (->> (inputs)
        (map (fn [{:keys [patterns outputs]}] (find-number patterns outputs)))
        (reduce +))))

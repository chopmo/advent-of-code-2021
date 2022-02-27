(ns advent.day10
  (:require [clojure.string :as str]))

(defn navigation-subsystem []
  (str/split-lines (slurp "resources/input10.txt")))

(def pairs
  {\( \)
   \[ \]
   \{ \}
   \< \>})

(defn opener? [c]
  (pairs c))

(defn matching? [opener closer]
  (= closer (pairs opener)))

;; Would have been nicer to avoid deep nesting using cond.
(defn find-corruption [s]
  (loop [chars (seq s)
         stack nil]
    (when (seq chars)
      (let [c (first chars)]
        (if (opener? c)
          (recur (rest chars) (conj stack c))
          (let [expected-opener (first stack)]
            (if (matching? expected-opener c)
              (recur (rest chars) (rest stack))
              c)))))))

(defn find-completion [s]
  (loop [chars (seq s)
         stack nil]
    (if (empty? chars)
      (map pairs stack)
      (let [c (first chars)]
        (if (opener? c)
          (recur (rest chars) (conj stack c))
          (recur (rest chars) (rest stack)))))))

(defn illegal-char-score [c]
  (get {\) 3
        \] 57
        \} 1197
        \> 25137}
       c))

(defn closer-score [c]
  (get {\) 1
        \] 2
        \} 3
        \> 4}
       c))

(defn puzzle10-1 []
  (->> (navigation-subsystem)
       (map find-corruption)
       (remove nil?)
       (map illegal-char-score)
       (reduce +)))

(defn score-completion [closers]
  (loop [score 0
         chars closers]
    (if (empty? chars)
      score
      (recur (+ (* 5 score)
                (closer-score (first chars)))
             (rest chars)))))

(defn puzzle10-2 []
  (let [scores (->> (navigation-subsystem)
                    (remove find-corruption)
                    (map find-completion)
                    (map score-completion)
                    sort)]
    (nth scores (/ (count scores) 2))))

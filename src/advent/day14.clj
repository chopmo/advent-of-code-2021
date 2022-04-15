(ns advent.day14
  (:require [clojure.string :as str]
            [taoensso.tufte :as tufte :refer (defnp p profile)]))

(tufte/add-basic-println-handler!
  {:format-pstats-opts {:columns [:n-calls :max :mean :mad :clock :total]}})

(defn parse-rule [s]
  (let [match (re-find #"(.+) -> (.+)" s)]
    [(seq (nth match 1))
     (first (nth match 2))]))


(defn read-input [filename]
  (let [lines (str/split-lines (slurp filename))]
    {:template (first lines)
     :rules (into {} (map parse-rule (drop 2 lines)))}))

(defn do-insert [rules s]
  (if-let [letter (get rules s)]
    (list (first s) letter (last s))
    s))

(defn step [rules template]
  (->> template
       (partition 2 1)
       (map (partial do-insert rules))
       (mapcat butlast)
       (apply str)
       (#(str %1 (last template)))))

(defn puzzle14-1 []
  (let [{:keys [template rules]} (read-input "resources/day14.txt")
        polymer (last (take 11 (iterate (partial step rules) template)))
        freqs (sort-by second (frequencies polymer))]
    (- (-> freqs last second)
       (-> freqs first second))))

(defn step2 [rules template]
  (let [polymer (transient [])]
    (doseq [i (range (dec (count template)))]
      (let [a       (p ::nth1 (nth template i))
            b       (p ::nth2 (nth template (inc i)))
            element (p ::get (get rules [a b]))]
        (p ::conj1 (conj! polymer a))
        (p ::conj2 (when element (conj! polymer element)))))
    (conj! polymer (nth template (dec (count template))))
    (persistent! polymer)))

(defn step3 [rules template]
  (let [polymer (char-array (* 2 (count template)))
        template-ar (char-array template)
        last-idx (dec (count template))]
    (loop [idx 0
           cursor 0
           prev-b nil]
      (if (= idx last-idx)
        (do
          (aset-char polymer cursor (nth template idx))
          (take (inc cursor) polymer))
        (let [a (p ::nth1 (or prev-b (get template-ar idx)))
              b (p ::nth2 (get template-ar (inc idx)))
              element (p ::get (get rules [a b]))]
          (if element
            (do
              (p ::insert
                 (do
                   (aset-char polymer cursor a)
                   (aset-char polymer (inc cursor) element)))
              (recur (inc idx)
                     (+ cursor 2)
                     b))
            (do
              (p ::no-insert (aset-char polymer cursor a))
              (recur (inc idx)
                     (inc cursor)
                     b))))))))

;; NOTE: There is always a matching rule.
(defn step4 [rules template]
  (let [polymer  (p ::alloc-array (char-array (* 2 (count template))))
        last-idx (dec (p ::count-template (count template)))
        idx      (atom 0)
        cursor   (atom 0)]
    (doseq [[a b] (p ::partition (partition 2 1 template))]
      (let [element (p ::get (get rules [a b]))]
        (p ::first-aset (aset-char polymer @cursor a))
        (p ::first-swap (swap! cursor inc))
        (p ::second-aset (aset-char polymer @cursor element))
        (p ::second-swap (swap! cursor inc))))
    (p ::last-aset
       (aset-char polymer @cursor (last template)))
    (p ::take
       (take (inc @cursor) polymer))))


(defn puzzle14-2 []
  (let [{:keys [template rules]} (read-input "resources/day14.txt")
        polymer                  (last (take 41 (iterate (partial step4 rules) (vec template))))
        freqs                    (sort-by second (frequencies polymer))]
    (- (-> freqs last second)
       (-> freqs first second))))

(comment
  (profile {} (puzzle14-2))

  (let [{:keys [template rules]} (read-input "resources/day14-example.txt")
        polymers                  (take 3 (iterate (partial step4 rules) (vec template)))
        ]
    polymers)


  ,)

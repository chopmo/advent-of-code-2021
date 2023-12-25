(ns advent.day20
  (:require [clojure.string :as str]))

(defn parse-line [s]
  (map #(if (= % \#) 1 0) s))

(defn read-input [filename]
  (let [lines (str/split-lines (slurp filename))]
    {:algo (parse-line (first lines))
     :image (map parse-line (drop 2 lines))}))

(defn num-rows [image]
  (count image))

(defn num-cols [image]
  (count (first image)))

(defn pixel [image x y]
  (if (or (< x 0)
          (>= x (num-cols image))
          (< y 0)
          (>= y (num-rows image)))
    0
    (nth (nth image y) x)))

(defn binary-to-int [xs]
  (BigInteger. (apply str xs) 2))

(defn pixel-box [image x y]
  (for [y' [(dec y) y (inc y)]
        x' [(dec x) x (inc x)]]
    (pixel image x' y')))

(defn enhance-pixel [image algo x y]
  (let [num (binary-to-int (pixel-box image x y))]
    (nth algo num)))

(defn enhance-image [algo image]
  (for [y (range -10 (+ (num-rows image) 10))]
    (for [x (range -10 (+ (inc (num-cols image)) 10))]
      (enhance-pixel image algo x y))
    ))



;; This does not work because the canvas is infinite. On the first iteration all pixels outside the image will turn white, on the next they will turn black. But what happens at the border?

;; Can I pad the image with N lines, then remove some of them again?

(defn trim-image [n image]
  (->> image
       (take (- (count image) n))
       (drop n)
       (map (fn [l] (drop n (take (- (count l) n) l))))))

(let [{:keys [algo image]} (read-input "resources/day20.txt")]
  (->> (enhance-image algo image)
       (enhance-image algo)
       (trim-image 10)
       (mapcat identity)
       (filter pos?)
       count))

(def image (:image (read-input "resources/day20-example.txt")))

(def algo (:algo (read-input "resources/day20-example.txt")))

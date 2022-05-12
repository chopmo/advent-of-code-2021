(ns advent.day19
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as combo]
            [clojure.test :refer [deftest is run-tests]]
            [clojure.math :as math]))

(defn parse-coordinate [s]
  (map #(Integer/parseInt %) (str/split s #",")))

(defn read-input [filename]
  (->> (slurp filename)
       (str/split-lines)
       (filter seq)
       (filter (complement (partial re-matches #"---.*")))
       (map parse-coordinate)
       (partition 25)))

(defn dist [[x1 y1 z1] [x2 y2 z2]]
  [(- x2 x1)
   (- y2 y1)
   (- z2 z1)])


;; Z points inwards
;; Positive angles mean CCW rotation
(defn rot-z [[x y z] r]
  [(math/round (- (* x (math/cos r)) (* y (math/sin r))))
   (math/round (+ (* x (math/sin r)) (* y (math/cos r))))
   z])

(defn rot-y [[x y z] r]
  [(math/round (+ (* x (math/cos r)) (* z (math/sin r))))
   y
   (math/round (+ (* -1 x (math/sin r)) (* z (math/cos r))))])

(defn rot-x [[x y z] r]
  [x
   (math/round (- (* y (math/cos r)) (* z (math/sin r))))
   (math/round (+ (* y (math/sin r)) (* z (math/cos r))))])


(defn z-orientations [p]
  (for [i (range 4)]
    (rot-z p (* i (/ math/PI 2)))))

(defn transformations [p]
  (let [pi-halves  (/ math/PI 2)
        viewpoints [p                          ;; Look into z
                    (rot-y p pi-halves)        ;; Look left on x
                    (rot-y p (* -1 pi-halves)) ;; Look right on x
                    (rot-y p (* 2 pi-halves))  ;; Look out of z
                    (rot-x p pi-halves)        ;; Look up on Y
                    (rot-x p (* -1 pi-halves)) ;; Look down on Y
                    ]]
    ;;(mapcat z-orientations viewpoints)
    viewpoints
    ))
(comment
  (transformations [1 2 3])
  (z-orientations [1 1 1])
  (rot-z [1 1 0] (/ math/PI 2))
  (rot-x [0 0 1] (/ math/PI 2))
  (rot-y [1 0 0] (/ math/PI 2))


  ,)

#_(def transformations
  [(fn [x y z] [ x  y  z]) ; 0 deg
   (fn [x y z] [-y  x  z]) ; 90 deg
   (fn [x y z] [-x -y  z]) ; 180 deg
   (fn [x y z] [ y -x  z]) ; 270 deg

;; x':
;; x: 1 0 -1 0
;; y: 0 -1 0 1
;; z: 0 0 0 0

;; y':
;; x: 0 1 0 -1
;; y: 1 0 -1 0
;; z: 0 0 0 0

;; z':
;; x: 0 0 0 0
;; y: 0 0 0 0
;; z: 1 1 1 1

   (fn [x y z] [-x y -z])
   (fn [x y z] [])
   (fn [x y z] [x y -z])
   (fn [x y z] [x y -z])
   (fn [x y z] [x y z])
   (fn [x y z] [x y z])
   (fn [x y z] [x y z])
   (fn [x y z] [x y z])
   (fn [x y z] [x y z])
   (fn [x y z] [x y z])
   (fn [x y z] [x y z])
   (fn [x y z] [x y z])
   (fn [x y z] [x y z])
   (fn [x y z] [x y z])])

(defn puzzle19-1 []
  (let [scanners (read-input "resources/day19-example.txt")]
    scanners))

(deftest dist-test
  (is (= [1 2 3] (dist [0 0 0] [1 2 3])))
  (is (= [1 -8 2] (dist [-1 5 7] [0 -3 9]))))


(comment

  ,)

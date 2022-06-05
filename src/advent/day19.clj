(ns advent.day19
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as combo]
            [clojure.test :refer [deftest is run-tests]]
            [clojure.math :as math]))

(defn parse-coordinate [s]
  (map #(Integer/parseInt %) (str/split s #",")))

(defn read-input [filename]
  ;; This is so ugly.
  (->> (slurp filename)
       (str/split-lines)
       (filter #(not (re-matches #"---.*" %)))
       (partition-by str/blank?)
       (filter #(> (count %) 1))
       (map #(map parse-coordinate %))))

(defn dist [[x1 y1 z1] [x2 y2 z2]]
  [(- x2 x1)
   (- y2 y1)
   (- z2 z1)])

;; Z points inwards
;; Positive angles mean CCW rotation
(defn rot-z [r [x y z]]
  [(math/round (- (* x (math/cos r)) (* y (math/sin r))))
   (math/round (+ (* x (math/sin r)) (* y (math/cos r))))
   z])

(defn rot-y [r [x y z]]
  [(math/round (+ (* x (math/cos r)) (* z (math/sin r))))
   y
   (math/round (+ (* -1 x (math/sin r)) (* z (math/cos r))))])

(defn rot-x [r [x y z]]
  [x
   (math/round (- (* y (math/cos r)) (* z (math/sin r))))
   (math/round (+ (* y (math/sin r)) (* z (math/cos r))))])

(def z-orientation-fns
  (for [i (range 4)]
    (partial rot-z (* i (/ math/PI 2)))))

(defn negate-coord [c]
  (map #(* -1 %) c))

(def viewpoint-fns
  (let [pi-halves (/ math/PI 2)]
    [identity                      ;; Look into z
     (partial rot-y pi-halves)     ;; Look left on x
     (partial rot-y (* -1 pi-halves)) ;; Look right on x
     (partial rot-y (* 2 pi-halves))  ;; Look out of z
     (partial rot-x pi-halves)        ;; Look up on Y
     (partial rot-x (* -1 pi-halves)) ;; Look down on Y
     ]))

(def transformation-fns
  (for [viewpoint-fn     viewpoint-fns
        z-orientation-fn z-orientation-fns]
    (comp z-orientation-fn viewpoint-fn)))

(defn find-distance [beacons1 beacons2]
  (let [beacon-dists (for [b1 beacons1 b2 beacons2] (dist b1 b2))
        freqs        (frequencies beacon-dists)]
    (->> freqs
         (filter #(>= (second %) 12))
         first
         first)))

(defn all-distances [bs1 bs2]
  (for [b1 bs1 b2 bs2]
    [(dist b1 b2) b1 b2]))

(defn find-matching-transformation-fn [world beacons]
  (->> transformation-fns
       (filter (fn [t-fn] (some? (find-distance world (map t-fn beacons)))))
       first))

(defn transform-and-find-distance [bs1 bs2]
  (when-let [t-fn (find-matching-transformation-fn bs1 bs2)]
    (find-distance bs1 (map t-fn bs2))))

(defn overlapping? [world beacons]
  (some? (find-matching-transformation-fn world beacons)))

(defn translate [[dx dy dz] [x y z]]
  [(+ x dx) (+ y dy) (+ z dz)])

(defn merge-beacons [world beacons]
  (let [transformation-fn (find-matching-transformation-fn world beacons)
        reoriented-beacons (map transformation-fn beacons)
        distance (find-distance reoriented-beacons world)
        translated-beacons (map (partial translate distance) reoriented-beacons)]
    (clojure.set/union world (set translated-beacons))))

(defn merge-scanners [world scanner-positions scanner-q]
  (if (empty? scanner-q)
    {:count (count world)
     :positions scanner-positions}
    (let [scanner (peek scanner-q)]
      (if-let [dist (transform-and-find-distance world scanner)]
        (merge-scanners (merge-beacons world scanner)
                        (conj scanner-positions (negate-coord dist))
                        (pop scanner-q))
        (merge-scanners world
                        scanner-positions
                        (conj (pop scanner-q) scanner))))))


;;; ********* TESTS *********

(deftest dist-test
  (is (= [1 2 3] (dist [0 0 0] [1 2 3])))
  (is (= [1 -8 2] (dist [-1 5 7] [0 -3 9]))))

(deftest overlapping?-test
  (let [scanners (read-input "resources/day19-example.txt")]
    (is (true? (overlapping? (nth scanners 0)
                             (nth scanners 1))))))

(deftest find-distance-test
  (let [scanners (read-input "resources/day19-example.txt")
        world (nth scanners 0)
        scanner1 (nth scanners 1)
        t-fn (find-matching-transformation-fn world scanner1)]
    (is (= [68 -1246 -43]
           (find-distance (map t-fn scanner1) world)))))

(defn puzzle19-1 []
  (let [scanners (read-input "resources/day19.txt")]
    (merge-scanners (set (first scanners))
                    (apply conj
                           (clojure.lang.PersistentQueue/EMPTY)
                           (rest scanners)))))

(defn manhattan [[x1 y1 z1] [x2 y2 z2]]
  (+ (Math/abs (- x2 x1))
     (Math/abs (- y2 y1))
     (Math/abs (- z2 z1))))

(defn puzzle19-2 []
  (let [scanners (read-input "resources/day19.txt")
        positions (:positions
                   (merge-scanners (set (first scanners))
                    [[0 0 0]]
                    (apply conj
                           (clojure.lang.PersistentQueue/EMPTY)
                           (rest scanners))))]
    (-> (for [p1 positions
              p2 positions]
          (manhattan p1 p2))
        sort
        last)))

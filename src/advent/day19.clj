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


(defn puzzle19-1 []
  (let [scanners (read-input "resources/day19-example.txt")
        s0 (first scanners)]
    (map (second transformation-fns) (first scanners))
    ))

(deftest dist-test
  (is (= [1 2 3] (dist [0 0 0] [1 2 3])))
  (is (= [1 -8 2] (dist [-1 5 7] [0 -3 9]))))


(comment
  (let [scanners (read-input "resources/day19-all-matching.txt")
        s0       (first scanners)
        s1       (second scanners)]

    #_(for [f transformation-fns]
      (sort (map dist s0 (map f s1))))

    s0
    )
  ,)


(defn scanner-dist [s1 s2]
  (let [beacon-dists (map dist s1 s2)
        freqs (frequencies beacon-dists)]
    (->> freqs
         (filter #(>= (second %) 12))
         first
         first)
    ))

(comment
  (let [match0 '((-618,-824,-621)
                (-537,-823,-458)
                (-447,-329,318)
                (404,-588,-901)
                (544,-627,-890)
                (528,-643,409)
                (-661,-816,-575)
                (390,-675,-793)
                (423,-701,434)
                (-345,-311,381)
                (459,-707,401)
                (-485,-357,347))

        match1 '((686,422,578)
                (605,423,415)
                (515,917,-361)
                (-336,658,858)
                (-476,619,847)
                (-460,603,-452)
                (729,430,532)
                (-322,571,750)
                (-355,545,-477)
                (413,935,-424)
                (-391,539,-444)
                (553,889,-390))

        ]
    (for [f transformation-fns]
                (scanner-dist (map f match1) match0))

    )
  ,)


(defn find-distance [beacons1 beacons2]
  (let [beacon-dists (for [b1 beacons1 b2 beacons2] (dist b1 b2))
        freqs        (frequencies beacon-dists)]
    (->> freqs
         (filter #(>= (second %) 12))
         first
         first)
    ))

(defn find-matching-transformation-fn [world beacons]
  (->> transformation-fns
       (filter (fn [t-fn] (some? (find-distance world (map t-fn beacons)))))
       first))

(defn overlapping? [world beacons]
  (some? (find-matching-transformation-fn world beacons)))

(defn translate [[dx dy dz] [x y z]]
  [(+ x dx) (+ y dy) (+ z dz)])

(defn merge-beacons [world beacons]
  (let [transformation-fn (find-matching-transformation-fn world beacons)
        reoriented-beacons (map transformation-fn beacons)
        distance (find-distance reoriented-beacons world)
        translated-beacons (map (partial translate distance) reoriented-beacons)]
    (clojure.set/union world (set translated-beacons)))
  )


(comment
  (let [q (clojure.lang.PersistentQueue/EMPTY)]
    (conj q 123))

  (def q (clojure.lang.PersistentQueue/EMPTY))
  (def q (conj q 4 5))
  (seq (pop q))

  ;; conj
  ;; peek
  ;; pop
  ,)


(deftest overlapping?-test
  (let [scanners (read-input "resources/day19-example.txt")]
    (is (true? (overlapping? (nth scanners 0)
                             (nth scanners 1))))

    )
  )

(deftest find-distance-test
  (let [scanners (read-input "resources/day19-example.txt")
        world (nth scanners 0)
        scanner1 (nth scanners 1)
        t-fn (find-matching-transformation-fn world scanner1)]
    (is (= [68 -1246 -43]
           (find-distance (map t-fn scanner1) world)))

    )
  )

(deftest merge-beacons-test
  (let [scanners (read-input "resources/day19-example.txt")
        world (set (nth scanners 0))]
    (-> world
        (merge-beacons (nth scanners 1))
        ;(merge-beacons (nth scanners 4))
        ;(merge-beacons (nth scanners 2))
        ;(merge-beacons (nth scanners 3))
        )
    (overlapping? (nth scanners 1)
                  (nth scanners 4))

    )

  )

(comment
  (let [scanners (read-input "resources/day19-example.txt")]
    (overlapping? (nth scanners 0)
                  (map (nth transformation-fns 3)  (nth scanners 1))))
  ,)


(comment
  (let [scanners (read-input "resources/day19-example.txt")]
    (overlapping? (nth scanners 1)
                  (nth scanners 4)))
  ,)

#_([-68 1246 43]
 [-68 1246 43]
 [-68 1246 43]
 [-68 1246 43]
 [-68 1246 43]
 [-68 1246 43]
 [-68 1246 43]
 [-68 1246 43]
 [-68 1246 43]
 [-68 1246 43]
 [-68 1246 43]
 [-68 1246 43])

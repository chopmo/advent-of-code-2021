(ns advent.day18
  (:require [clojure.test :refer [deftest is]]
            [clojure.string :as str]
            [clojure.math.combinatorics :as comb]))

(defn read-input [filename]
  (str/split-lines (slurp filename)))

(defn parse-snum [s]
  (reduce (fn [agg c]
            (case c
              \, agg
              \[ (conj agg \[)
              \] (conj agg \])
              (conj agg (Integer/parseInt (str c)))))
          []
          s))

(defn add-snums [n1 n2]
  (concat [\[]
          n1
          n2
          [\]]))

(defn find-exploder [snum]
  (loop [idx   0
         depth 0]
    (if (= 5 depth)
      {:before         (take (dec idx) snum)
       :exploder-left  (nth snum idx)
       :exploder-right (nth snum (inc idx))
       :after          (drop (+ idx 3) snum)}
      (when (< idx (dec (count snum)))
        (let [c (nth snum idx)]
          (recur (inc idx)
                 (cond
                   (= \[ c) (inc depth)
                   (= \] c) (dec depth)
                   :else    depth)))))))

(defn add-to-leftmost [snum n]
  (if-let [int-idx (first (keep-indexed (fn [idx c] (when (int? c) idx)) snum))]
    (update (vec snum) int-idx (partial + n))
    snum))

(defn add-to-rightmost [snum n]
  (-> snum
      reverse
      (add-to-leftmost n)
      reverse))

(defn explode [snum]
  (let [{:keys [before after exploder-left exploder-right] :as exploder}
        (find-exploder snum)]
    (assert exploder "No exploder found")
    (concat (add-to-rightmost before exploder-left)
            [0]
            (add-to-leftmost after exploder-right))))

(defn find-split-idx [snum]
  (first
   (keep-indexed (fn [idx c] (when (and (int? c) (>= c 10)) idx))
                 snum)))

(defn split [snum]
  (let [idx (find-split-idx snum)]
    (assert idx "No split found")
    (let [n     (nth snum idx)
          left  (int (/ n 2))
          right (- n left)]
      (concat (take idx snum)
              [\[ left right \]]
              (drop (inc idx) snum)))))

(defn reduce-snum [snum]
  (cond (find-exploder snum) (reduce-snum (explode snum))
        (find-split-idx snum) (reduce-snum (split snum))
        :else snum))

(defn magnitude [snum]
  (let [vec-num (clojure.edn/read-string (str/join " " snum))
        mag (fn mag [[a b]]
              (+ (* 3 (if (int? a) a (mag a)))
                 (* 2 (if (int? b) b (mag b)))))]
    (mag vec-num)))

(defn puzzle18-1 []
  (->> (read-input "resources/day18.txt")
       (map parse-snum)
       (reduce (fn [agg snum] (reduce-snum (add-snums agg snum))))
       magnitude))

(defn puzzle18-2 []
  (let [snums (map parse-snum (read-input "resources/day18.txt"))]
    (->> (comb/combinations snums 2)
         (mapcat (fn [[n1 n2]] [(add-snums n1 n2)
                               (add-snums n2 n1)]))
         (map reduce-snum)
         (map magnitude)
         (apply max))))

(deftest parse-snum-test
  (is (= [\[ \[ 1 2 \] 3 \]] (parse-snum "[[1,2],3]")))
  (is (= [\[ 1 2 \]] (parse-snum "[1,2]"))))

(deftest add-snums-test
  (is (= (parse-snum "[[1,2],[[3,4],5]]")
         (add-snums (parse-snum "[1,2]")
                    (parse-snum "[[3,4],5]")))))

(deftest find-exploder-test
  (is (nil?
       (find-exploder (parse-snum "[[[[9,8],1],2],3]"))))

  (is (= {:before (parse-snum "[[[[")
          :exploder-left 9
          :exploder-right 8
          :after (parse-snum ",1],2],3],4]")}
         (find-exploder (parse-snum "[[[[[9,8],1],2],3],4]")))))

(deftest add-to-leftmost-test
  (is (= (parse-snum "],],],]")
         (add-to-leftmost (parse-snum (parse-snum "],],],]")) 8)))
  (is (= (parse-snum "9],2],3],4]")
         (add-to-leftmost (parse-snum ",1],2],3],4]") 8))))

(deftest add-to-rightmost-test
  (is (= (parse-snum "1],2],3],8]")
         (add-to-rightmost (parse-snum ",1],2],3],4]") 4))))

(deftest explode-test
  (is (= (parse-snum "[[[[0,9],2],3],4]")
         (explode (parse-snum "[[[[[9,8],1],2],3],4]"))))

  (is (= (parse-snum "[7,[6,[5,[7,0]]]]")
         (explode (parse-snum "[7,[6,[5,[4,[3,2]]]]]"))))

  (is (= (parse-snum "[[6,[5,[7,0]]],3]")
         (explode (parse-snum "[[6,[5,[4,[3,2]]]],1]")))))

(deftest find-split-idx-test
  (is (= 10 (find-split-idx [\[ \[ \[ \[ 0 7 \] 4 \] \[ 15 \[ 0 13 \] \] \] \[ 1 1 \]]))))

(deftest split-test
  (is (= [\[ \[ \[ \[ 0 7 \] 4 \] \[ \[ 7 8 \] \[ 0 13 \] \] \] \[ 1 1 \]]
         (split [\[ \[ \[ \[ 0 7 \] 4 \] \[ 15 \[ 0 13 \] \] \] \[ 1 1 \]]))))

(deftest reduce-test
  (is (= (parse-snum "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]")
         (reduce-snum (parse-snum "[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]")))))

(deftest magnitude-test
  (is (= 143 (magnitude (parse-snum "[[1,2],[[3,4],5]]"))))
  (is (= 1384 (magnitude (parse-snum "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]")))))

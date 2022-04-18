(ns advent.day16
  (:require [clojure.string :as str]))

(defn char-to-binary [s]
  (let [bin-string (.toString (BigInteger. s 16) 2)
        padded-string (str
                       (apply str (repeat (- 4 (count bin-string)) "0"))
                       bin-string)]
    (map #(Integer/parseInt %) (str/split padded-string #""))))

(defn binary-to-int [xs]
  (BigInteger. (apply str xs) 2))

(defn parse-input [hex]
  (mapcat char-to-binary (str/split hex #"")))

(defn read-input [filename]
  (let [hex (str/trim (slurp filename))]
    (parse-input hex)))


(defn drop-bits [n stream]
  (swap! stream (partial drop n)))

(defn read-literal [stream]
  (let [read-chunk
        (fn [stream]
          (let [ch (take 4 (drop 1 @stream))]
            (drop-bits 5 stream)
            ch))]
    (loop [bits []]
      (if (= 1 (first @stream))
        (recur (concat bits (read-chunk stream)))
        {:literal (binary-to-int (concat bits (read-chunk stream)))}))))

(defn read-bits [n stream]
  (let [bits (take n @stream)]
    (drop-bits n stream)
    bits))

(defn read-integer [n stream]
  (let [i (binary-to-int (take n @stream))]
    (swap! stream (partial drop n))
    i))

(def read-packet)

(defn read-operator [stream]
  (let [mode (read-integer 1 stream)]
    (if (= 0 mode)
      (let [bit-len (read-integer 15 stream)]
        (let [substream (atom (take bit-len @stream))]
          (drop-bits bit-len stream)
          (loop [subpackets []]
            (if (empty? @substream)
              {:length-type-id 0
               :bit-length bit-len
               :subpackets subpackets}
              (recur (conj subpackets (read-packet substream)))))))
      (let [num-packets (read-integer 11 stream)]
        {:length-type-id 1
         :num-packets num-packets
         :subpackets (doall (for [i (range num-packets)] (read-packet stream)))}))))

(defn read-packet [stream]
  (let [version (read-integer 3 stream)
        ptype (read-integer 3 stream)]
    (if (= 4 ptype)
      (merge
       {:version version
        :type    ptype}
       (read-literal stream))
      (merge
       {:version version
        :type    ptype}
       (read-operator stream)))))

(defn sum-versions [p]
  (+ (:version p)
     (reduce + (map sum-versions (:subpackets p)))))

(defn puzzle16-1 []
  (let [stream (atom (read-input "resources/day16.txt"))]
    (sum-versions (read-packet stream))))

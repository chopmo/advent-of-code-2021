(ns advent.day17)

;; Puzzle input: target area: x=241..273, y=-97..-63

(defn puzzle17-1 []
  ;; No need for code on day one, just some hammock time.
  ;;
  ;; I'm ignoring the X value because the task is just to find the
  ;; maximum Y.
  ;;
  ;; Pretend we're shooting straight up. No matter what Y velocity we
  ;; choose, there is a step where the probe is back to position 0. We
  ;; want it to be travelling at the max possible velocity. So to
  ;; avoid overshooting the target, it must travel a maximum of 97
  ;; units on the next step. Meaning that the velocity must have been
  ;; 96 on the step where it came back to position 0. Which again
  ;; means that this is the velocity with which we fired it.
  ;;
  ;; So calculating the max height can be done as (96 + 95 +
  ;; 94....). We can use the Gauss formula here:
  ;; https://study.com/academy/lesson/finding-the-sum-of-consecutive-numbers.html
  )

(defn next-velocity [[dx dy]]
  (let [new-dx (cond (> dx 0) (dec dx)
                     (< dx 0) (inc dx)
                     :else    0)
        new-dy (dec dy)]
    [new-dx new-dy]))

(defn trajectory
  ([velocity]
   (trajectory [0 0] velocity))
  ([[x y :as pos] [dx dy :as velocity]]
   (lazy-seq
    (cons pos (trajectory [(+ x dx) (+ y dy)]
                          (next-velocity velocity))))))

(defn within-area? [{:keys [min-x min-y max-x max-y]} [x y]]
  (and (<= min-x x max-x)
       (<= min-y y max-y)))

(defn hit? [target-area velocity]
  (->> (trajectory velocity)
       (take-while (fn [[_ y]] (>= y (:min-y target-area))))
       (filter (partial within-area? target-area))
       first
       boolean))

(defn puzzle17-2 []
  (let [;; Example area
        ;; target-area {:min-x 20 :max-x 30 :min-y -10 :max-y -5}

        ;; Puzzle area
        target-area {:min-x 241 :max-x 273 :min-y -97 :max-y -63}
        max-dx      (:max-x target-area)
        min-dx      (- max-dx)
        max-dy      (Math/abs (:min-y target-area))
        min-dy      (- max-dy)]

    (->> (for [dx (range min-dx (inc max-dx))
               dy (range min-dy (inc max-dy))]
           [dx dy])
         (filter (partial hit? target-area))
         count)))

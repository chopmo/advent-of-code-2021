(ns advent.day17)

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

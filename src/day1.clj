(ns day1
  (:require
   [clojure.java.io :as io]
   [parse :as parse]))

(def test-input "3   4
4   3
2   5
1   3
3   9
3   3")

;; Part 1
(defn num-diffs [input]
  (->> input
       parse/parse-int-mat
       (apply map vector)
       (map sort)
       (apply map vector)
       (map #(apply - %))
       (map abs)
       (apply +)))

(assert (= 11 (num-diffs test-input)))
(num-diffs (slurp (io/resource "day1.txt")))

;; Part 2
(defn sum-frequency [input]
  (let [[l r] (->> input
                   parse/parse-int-mat
                   (apply map vector)
                   (map frequencies))]
    (->> l
         (map (fn [[num freq]] (* num freq (get r num 0))))
         (apply +))))

(sum-frequency (slurp (io/resource "day1.txt")))
